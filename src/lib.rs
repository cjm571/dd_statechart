/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : lib.rs

Copyright (C) 2020 CJ McAllister
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

Purpose:
    Top-level module defining Data-Driven Harel Statecharts.

    This crate is designed to generally conform to the State Chart XML (SCXML)
    standard established in W3C Recommendation REC-scxml-20150901.
    This standard can be found at: https://www.w3.org/TR/2015/REC-scxml-20150901/

    All submodules in this crate will reference subsections of this standard.

    However, due to the focus on embedded systems, some nonconformances will be
    necessary. Any such nonconformances are listed below:

    1) §3.14: IDs
         “When such an attribute is defined to be optional and the author
          omits it, then, for elements other than <send> and <invoke>, the
          SCXML processor must generate a unique id automatically at
          document load time”
        Omitted IDs shall be treated as errors, and the input SCXML shall be
        rejected.

    2) §3.3.1 [<state>] Attribute Details
        The 'id' field shall be required for all <state> elements.

    3) §3.3.2 [<state>] Children
         “<datamodel> Defines part or all of the data model. Occurs 0 or
          1 times. See 5.2 <datamodel>
          <invoke> Invokes an external service. Occurs 0 or more times.
          See 6.4 <invoke> for 	details.”
        <datamodel> amd <invoke> items shall not be supported.

    4) §3.7 <final>
        Final States shall not be supported. Embedded systems are designed to
        run continuously, and therefore should not enter a state that cannot
        be exited.
    4a) §5.5 <donedata>
        This element shall not be supported, as it is triggered by entering a
        Final State, which is not supported.

    5) §3.2.1 [<scxml>] Attribute Details - `initial` Description
         "The id of the initial state(s) for the document. If not specified, 
          the default initial state is the first child state in document order."
        Multiple initial states shall not be supported.
        Note: I don't think they're actually supported by the standard either...


    OPERATIONAL NOTES
    1) "Document Order" will be the default order of vectors, i.e., the 0th item
       in a vector will be the first item in document order.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
    collections::VecDeque,
    error::Error,
    fmt,
};


///////////////////////////////////////////////////////////////////////////////
//  Module Declarations
///////////////////////////////////////////////////////////////////////////////

pub mod datamodel;
pub mod event;
pub mod executable_content;
pub mod interpreter;
pub mod parser;
pub mod registry;
pub mod state;
pub mod transition;

use crate::{
    datamodel::{
        DataModelError,
        SystemVariables,
    },
    event::Event,
    executable_content::ExecutableContentError,
    interpreter::EcmaScriptValue,
    registry::{
        Registry,
        RegistryError,
    },
    state::{
        State,
        StateError,
        StateId,
    },
    transition::TransitionId,
};



///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Top-level representation of a complete statechart.
///
/// Contains a list of all nodes and events that make up the statechart.
#[derive(PartialEq)]
pub struct StateChart {
    initial:        Option<StateId>,
    internal_queue: VecDeque<Event>,
    registry:       Registry,
    sys_vars:       SystemVariables,
}

pub type StateChartId = String;

#[derive(Debug, PartialEq)]
pub enum StateChartError {
    ReceivedUnregisteredEvent(Event),

    // Wrappers
    ExecutableContentError(ExecutableContentError),
    StateError(StateError),
}

#[derive(Debug, Default, PartialEq)]
pub struct StateChartBuilder {
    initial:    Option<StateId>,
    registry:   Registry,
    sys_vars:   SystemVariables,
}

#[derive(Debug, PartialEq)]
pub enum StateChartBuilderError {
    InitialStateNotRegistered(StateId),
    NoStatesRegistered,

    // Wrappers
    DataModelError(DataModelError),
    RegistryError(RegistryError),
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl StateChart {

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    /// Retrieves all active states in the StateChart.
    pub fn active_state_ids(&self) -> Vec<StateId> {
        self.registry.get_active_state_ids()
    }


    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */

    pub fn process_external_event(&mut self, event: &Event) -> Result<(), StateChartError> {
        // Ensure the given Event is registered
        if !self.registry.event_is_registered(event.clone()) {
            return Err(StateChartError::ReceivedUnregisteredEvent(event.clone()));
        }

        // Set the _event System Variable to the given Event
        self.sys_vars.set_event(event.clone());
        
        // Collect and process the set of enabled Transitions
        let mut enabled_transition_ids = self.select_transitions(Some(event.clone()))?;

        //OPT: *STYLE* I don't like this loop, it's not very easy to tell what's being processed in the microstep
        // Enter Microstep processing loop
        while !enabled_transition_ids.is_empty() {
            self.process_microstep(enabled_transition_ids)?;
            
            // Select eventless Transitions
            enabled_transition_ids = self.select_transitions(None)?;
            
            // Found eventless Transitions, return to top of loop to process a Microstep
            if !enabled_transition_ids.is_empty() {
                continue;
            }
            // No eventless Transitions found, check the internal queue for Events
            else if self.internal_queue.is_empty() {
                break; // Nothing left to process, job done!
            }
            else {
                // Pop the event off the queue
                let internal_event = self.internal_queue.pop_front().unwrap();

                // Set the _event System Variable to the internal Event
                self.sys_vars.set_event(internal_event.clone());

                // Select transitions for the internal Event and return to top of the loop
                enabled_transition_ids = self.select_transitions(Some(internal_event))?;
                continue;
            }
        }

        Ok(())
    }


    /*  *  *  *  *  *  *  *\
     *  Helper Methods    *
    \*  *  *  *  *  *  *  */

    fn select_transitions(&self, event: Option<Event>) -> Result<Vec<TransitionId>, StateChartError> {
        let mut enabled_transitions = Vec::new();

        // Traverse the map of states and send the event to each for evaluation
        for state_id in self.registry.get_active_state_ids() {
            let state = self.registry.get_state(state_id).unwrap();

            if let Some(enabled_transition) = state.evaluate_event(event.clone(), &self.sys_vars)? {
                enabled_transitions.push(enabled_transition);
            }
        }

        Ok(enabled_transitions)
    }

    /// Processes a single set of Enabled Transitions.
    ///
    /// Per definition of the `microstep()` procedure in Appendix D of the standard,
    /// processing a microstep involves 3 phases:
    /// 1. Exiting source State(s)
    /// 2. Executing executable content of a Transition
    /// 3. Entering target State(s)
    fn process_microstep(&mut self, enabled_transition_ids: Vec<TransitionId>) -> Result<(), StateChartError> {
        //TODO: Sort transition source states into exit order, for now, just copying as-is
        let exit_sorted_transition_ids = enabled_transition_ids.clone();

        // Exit source State(s) in "Exit Order"
        for transition_id in exit_sorted_transition_ids {
            // Only operate on Transitions with targets
            //TODO: Possible extraneous clone
            //FIXME: awful style
            if !self.registry.get_transition(transition_id.clone()).unwrap().target_ids().is_empty() {
                let source_state_id = self.registry.get_transition(transition_id).unwrap().source_id();
                let source_state = self.registry.get_mut_state(source_state_id).unwrap();

                source_state.exit()?;
            }
        }

        //TODO: Perform executable content of Transition(s)
        //FIXME: Fix this clone hideousness
        for transition_id in enabled_transition_ids.clone() {
            if let Some(cur_transition) = self.registry.get_transition(transition_id) {
                for exec_content in cur_transition.executable_content() {
                    exec_content.execute(&mut self.sys_vars)?;
                }
            }
            else {
                //FIXME: HANDLE ERROR OR SOMETHING
            }
        }
        
        //TODO: Sort transition source states into entry order, for now, just copying as-is
        let entry_sorted_transition_ids = enabled_transition_ids;

        // Enter target State(s) in "Entry Order"
        for transition_id in entry_sorted_transition_ids {
            let target_state_ids = self.registry.get_transition(transition_id).unwrap().target_ids();
            //TODO: Sort these too?
            for state_id in target_state_ids {
                let target_state = self.registry.get_mut_state(state_id).unwrap();
                target_state.enter()?;
            }
        }

        Ok(())
    }
}


impl StateChartBuilder {

    /*  *  *  *  *  *  *  *\
     *  Builder Methods   *
    \*  *  *  *  *  *  *  */
    
    pub fn build(&mut self) -> Result<StateChart, StateChartBuilderError> {
        // Ensure at least one State has been registered
        if self.registry.get_all_state_ids().is_empty() {
            return Err(StateChartBuilderError::NoStatesRegistered);
        }
        
        // If no initial State ID was provided, set to first doc-order child
        if self.initial.is_none() {
            self.initial = Some(self.registry.get_all_state_ids().first().unwrap().clone());
        }

        // Activate the Initial State
        //TODO: Initial cannot be copied due to StateId being a String. should be made into a &str
        if let Some(state) = self.registry.get_mut_state(self.initial.clone().unwrap()) {
            state.activate()
            //TODO: Probably gotta recurse here for sub-states
            //      Or not? maybe the activate function could do that...
        }
        else {
            // Initial State was not found in the Registry, therefore the StateChart is invalid.
            //TODO: must also be a top-level state
            return Err(StateChartBuilderError::InitialStateNotRegistered(self.initial.clone().unwrap()));
        }
        
        Ok(
            StateChart {
                sys_vars:       self.sys_vars.clone(),
                initial:        self.initial.clone(),
                registry:       self.registry.clone(),
                internal_queue: VecDeque::new(),
            }
        )
    }

    pub fn name(&mut self, name: &str) -> &mut Self {
        self.sys_vars.set_name(String::from(name));

        self
    }

    pub fn state(&mut self, state: State) -> Result<&mut Self, StateChartBuilderError> {        
        // Register the State
        self.registry.register_state(state)?;

        Ok(self)
    }

    pub fn initial(&mut self, initial: StateId) -> &mut Self {
        self.initial = Some(initial);

        self
    }

    pub fn data_member(&mut self, id: String, value: EcmaScriptValue) -> Result<&mut Self, StateChartBuilderError> {
        self.sys_vars.set_data_member(id, value)?;

        Ok(self)
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *     StateChart     *
\*  *  *  *  *  *  *  */

impl fmt::Debug for StateChart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("StateChart")
            .field("sys_vars",          &self.sys_vars)
            .field("initial",           &self.initial)
            .field("registry",          &self.registry)
            .field("internal_queue",    &self.internal_queue)
            .finish()
    }
}


/*  *  *  *  *  *  *  *\
 *  StateChartError   *
\*  *  *  *  *  *  *  */

impl Error for StateChartError {}

impl fmt::Display for StateChartError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ReceivedUnregisteredEvent(event) => {
                write!(f, "Received Event '{}', which is unregistered", event)
            },

            // Wrappers
            Self::ExecutableContentError(exec_err) => {
                write!(f, "ExecutableContentError '{}' encountered while building state chart", exec_err)
            },
            Self::StateError(state_err) => {
                write!(f, "{}", state_err)
            },
        }
    }
}

impl From<StateError> for StateChartError {
    fn from (src: StateError) -> Self {
        Self::StateError(src)
    }
}
impl From<ExecutableContentError> for StateChartError {
    fn from (src: ExecutableContentError) -> Self {
        Self::ExecutableContentError(src)
    }
}


/*  *  *  *  *  *  *  *  *  *\
 *  StateChartBuilderError  *
\*  *  *  *  *  *  *  *  *  */

impl Error for StateChartBuilderError {}

impl fmt::Display for StateChartBuilderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InitialStateNotRegistered(state_id) => {
                write!(f, "ID '{}' is in the `initial` vector, but is not registered", state_id)
            },
            Self::NoStatesRegistered => {
                write!(f, "No States registered")
            },

            // Wrappers
            Self::DataModelError(data_err) => {
                write!(f, "DataModelError '{}' encountered while building state chart", data_err)
            },
            Self::RegistryError(reg_err) => {
                write!(f, "RegistryError '{}' encountered while building state chart", reg_err)
            },
        }
    }
}

impl From<DataModelError> for StateChartBuilderError {
    fn from(src: DataModelError) -> Self {
        Self::DataModelError(src)
    }
}
impl From<RegistryError> for StateChartBuilderError {
    fn from(src: RegistryError) -> Self {
        Self::RegistryError(src)
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use std::error::Error;

    use crate::{
        StateChartBuilder,
        StateChartBuilderError,
        StateChartError,
        event::Event,
        registry::RegistryError,
        state::StateBuilder,
        transition::TransitionBuilder,
    };

    #[test]
    fn theia() -> Result<(), Box<dyn Error>>  {
        //TODO: Extraneous clones
        // Define Event and State IDs for reference
        let go_to_non_imaging   = Event::from("go_to_non_imaging")?;
        let idle_id             = String::from("IDLE");
        let diagnostic_id       = String::from("DIAGNOSTIC");
        let non_imaging_id      = String::from("NON-IMAGING");
        let imaging_standby_id  = String::from("IMAGING STANDBY");
        let imaging_id          = String::from("IMAGING");

        // Build Transitions
        let idle_to_non_imaging = TransitionBuilder::new(idle_id.clone())
            .event(go_to_non_imaging.clone())?
            .target_id(non_imaging_id.clone())?
            .build();

        // Build States
        let idle            = StateBuilder::new(idle_id.clone())
            .transition(idle_to_non_imaging)?
            .build()?;

        let diagnostic      = StateBuilder::new(diagnostic_id).build()?;
        let non_imaging     = StateBuilder::new(non_imaging_id.clone()).build()?;
        let imaging_standby = StateBuilder::new(imaging_standby_id).build()?;
        let imaging         = StateBuilder::new(imaging_id).build()?;

        // Build statechart
        let mut statechart = StateChartBuilder::default()
            .name("theia")
            .initial(idle.id())
            .state(idle)?
            .state(diagnostic)?
            .state(non_imaging)?
            .state(imaging_standby)?
            .state(imaging)?
            .build()?;

        // Broadcast a Go To Non-Imaging event
        statechart.process_external_event(&go_to_non_imaging)?;

        // Verify that IDLE is inactive and NON-IMAGING is active
        assert_eq!(statechart.active_state_ids().contains(&idle_id), false);
        assert_eq!(statechart.active_state_ids().contains(&non_imaging_id), true);

        Ok(())
    }

    #[test]
    fn duplicate_state_id() -> Result<(), Box<dyn Error>> {
        //TODO: Extraneous clones
        // Define states with duplicate IDs
        let duplicate_id = String::from("duplicate");
        let duplicate_a = StateBuilder::new(duplicate_id.clone()).build()?;
        let duplicate_b = StateBuilder::new(duplicate_id.clone()).build()?;

        // Create the statechart object and add states to it
        let mut statechart_builder = StateChartBuilder::default();
        statechart_builder.state(duplicate_a)?;

        // Verify that adding the duplicate ID results in an error
        assert_eq!(
            statechart_builder.state(duplicate_b),
            Err(StateChartBuilderError::RegistryError(RegistryError::StateAlreadyRegistered(duplicate_id))),
            "Failed to detect duplicate State ID error."
        );

        Ok(())
    }

    #[test]
    fn unregistered_event() -> Result<(), Box<dyn Error>>  {
        // Create an event but don't register it
        let unregistered_event = Event::from("unregistered")?;

        let state = StateBuilder::new(String::from("state")).build()?;

        let mut statechart = StateChartBuilder::default()
            .state(state)?
            .build()?;

        // Verify the unregistered event is rejected
        assert_eq!(
            statechart.process_external_event(&unregistered_event),
            Err(StateChartError::ReceivedUnregisteredEvent(unregistered_event)),
            "Failed to reject an unregistered event"
        );

        Ok(())
    }

    #[test]
    fn eventless_transition() -> Result<(), Box<dyn Error>> {
        //TODO: Extraneous clones
        let start_id = String::from("start");
        let end_id = String::from("end");

        // Create an event for the eventful Transition
        let event_id = "test.event";
        let event = Event::from(event_id)?;

        let eventful = TransitionBuilder::new(start_id.clone())
            .event(event.clone())?
            .build();

        // Create the eventless Transition
        let eventless = TransitionBuilder::new(start_id.clone())
            .target_id(end_id.clone())?
            .build();

        // Create a State using the 2 Transitions
        let start = StateBuilder::new(start_id)
            .transition(eventful)?
            .transition(eventless)?
            .build()?;
        
        // Create target State
        let end = StateBuilder::new(end_id.clone()).build()?;

        // Create a StateChart containing all of the above
        let mut statechart = StateChartBuilder::default()
            .state(start)?
            .state(end)?
            .build()?;
        
        // Send the test Event to the statechart, and verify that the eventless transition is also executed
        statechart.process_external_event(&event)?;

        assert_eq!(
            statechart.active_state_ids().first().unwrap(),
            &end_id,
            "Failed to trigger eventless transition."
        );

        Ok(())
    }
}

#[cfg(test)]
mod builder_tests {

    use std::error::Error;

    use crate::{
        StateChartBuilder,
        StateChartBuilderError,
        state::StateBuilder,
    };

    #[test]
    fn initial_state_not_registered() -> Result<(), Box<dyn Error>> {
        // Create states, one will be registered the other will not
        let unregistered = StateBuilder::new(String::from("unregistered")).build()?;
        let registered = StateBuilder::new(String::from("registered")).build()?;

        // Attempt to build a StateChart with an unregistered initial ID
        let mut invalid_builder = StateChartBuilder::default();
        invalid_builder
            .state(registered)?
            .initial(unregistered.id());

        assert_eq!(
            invalid_builder.build(),
            Err(StateChartBuilderError::InitialStateNotRegistered(unregistered.id())),
            "Failed to detect unregistered ID in the initial vector"
        );

        Ok(())
    }

    #[test]
    fn no_states_registered() -> Result<(), Box<dyn Error>> {
        
        assert_eq!(
            StateChartBuilder::default().build(),
            Err(StateChartBuilderError::NoStatesRegistered),
            "Failed to catch that no states were registered"
        );

        Ok(())
    }
}
