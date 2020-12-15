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

    2) §3.3.2 [<state>] Children
         “<datamodel> Defines part or all of the data model. Occurs 0 or
          1 times. See 5.2 <datamodel>
          <invoke> Invokes an external service. Occurs 0 or more times.
          See 6.4 <invoke> for 	details.”
        <datamodel> amd <invoke> items shall not be supported.
    3) §3.7 <final>
        Final states shall not be supported. Embedded systems are designed to
        run continuously, and therefore should not enter a state that cannot
        be exited.
    4) §3.2.1 [<scxml>] Attribute Details - `initial` Description
        "The id of the initial state(s) for the document. If not specified, 
         the default initial state is the first child state in document order."
        Multiple initial states shall not be supported.
        Note: I don't think they're actually supported by the standard either...


    OPERATIONAL NOTES
    1) "Document Order" will be the default order of vectors, i.e., the 0th item
       in a vector will be the first item in document order.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
    error::Error,
    fmt,
};


///////////////////////////////////////////////////////////////////////////////
//  Module Declarations
///////////////////////////////////////////////////////////////////////////////

pub mod event;
pub mod registry;
pub mod state;
pub mod transition;

use crate::{
    event::Event,
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
    id:         StateChartId,
    initial:    Option<StateId>,
    registry:   Registry,
}

pub type StateChartId = &'static str;

#[derive(Debug, PartialEq)]
pub enum StateChartError {
    ReceivedUnregisteredEvent(Event),
    StateError(StateError),
}

#[derive(Debug, PartialEq)]
pub struct StateChartBuilder {
    id:         StateChartId,
    initial:    Option<StateId>,
    registry:   Registry,
}

#[derive(Debug, PartialEq)]
pub enum StateChartBuilderError {
    InitialStateNotRegistered(StateId),
    NoStatesRegistered,
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

    pub fn process_external_event(&mut self, event: Event) -> Result<(), StateChartError> {
        // Collect and process the set of enabled Transitions
        let enabled_transition_ids = self.select_transitions(event)?;

        // Perform microstep processing for the current Event
        self.process_microstep(enabled_transition_ids)
    }


    /*  *  *  *  *  *  *  *\
     *  Helper Methods    *
    \*  *  *  *  *  *  *  */

    fn select_transitions(&self, event: Event) -> Result<Vec<TransitionId>, StateChartError> {
        if !self.registry.event_is_registered(event) {
            return Err(StateChartError::ReceivedUnregisteredEvent(event));
        }

        let mut enabled_transitions = Vec::new();

        // Traverse the map of states and send the event to each for evaluation
        for state_id in &self.registry.get_active_state_ids() {
            let state = self.registry.get_state(state_id).unwrap();
            let enabled_transition = state.evaluate_event(event)?.unwrap();

            enabled_transitions.push(enabled_transition);
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
            let source_state_id = self.registry.get_transition(transition_id).unwrap().source_id();
            let source_state = self.registry.get_mut_state(source_state_id).unwrap();

            source_state.exit()?;
        }

        //TODO: Perform executable content of Transition(s)
        
        //TODO: Sort transition source states into entry order, for now, just copying as-is
        let entry_sorted_transition_ids = enabled_transition_ids.clone();

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
    pub fn new(id: StateChartId) -> Self {
        Self {
            id,
            initial:  None,
            registry: Registry::default(),
        }
    }


    /*  *  *  *  *  *  *  *\
     *  Builder Methods   *
    \*  *  *  *  *  *  *  */
    
    pub fn build(mut self) -> Result<StateChart, StateChartBuilderError> {
        // Ensure at least one State has been registered
        if self.registry.get_all_state_ids().is_empty() {
            return Err(StateChartBuilderError::NoStatesRegistered);
        }
        
        // If no initial State ID was provided, set to first doc-order child
        if self.initial.is_none() {
            self.initial = Some(self.registry.get_all_state_ids().first().unwrap());
        }

        // Activate the Initial State
        if let Some(state) = self.registry.get_mut_state(self.initial.unwrap()) {
            state.activate()
            //TODO: Probably gotta recurse here for sub-states
            //      Or not? maybe the activate function could do that...
        }
        else {
            // Initial State was not found in the Registry, therefore the StateChart is invalid.
            //TODO: must also be a top-level state
            return Err(StateChartBuilderError::InitialStateNotRegistered(self.initial.unwrap()));
        }
        
        Ok(
            StateChart {
                id:         self.id,
                initial:    self.initial,
                registry:   self.registry,
            }
        )
    }

    pub fn state(mut self, state: State) -> Result<Self, RegistryError>{
        self.registry.register_state(state)?;

        Ok(self)
    }

    pub fn event(mut self, event: Event) -> Result<Self, RegistryError> {
        self.registry.register_event(event)?;

        Ok(self)
    }

    //OPT: *DESIGN* should push a single ID into the vec and check for duplicates
    pub fn initial(mut self, initial: StateId) -> Self {
        self.initial = Some(initial);

        self
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
            .field("id", &self.id)
            .field("initial", &self.initial)
            .field("registry", &self.registry)
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
            }
            Self::StateError(state_err) => {
                write!(f, "{}", state_err)
            }
        }
    }
}

impl From<StateError> for StateChartError {
    fn from (source: StateError) -> Self {
        Self::StateError(source)
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
            }
        }
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
        StateChartError,
        event::Event,
        state::StateBuilder,
        registry::RegistryError,
        transition::TransitionBuilder,
    };

    #[test]
    fn theia() -> Result<(), Box<dyn Error>>  {
        // Define Event and State IDs for reference
        let go_to_non_imaging   = Event::from("go_to_non_imaging")?;
        let idle_id             = "IDLE";
        let diagnostic_id       = "DIAGNOSTIC";
        let non_imaging_id      = "NON-IMAGING";
        let imaging_standby_id  = "IMAGING STANDBY";
        let imaging_id          = "IMAGING";

        // Build Transitions
        let idle_to_non_imaging = TransitionBuilder::new("idle_to_non-imaging", idle_id)
            .event(go_to_non_imaging)?
            .target_id(non_imaging_id)?
            .build();

        // Build States
        let idle            = StateBuilder::new(idle_id)
            .transition(idle_to_non_imaging)?
            .build()?;

        let diagnostic      = StateBuilder::new(diagnostic_id).build()?;
        let non_imaging     = StateBuilder::new(non_imaging_id).build()?;
        let imaging_standby = StateBuilder::new(imaging_standby_id).build()?;
        let imaging         = StateBuilder::new(imaging_id).build()?;

        // Build statechart
        let mut statechart = StateChartBuilder::new("theia")
            .initial(idle.id())
            .state(idle)?
            .state(diagnostic)?
            .state(non_imaging)?
            .state(imaging_standby)?
            .state(imaging)?
            .event(go_to_non_imaging)?
            .build().unwrap();

        // Broadcast a Go To Non-Imaging event
        statechart.process_external_event(go_to_non_imaging)?;

        // Verify that IDLE is inactive and NON-IMAGING is active
        assert_eq!(statechart.active_state_ids().contains(&idle_id), false);
        assert_eq!(statechart.active_state_ids().contains(&non_imaging_id), true);

        Ok(())
    }

    #[test]
    fn duplicate_state_id() -> Result<(), Box<dyn Error>>  {
        // Define states with duplicate IDs
        let duplicate_id = "duplicate";
        let duplicate_a = StateBuilder::new(duplicate_id).build()?;
        let duplicate_b = StateBuilder::new(duplicate_id).build()?;

        // Create the statechart object and add states to it
        let statechart_builder = StateChartBuilder::new("duplicate_chart")
            .state(duplicate_a)?;

        // Verify that adding the duplicate ID results in an error
        assert_eq!(
            statechart_builder.state(duplicate_b),
            Err(RegistryError::StateAlreadyRegistered(duplicate_id)),
            "Failed to detect duplicate State ID error."
        );

        Ok(())
    }

    #[test]
    fn unregistered_event() -> Result<(), Box<dyn Error>>  {
        // Create an event but don't register it
        let unregistered_event = Event::from("unregistered")?;

        let state = StateBuilder::new("state").build()?;

        let mut statechart = StateChartBuilder::new("statechart")
            .state(state)?
            .build()?;

        // Verify the unregistered event is rejected
        assert_eq!(
            statechart.process_external_event(unregistered_event),
            Err(StateChartError::ReceivedUnregisteredEvent(unregistered_event)),
            "Failed to reject an unregistered event"
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
        let unregistered = StateBuilder::new("unregistered").build()?;
        let registered = StateBuilder::new("registered").build()?;

        // Attempt to build a StateChart with an unregistered initial ID
        let invalid_builder = StateChartBuilder::new("invalid")
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
            StateChartBuilder::new("no_states").build(),
            Err(StateChartBuilderError::NoStatesRegistered),
            "Failed to catch that no states were registered"
        );

        Ok(())
    }
}
