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

use std::io::Write;
use std::{collections::VecDeque, error::Error, fmt};


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
    datamodel::{DataModelError, SystemVariables},
    event::Event,
    executable_content::ExecutableContentError,
    interpreter::EcmaScriptValue,
    parser::{Parser, ParserError},
    registry::{Registry, RegistryError},
    state::{State, StateError, StateId},
    transition::TransitionFingerprint,
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Top-level representation of a complete statechart.
///
/// Contains a list of all nodes and events that make up the statechart.
#[derive(PartialEq)]
pub struct StateChart<'w, W: 'w + Write> {
    initial: Option<StateId>,
    internal_queue: VecDeque<Event>,
    registry: Registry,
    sys_vars: SystemVariables,
    writer: &'w mut W,
}

pub type StateChartId = String;

#[derive(Debug, PartialEq)]
pub enum StateChartError {
    ReceivedUnregisteredEvent(Event),

    // Wrappers
    ExecutableContentError(ExecutableContentError),
    ParserError(ParserError),
    RegistryError(RegistryError),
    StateError(StateError),
}

#[derive(Debug, PartialEq)]
pub struct StateChartBuilder<'w, W: 'w + Write> {
    initial: Option<StateId>,
    registry: Registry,
    sys_vars: SystemVariables,
    writer: &'w mut W,
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

impl<'w, W: 'w + Write> StateChart<'w, W> {
    pub fn from(path: &str, writer: &'w mut W) -> Result<Self, StateChartError> {
        // Parse the SCXML doc at the given path
        Parser::new(path, writer)
            .map_err(StateChartError::ParserError)?
            .parse()
            .map_err(StateChartError::ParserError)
    }


    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    /// Retrieves the IDs of all active states in the StateChart.
    pub fn active_state_ids(&self) -> Vec<&str> {
        self.registry
            .get_active_states()
            .iter()
            .map(|v| v.id())
            .collect()
    }


    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */

    pub fn process_external_event(&mut self, event: &Event) -> Result<(), StateChartError> {
        // Ensure the given Event is registered
        if !self.registry.event_is_registered(event) {
            return Err(StateChartError::ReceivedUnregisteredEvent(event.clone()));
        }

        // Set the _event System Variable to the given Event
        self.sys_vars.set_event(event.clone());

        // Collect and process the set of enabled Transitions
        let mut enabled_transition_fingerprints = self.select_transitions(Some(event))?;

        //OPT: *STYLE* I don't like this loop, it's not very easy to tell what's being processed in the microstep
        // Enter Microstep processing loop
        while !enabled_transition_fingerprints.is_empty() {
            self.process_microstep(enabled_transition_fingerprints)?;

            // Select eventless Transitions
            enabled_transition_fingerprints = self.select_transitions(None)?;

            // Found eventless Transitions, return to top of loop to process a Microstep
            if !enabled_transition_fingerprints.is_empty() {
                continue;
            }

            // No eventless Transitions found, check the internal queue for Events
            if let Some(internal_event) = self.internal_queue.pop_front() {
                // Set the _event System Variable to the internal Event
                self.sys_vars.set_event(internal_event.clone());

                // Select transitions for the internal Event and return to top of the loop
                enabled_transition_fingerprints = self.select_transitions(Some(&internal_event))?;
                continue;
            } else {
                break; // Nothing left to process, job done!
            }
        }

        Ok(())
    }


    /*  *  *  *  *  *  *  *\
     *  Helper Methods    *
    \*  *  *  *  *  *  *  */

    fn select_transitions(
        &self,
        event: Option<&Event>,
    ) -> Result<Vec<TransitionFingerprint>, StateChartError> {
        let mut enabled_transitions = Vec::new();

        // Traverse the map of states and send the event to each for evaluation
        for state in &self.registry.get_active_states() {
            if let Some(enabled_transition) = state.evaluate_event(event, &self.sys_vars)? {
                // These transitions will be used later by &mut self methods, so we must clone() in order
                // to give them owned TransitionFingerprints
                enabled_transitions.push(enabled_transition.clone());
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
    fn process_microstep(
        &mut self,
        enabled_transition_fingerprints: Vec<TransitionFingerprint>,
    ) -> Result<(), StateChartError> {
        // Transitions are selected in Document Order, so reverse this Vec to get Exit Order
        let mut exit_sorted_transition_fingerprints = enabled_transition_fingerprints.clone();
        exit_sorted_transition_fingerprints.reverse();

        // Exit source State(s) in Exit Order
        for transition_fingerprint in &exit_sorted_transition_fingerprints {
            // Only operate on Transitions with targets here
            if !self
                .registry
                .get_transition(transition_fingerprint)?
                .target_ids()
                .is_empty()
            {
                // Clone is necessary as the ID will be used by mutable methods next
                let source_state_id = self
                    .registry
                    .get_transition(transition_fingerprint)?
                    .source_id()
                    .clone();
                let source_state = self.registry.get_mut_state(&source_state_id)?;

                source_state.exit()?;
            }
        }

        // Perform executable content of Transition(s) in Document Order
        for transition_fingerprint in &enabled_transition_fingerprints {
            let cur_transition = self.registry.get_transition(transition_fingerprint)?;
            for exec_content in cur_transition.executable_content() {
                exec_content.execute(&mut self.sys_vars, &mut self.writer)?;
            }
        }

        // Enter target State(s) in Entry Order (same as Document Order)
        for transition_fingerprint in &enabled_transition_fingerprints {
            // Clone is necessary as the IDs will be used by mutable methods next
            let target_state_ids = self
                .registry
                .get_transition(transition_fingerprint)?
                .target_ids()
                .clone();
            for state_id in &target_state_ids {
                let target_state = self.registry.get_mut_state(state_id)?;
                target_state.enter()?;
            }
        }

        Ok(())
    }
}


impl<'w, W: 'w + Write> StateChartBuilder<'w, W> {
    pub fn new(writer: &'w mut W) -> Self {
        Self {
            initial: Option::default(),
            registry: Registry::default(),
            sys_vars: SystemVariables::default(),
            writer,
        }
    }

    /*  *  *  *  *  *  *  *\
     *  Builder Methods   *
    \*  *  *  *  *  *  *  */

    pub fn build(mut self) -> Result<StateChart<'w, W>, StateChartBuilderError> {
        // Ensure at least one State has been registered
        if let Some(first_state) = self.registry.get_all_states().first() {
            // If no initial State ID was provided, set to first doc-order child
            if self.initial.is_none() {
                self.initial = Some(first_state.id().to_string());
            }
        } else {
            return Err(StateChartBuilderError::NoStatesRegistered);
        }

        // Activate the Initial State
        if let Some(initial_id) = &self.initial {
            if let Ok(state) = self.registry.get_mut_state(initial_id.as_ref()) {
                state.activate()
            } else {
                // Initial State was not found in the Registry, therefore the StateChart is invalid.
                return Err(StateChartBuilderError::InitialStateNotRegistered(
                    initial_id.clone(),
                ));
            }
        } else {
            // Intentionally left blank
            // self.initial is guaranteed to contain a value since it is set above
        }

        Ok(StateChart {
            sys_vars: self.sys_vars,
            initial: self.initial,
            registry: self.registry,
            internal_queue: VecDeque::new(),
            writer: self.writer,
        })
    }

    pub fn name(mut self, name: &str) -> Self {
        self.sys_vars.set_name(String::from(name));

        self
    }

    pub fn state(mut self, state: State) -> Result<Self, StateChartBuilderError> {
        // Register the State
        self.registry.register_state(state)?;

        Ok(self)
    }

    pub fn initial(mut self, initial: StateId) -> Self {
        self.initial = Some(initial);

        self
    }

    pub fn data_member(mut self, id: &str, value: EcmaScriptValue) -> Self {
        self.sys_vars.set_data_member(id, value);

        self
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *     StateChart     *
\*  *  *  *  *  *  *  */

impl<'w, W: 'w + Write> fmt::Debug for StateChart<'w, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("StateChart")
            .field("sys_vars", &self.sys_vars)
            .field("initial", &self.initial)
            .field("registry", &self.registry)
            .field("internal_queue", &self.internal_queue)
            .field("writer", &String::from(std::any::type_name::<W>()))
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

            // Wrappers
            Self::ExecutableContentError(exec_err) => {
                write!(
                    f,
                    "ExecutableContentError '{:?}' encountered while processing state chart",
                    exec_err
                )
            }
            Self::ParserError(parse_err) => {
                write!(
                    f,
                    "ParserError '{:?}' encountered while processing state chart",
                    parse_err
                )
            }
            Self::RegistryError(registry_err) => {
                write!(
                    f,
                    "RegistryError '{:?}' encountered while processing state chart",
                    registry_err
                )
            }
            Self::StateError(state_err) => {
                write!(
                    f,
                    "StateError '{:?}' encountered while processing state chart",
                    state_err
                )
            }
        }
    }
}

impl From<ExecutableContentError> for StateChartError {
    fn from(src: ExecutableContentError) -> Self {
        Self::ExecutableContentError(src)
    }
}
impl From<RegistryError> for StateChartError {
    fn from(src: RegistryError) -> Self {
        Self::RegistryError(src)
    }
}
impl From<StateError> for StateChartError {
    fn from(src: StateError) -> Self {
        Self::StateError(src)
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
                write!(
                    f,
                    "ID '{}' is in the `initial` vector, but is not registered",
                    state_id
                )
            }
            Self::NoStatesRegistered => {
                write!(f, "No States registered")
            }

            // Wrappers
            Self::DataModelError(data_err) => {
                write!(
                    f,
                    "DataModelError '{:?}' encountered while building state chart",
                    data_err
                )
            }
            Self::RegistryError(reg_err) => {
                write!(
                    f,
                    "RegistryError '{:?}' encountered while building state chart",
                    reg_err
                )
            }
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
    use std::{error::Error, fmt};
    use std::io::{self, Read};
    use std::fs::File;

    use crate::{
        event::Event, registry::RegistryError, state::StateBuilder, transition::TransitionBuilder,
        StateChart, StateChartBuilder, StateChartBuilderError, StateChartError,
    };


    type TestResult = Result<(), Box<dyn Error>>;

    #[derive(Debug, PartialEq)]
    struct GenericError {}
    impl Error for GenericError {}
    impl fmt::Display for GenericError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }


    #[test]
    fn theia() -> TestResult {
        // Define Event and State IDs for reference
        let go_to_non_imaging = Event::from("go_to_non_imaging")?;
        let idle_id = String::from("IDLE");
        let diagnostic_id = String::from("DIAGNOSTIC");
        let non_imaging_id = String::from("NON-IMAGING");
        let imaging_standby_id = String::from("IMAGING STANDBY");
        let imaging_id = String::from("IMAGING");

        // Build Transitions
        let idle_to_non_imaging = TransitionBuilder::new(idle_id.as_str())
            .event(&go_to_non_imaging)?
            .target_id(non_imaging_id.as_str())?
            .build()?;

        // Build States
        let idle = StateBuilder::new(idle_id.clone())
            .transition(idle_to_non_imaging)?
            .build()?;

        let diagnostic = StateBuilder::new(diagnostic_id).build()?;
        let non_imaging = StateBuilder::new(non_imaging_id.clone()).build()?;
        let imaging_standby = StateBuilder::new(imaging_standby_id).build()?;
        let imaging = StateBuilder::new(imaging_id).build()?;

        // Build statechart
        let mut dev_null = io::sink();
        let mut statechart = StateChartBuilder::new(&mut dev_null)
            .name("theia")
            .initial(idle.id().to_string())
            .state(idle)?
            .state(diagnostic)?
            .state(non_imaging)?
            .state(imaging_standby)?
            .state(imaging)?
            .build()?;

        // Broadcast a Go To Non-Imaging event
        statechart.process_external_event(&go_to_non_imaging)?;

        // Verify that IDLE is inactive and NON-IMAGING is active
        assert_eq!(
            statechart.active_state_ids().contains(&idle_id.as_str()),
            false
        );
        assert_eq!(
            statechart
                .active_state_ids()
                .contains(&non_imaging_id.as_str()),
            true
        );

        Ok(())
    }

    #[test]
    fn duplicate_state_id() -> TestResult {
        // Define states with duplicate IDs
        let duplicate_id = String::from("duplicate");
        let duplicate_a = StateBuilder::new(duplicate_id.clone()).build()?;
        let duplicate_b = StateBuilder::new(duplicate_id.clone()).build()?;

        // Create the statechart object and add states to it
        let mut dev_null = io::sink();
        let mut statechart_builder = StateChartBuilder::new(&mut dev_null);
        statechart_builder = statechart_builder.state(duplicate_a)?;

        // Verify that adding the duplicate ID results in an error
        assert_eq!(
            statechart_builder.state(duplicate_b).unwrap_err(),
            StateChartBuilderError::RegistryError(RegistryError::StateAlreadyRegistered(
                duplicate_id
            )),
            "Failed to detect duplicate State ID error."
        );

        Ok(())
    }

    #[test]
    fn unregistered_event() -> TestResult {
        // Create an event but don't register it
        let unregistered_event = Event::from("unregistered")?;

        let state = StateBuilder::new(String::from("state")).build()?;

        let mut dev_null = io::sink();
        let mut statechart = StateChartBuilder::new(&mut dev_null)
            .state(state)?
            .build()?;

        // Verify the unregistered event is rejected
        assert_eq!(
            statechart.process_external_event(&unregistered_event),
            Err(StateChartError::ReceivedUnregisteredEvent(
                unregistered_event
            )),
            "Failed to reject an unregistered event"
        );

        Ok(())
    }

    #[test]
    fn eventless_transition() -> TestResult {
        let start_id = String::from("start");
        let end_id = String::from("end");

        // Create an event for the eventful Transition
        let event_id = "test.event";
        let event = Event::from(event_id)?;

        let eventful = TransitionBuilder::new(start_id.as_str())
            .event(&event)?
            .build()?;

        // Create the eventless Transition
        let eventless = TransitionBuilder::new(start_id.as_str())
            .target_id(end_id.as_str())?
            .build()?;

        // Create a State using the 2 Transitions
        let start = StateBuilder::new(start_id)
            .transition(eventful)?
            .transition(eventless)?
            .build()?;

        // Create target State
        let end = StateBuilder::new(end_id.clone()).build()?;

        // Create a StateChart containing all of the above
        let mut dev_null = io::sink();
        let mut statechart = StateChartBuilder::new(&mut dev_null)
            .state(start)?
            .state(end)?
            .build()?;

        // Send the test Event to the statechart, and verify that the eventless transition is also executed
        statechart.process_external_event(&event)?;

        assert_eq!(
            statechart.active_state_ids().first(),
            Some(&end_id.as_str()),
            "Failed to trigger eventless transition."
        );

        Ok(())
    }

    #[test]
    fn end_to_end_scxml() -> TestResult {
        let mut dev_null = io::sink();
        // Parse a StateChart from the microwave SCXML sample
        let mut statechart =
            StateChart::<io::Sink>::from("res/examples/01_microwave.scxml", &mut dev_null)?;

        // Create events to be sent (already registered by parsing process)
        let turn_on = Event::from("turn.on")?;
        let door_open = Event::from("door.open")?;
        let door_close = Event::from("door.close")?;
        let time = Event::from("time")?;

        // Process the events
        statechart.process_external_event(&turn_on)?;
        statechart.process_external_event(&door_open)?;
        statechart.process_external_event(&door_close)?;
        for _ in 0..5 {
            statechart.process_external_event(&time)?;
        }

        eprintln!("*** Active State(s):\n{:#?}", statechart.active_state_ids());
        assert_eq!(statechart.active_state_ids(), vec!["off".to_string()]);

        Ok(())
    }

    #[test]
    fn logging_microwave() -> TestResult {
        // Create a buffer for log verification
        let mut buffer = Vec::new();

        // Parse a StateChart from the modified microwave SCXML example to include some contrived logging
        let mut statechart =
            StateChart::<Vec<u8>>::from("res/test_cases/logging_microwave.scxml", &mut buffer)?;

        // Create events to be sent (already registered by parsing process)
        let turn_on = Event::from("turn.on")?;
        let door_open = Event::from("door.open")?;
        let door_close = Event::from("door.close")?;
        let time = Event::from("time")?;

        // Process the events
        statechart.process_external_event(&turn_on)?;
        statechart.process_external_event(&door_open)?;
        statechart.process_external_event(&door_close)?;
        for _ in 0..5 {
            statechart.process_external_event(&time)?;
        }

        // Read in verification file and compare to output
        let mut file = File::open("res/verification/logging_microwave.log")?;
        let mut verf_buffer = Vec::new();
        file.read_to_end(&mut verf_buffer)?;

        assert_eq!(
            String::from_utf8(buffer),
            String::from_utf8(verf_buffer)
        );

        Ok(())
    }
}


#[cfg(test)]
mod builder_tests {

    use std::{error::Error, io};

    use crate::{state::StateBuilder, StateChartBuilder, StateChartBuilderError};


    type TestResult = Result<(), Box<dyn Error>>;


    #[test]
    fn initial_state_not_registered() -> TestResult {
        // Create states, one will be registered the other will not
        let unregistered = StateBuilder::new(String::from("unregistered")).build()?;
        let registered = StateBuilder::new(String::from("registered")).build()?;

        // Attempt to build a StateChart with an unregistered initial ID
        let mut dev_null = io::sink();
        let mut invalid_builder = StateChartBuilder::new(&mut dev_null);
        invalid_builder = invalid_builder
            .state(registered)?
            .initial(unregistered.id().to_string());

        assert_eq!(
            invalid_builder.build().unwrap_err(),
            StateChartBuilderError::InitialStateNotRegistered(unregistered.id().to_string()),
            "Failed to detect unregistered ID in the initial vector"
        );

        Ok(())
    }

    #[test]
    fn no_states_registered() -> TestResult {
        assert_eq!(
            StateChartBuilder::new(&mut io::sink()).build().unwrap_err(),
            StateChartBuilderError::NoStatesRegistered,
            "Failed to catch that no states were registered"
        );

        Ok(())
    }
}
