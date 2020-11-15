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

    This crate is designed to conform to the State Chart XML (SCXML) standard
    established in W3C Recommendation REC-scxml-20150901.
    This standard can be found at: https://www.w3.org/TR/2015/REC-scxml-20150901/

    All submodules in this crate will reference subsections of this standard.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

//TODO: Nested states


///////////////////////////////////////////////////////////////////////////////
//  Module Declarations
///////////////////////////////////////////////////////////////////////////////

pub mod registry;
pub mod event;
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

//TODO: Probably expand this to a more detailed struct
pub type StateChartId = &'static str;

/// Top-level representation of a complete statechart.
///
/// Contains a list of all nodes and events that make up the statechart.
pub struct StateChart {
    id:         StateChartId,
    initial:    Vec<StateId>,
    registry:   Registry,
}

#[derive(Debug, PartialEq)]
pub enum StateChartError {
    AlreadyExists,
    DuplicateStateId(StateId),
    Other,
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl StateChart {
    /// Creates a StateChart object with the given ID.
    pub fn new(id: StateChartId) -> Self {
        Self {
            id,
            initial:    Vec::new(),
            registry:   Registry::default(),
        }
    }

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    /// Retrieves all active states in the StateChart.
    pub fn active_state_ids(&self) -> Vec<StateId> {
        self.registry.get_active_state_ids()
    }


    //TODO: replace add_* with Builder pattern
    /*  *  *  *  *  *  *  *\
     *  Mutator Methods   *
    \*  *  *  *  *  *  *  */
    pub fn add_state(&mut self, mut state: State, is_initial: bool) -> Result<(), RegistryError> {
        //OPT: *DESIGN* Should pushing to `initial` be done before registration, which can fail?
        // If necessary, add the ID to the initially-active list and activate
        if is_initial {
            self.initial.push(state.id());
            state.activate();
        }

        // Register the state
        self.registry.register_state(state)?;

        Ok(())
    }


    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */
    pub fn process_external_event(&mut self, event: &Event) -> Result<(), StateError> {
        // Collect and process the set of enabled Transitions
        let enabled_transition_ids = self.select_transitions(event)?;

        // Perform microstep processing for the current Event
        self.process_microstep(enabled_transition_ids)
    }


    /*  *  *  *  *  *  *  *\
     *  Helper Methods    *
    \*  *  *  *  *  *  *  */    
    /// Collects the ID(s) of Transition(s) enabled by the given Event.
    ///
    /// Returns Ok() if the event was valid, StateChartError if the event was rejected.
    ///
    /// TODO: Fix/Finish comment
    pub fn select_transitions(&self, event: &Event) -> Result<Vec<TransitionId>, StateError> {
        //TODO: Sanity-check event?

        let mut enabled_transitions = Vec::new();

        // Traverse the map of states and send the event to each for evaluation
        for state_id in &self.registry.get_active_state_ids() {
            let state = self.registry.get_state(state_id);
            let enabled_transition = state.evaluate_event(event.id())?.unwrap();

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
    fn process_microstep(&mut self, enabled_transition_ids: Vec<TransitionId>) -> Result<(), StateError> {
        //TODO: Sort transition source states into exit order, for now, just copying as-is
        let exit_sorted_transition_ids = enabled_transition_ids.clone();

        // Exit source State(s) in "Exit Order"
        for transition_id in exit_sorted_transition_ids {
            let source_state_id = self.registry.get_transition(transition_id).unwrap().source_id();
            let source_state = self.registry.get_mut_state(source_state_id);

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
                let target_state = self.registry.get_mut_state(state_id);
                target_state.enter()?;
            }
        }

        Ok(())
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

impl std::fmt::Debug for StateChart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StateChart")
            .field("id", &self.id)
            .field("initial", &self.initial)
            .field("registry", &self.registry)
            .finish()
    }
}

///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use crate::{
        StateChart,
        event::Event,
        state::{
            State,
            StateError,
        },
        registry::RegistryError,
        transition::Transition,
    };

    /// Constvenience function for use as a "null conditional"
    const fn always_true() -> bool { true }
    /// Constvenience function for use as an "anti-null conditional"
    const fn always_false() -> bool { false }


    #[test]
    fn failed_condition() {
        // Define states
        let mut initial = State::new("INITIAL");
        let unreachable = State::new("UNREACHABLE");

        // Define events and transitions
        let go_to_unreachable = Event::new("go_to_unreachable");
        let initial_to_unreachable_id = "initial_to_unreachable";
        let initial_to_unreachable = Transition::new(
            initial_to_unreachable_id,
            go_to_unreachable.id(),
            always_false,
            initial.id(),
            vec![unreachable.id()]);
        initial.add_transition(initial_to_unreachable);

        // Create statechart object and add states to it
        let mut hapless_statechart = StateChart::new("hapless");
        hapless_statechart.add_state(initial, true).unwrap();
        hapless_statechart.add_state(unreachable, false).unwrap();

        // Broadcast the event and verify that the transition failed its guard condition
        assert_eq!(
            hapless_statechart.process_external_event(&go_to_unreachable),
            Err(StateError::FailedConditions(vec![initial_to_unreachable_id])),
            "Failed to detect failed Transition due to failed Condition." 
        );
    }

    #[test]
    fn hyperion() {
        // Define state IDs for reference
        let idle_id             = "IDLE";
        let diagnostic_id       = "DIAGNOSTIC";
        let non_imaging_id      = "NON-IMAGING";
        let imaging_standby_id  = "IMAGING STANDBY";
        let imaging_id          = "IMAGING";

        // Define system states
        let mut idle = State::new(idle_id);
        let diagnostic = State::new(diagnostic_id);
        let non_imaging = State::new(non_imaging_id);
        let imaging_standby = State::new(imaging_standby_id);
        let imaging = State::new(imaging_id);

        // Define events and transitions
        let go_to_non_imaging = Event::new("go_to_non_imaging");
        let idle_to_non_imaging = Transition::new(
            "idle_to_non-imaging",
            go_to_non_imaging.id(),
            always_true,
            idle.id(),
            vec![non_imaging.id()]);
        idle.add_transition(idle_to_non_imaging);

        // Create statechart object and add states to it
        let mut hyperion_statechart = StateChart::new("hyperion");
        hyperion_statechart.add_state(idle, true).unwrap();
        hyperion_statechart.add_state(diagnostic, false).unwrap();
        hyperion_statechart.add_state(non_imaging, false).unwrap();
        hyperion_statechart.add_state(imaging_standby, false).unwrap();
        hyperion_statechart.add_state(imaging, false).unwrap();

        //FIXME: DEBUG DELETE
        eprintln!("Active States BEFORE Event: {:?}", hyperion_statechart.active_state_ids());
        eprintln!("{:?}\n", hyperion_statechart);

        // Broadcast a Go To Non-Imaging event
        hyperion_statechart.process_external_event(&go_to_non_imaging).unwrap();
        
        //FIXME: DEBUG DELETE
        eprintln!("Active States AFTER Event: {:?}", hyperion_statechart.active_state_ids());
        eprintln!("{:?}", hyperion_statechart);

        // Verify that IDLE is inactive and NON-IMAGING is active
        assert_eq!(hyperion_statechart.active_state_ids().contains(&idle_id), false);
        assert_eq!(hyperion_statechart.active_state_ids().contains(&non_imaging_id), true);
    }

    #[test]
    fn duplicate_state_id() {
        // Define states with duplicate IDs
        let duplicate_id = "duplicate";
        let duplicate_a = State::new(duplicate_id);
        let duplicate_b = State::new(duplicate_id);

        // Create the statechart object and add states to it
        let mut duplicate_statechart = StateChart::new("duplicate_chart");
        duplicate_statechart.add_state(duplicate_a, true).unwrap();
        
        // Verify that adding the duplicate ID results in an error
        assert_eq!(
            duplicate_statechart.add_state(duplicate_b, false),
            Err(RegistryError::AlreadyExists),
            "Failed to detect duplicate State ID error."
        );
    }
}
