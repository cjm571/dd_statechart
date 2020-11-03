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

use std::collections::HashMap;


///////////////////////////////////////////////////////////////////////////////
//  Module Declarations
///////////////////////////////////////////////////////////////////////////////

pub mod event;
pub mod state;
pub mod transition;

use event::Event;
use state::{
    State,
    StateError,
    StateId,
};



///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Top-level representation of a complete statechart.
///
/// Contains a list of all nodes and events that make up the statechart.
#[derive(Default)]
pub struct StateChart {
    id:         String,
    initial:    Vec<StateId>,
    states:     HashMap<StateId, State>,
}


#[derive(Debug)]
pub enum StateChartError {
    AlreadyExists,
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl StateChart {
    /// Creates a StateChart object with the given parameters.
    pub fn new(id: String, initial: Vec<StateId>, states: HashMap<StateId, State>) -> Self {
        Self {
            id,
            initial,
            states,
        }
    }

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    /// Retrieves all active states in the StateChart.
    pub fn active_state_ids(&self) -> Vec<StateId> {
        let mut active_states: Vec<StateId> = Vec::new();

        // Traverse the map of states and push all active states into a vector
        for state in self.states.values() {
            if state.is_active() {
                active_states.push(state.id());
            }
        }

        active_states
    }


    //TODO: replace add_* with Builder pattern
    /*  *  *  *  *  *  *  *\
     *  Mutator Methods   *
    \*  *  *  *  *  *  *  */
    pub fn add_state(&mut self, mut state: State, is_initial: bool) {
        //TODO: Need to sanity-check is_initial so that nonparallel states are not marked as initially active

        // If necessary, add the ID to the initially-active list and activate
        if is_initial {
            self.initial.push(state.id());
            state.activate();
        }

        // Add new state to the map
        self.states.insert(state.id(), state);
    }


    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */
    /// Broadcasts an event to all states of the StateChart.
    ///
    /// Returns Ok() if the event was valid, StateChartError if the event was rejected.
    pub fn broadcast_event(&mut self, event: &Event) -> Result<HashMap<StateId, Vec<StateId>>, StateError> {
        //TODO: Sanity-check event?

        // Traverse the map of states and send the event to each for evaluation
        let mut state_changes: HashMap<StateId, Vec<StateId>> = HashMap::new();
        for source_state in self.states.values() {
            match source_state.evaluate_event(event.id()) {
                Ok(target_ids) => {
                    if !target_ids.is_empty() {
                        // Transition Enabled, record the source and target State(s) for de/activation
                        state_changes.insert(source_state.id(), target_ids);
                    }
                },
                Err(StateError::FailedConditions(transition_ids)) => {
                    // Transition(s) failed, short-circuit before we alter the Configuration
                    return Err(StateError::FailedConditions(transition_ids));
                }
            }
        }

        // Traverse state change map to de/activate source and target States
        for (source_state_id, target_state_ids) in &state_changes {
            self.get_mut_state_from_id(source_state_id).deactivate();

            for state_id in target_state_ids {
                self.get_mut_state_from_id(state_id).activate();
            }
        }        

        Ok(state_changes)
    }


    /*  *  *  *  *  *  *  *\
     *  Helper Methods    *
    \*  *  *  *  *  *  *  */
    fn get_mut_state_from_id(&mut self, id: StateId) -> &mut State {
        self.states.get_mut(&id).unwrap()
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
            .field("states", &self.states)
            .finish()
    }
}

///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use crate::{
        event::Event,
        state::{
            State,
            StateError,
        },
        StateChart,
        transition::Transition,
    };

    /// Constvenience variable for empty statechart comparison.
    const EMPTY_STATECHART_PRINT: &str = "StateChart { id: \"\", initial: [], states: {} }";

    /// Constvenience function for use as a "null conditional"
    const fn always_true() -> bool { true }
    /// Constvenience function for use as an "anti-null conditional"
    const fn always_false() -> bool { false }

    #[test]
    fn empty_statechart() {
        let statechart = StateChart::default();

        assert_eq!(EMPTY_STATECHART_PRINT, format!("{:?}", statechart));
    }

    #[test]
    fn failed_condition() {
        // Define states
        let mut initial = State::new("INITIAL");
        let unreachable = State::new("UNREACHABLE");

        // Define events and transitions
        let go_to_unreachable = Event::new("go_to_unreachable");
        let initial_to_unreachable = Transition::new(
            "initial_to_unreachable",
            go_to_unreachable.id(),
            always_false,
            vec![unreachable.id()]);
        initial.add_transition(initial_to_unreachable);

        // Create statechart object and add states to it
        let mut hapless_statechart = StateChart::default();
        hapless_statechart.add_state(initial, true);
        hapless_statechart.add_state(unreachable, false);

        // Broadcast the event and verify that the transition failed its guard condition
        match hapless_statechart.broadcast_event(&go_to_unreachable) {
            Ok(ids) => {
                eprintln!("Error: Unexpected successful event evaluation.");
                eprintln!("Successful Change Map: {:?}", ids);
                panic!();
            }
            Err(StateError::FailedConditions(_ids)) => {
                // Pass!
                return;
            }
        }
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
            vec![non_imaging.id()]);
        idle.add_transition(idle_to_non_imaging);

        // Create statechart object and add states to it
        let mut hyperion_statechart = StateChart::default();
        hyperion_statechart.add_state(idle, true);
        hyperion_statechart.add_state(diagnostic, false);
        hyperion_statechart.add_state(non_imaging, false);
        hyperion_statechart.add_state(imaging_standby, false);
        hyperion_statechart.add_state(imaging, false);

        // Broadcast a Go To Non-Imaging event
        hyperion_statechart.broadcast_event(&go_to_non_imaging).unwrap();

        // Verify that IDLE is inactive and NON-IMAGING is active
        assert_eq!(hyperion_statechart.active_state_ids().contains(&idle_id), false);
        assert_eq!(hyperion_statechart.active_state_ids().contains(&non_imaging_id), true);
    }
}
