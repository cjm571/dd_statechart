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

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

//TODO: Need to use function pointer for this or some shit
//TODO: Add nested states
//TODO: Proper error enum implementation with description() and shit

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
    pub fn active_state_ids(&self) -> Vec<&StateId> {
        let mut active_states: Vec<&StateId> = Vec::new();
        
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
    pub fn broadcast_event(&mut self, event: &Event) -> Result<(), StateChartError> {
        //TODO: Sanity-check event?

        // Traverse the map of states and send the event to each for evaluation
        let mut state_changes: HashMap<StateId, Vec<StateId>> = HashMap::new();
        for state in self.states.values_mut() {
            match state.evaluate_event(event.id()) {
                Ok(new_state_ids)   => {
                    state_changes.insert(state.id(), new_state_ids);
                },
                Err(e)          => {
                    //TODO: Do something with the error
                    println!("Error: {:?}", e);
                }
            }
        }

        // Deactivate the old states and activate the new one(s)
        for (old_state_id, new_state_ids) in state_changes {
            self.get_mut_state_from_id(old_state_id).deactivate();

            for state_id in new_state_ids {
                self.get_mut_state_from_id(state_id).activate();
            }
        }

        Ok(())
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
        State,
        StateChart,
    };

    const EMPTY_STATECHART_PRINT: &str = "StateChart { id: \"\", initial: [], states: {} }";

    #[test]
    fn empty_statechart() {
        let statechart = StateChart::default();

        assert_eq!(EMPTY_STATECHART_PRINT, format!("{:?}", statechart));
    }

    #[test]
    fn sandbox() {
        // Define states to be added to the statechart
        let state_a = State::new("A");

        let mut statechart = StateChart::default();
        statechart.add_state(state_a, true);

        println!("{:#?}", statechart);
    }
}
