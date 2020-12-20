/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : registry.rs

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
    This module defines a Registry object that tracks elements of the Statechart.

    It is meant to be a sole source of information on States, Transformation, and
    valid Events. Additionally, it is the gatekeeper for mutable State objects.
    
    Many functions of StateChart processing require both reading and mutating
    States,such as determining if a target state should be activated, and Rust's
    borrow checker (wisely) will not allow these in-place mutations. The Registry
    provides a safe method to retrieve a mutable reference via ID.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
    error::Error,
    fmt,
};

use crate::{
    event::Event,
    state::{
        State,
        StateId,
    },
    transition::{
        Transition,
        TransitionId,
    }
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Default, PartialEq)]
pub struct Registry {
    states: Vec<State>,
    events: Vec<Event>,
}

#[derive(Debug, PartialEq)]
pub enum RegistryError {
    StateAlreadyRegistered(StateId),
    EventAlreadyRegistered(Event),
}

///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Registry {
    
    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    
    pub fn get_state(&self, id: StateId) -> Option<&State> {
        for state in self.states.iter() {
            if state.id() == id {
                return Some(state)
            }
        }

        None
    }

    pub fn get_mut_state(&mut self, id: StateId) -> Option<&mut State> {
        for state in self.states.iter_mut() {
            if state.id() == id {
                return Some(state)
            }
        }

        None
    }

    pub fn get_all_state_ids(&self) -> Vec<StateId> {
        let mut state_ids = Vec::new();

        for state in self.states.iter() {
            state_ids.push(state.id());
        }

        state_ids
    }

    pub fn get_active_state_ids(&self) -> Vec<StateId> {
        let mut active_state_ids = Vec::new();

        for state in self.states.iter() {
            if state.is_active() {
                active_state_ids.push(state.id());
            }
        }

        active_state_ids
    }

    pub fn get_transition(&self, id: TransitionId) -> Option<&Transition> {
        // Search State map for a State containing this ID
        for state in self.states.iter() {
            for transition in state.transitions() {
                if id == transition.id() {
                    return Some(transition);
                }
            }
        }

        None
    }

    pub fn event_is_registered(&self, event: Event) -> bool {
        self.events.contains(&event)
    }


    /*  *  *  *  *  *  *  *\
     *  Mutator Methods   *
    \*  *  *  *  *  *  *  */

    pub fn register_state(&mut self, state: State) -> Result<(), RegistryError> {
        // Ensure State is not already registered
        if self.states.contains(&state) {
            return Err(RegistryError::StateAlreadyRegistered(state.id()));
        }
        
        //TODO: Sanity check(s) for invalid State arrangements

        // Push State into the vector
        self.states.push(state);

        Ok(())
    }

    pub fn register_event(&mut self, event: Event) -> Result<(), RegistryError> {
        // Ensure Event is not already registered
        if self.events.contains(&event) {
            return Err(RegistryError::EventAlreadyRegistered(event));
        }

        // Push Event into the vector
        self.events.push(event);
        Ok(())
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *    RegistryError   *
\*  *  *  *  *  *  *  */

impl Error for RegistryError {}

impl fmt::Display for RegistryError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::StateAlreadyRegistered(state_id) => {
                write!(f, "State with ID '{}' already registered", state_id)
            },
            Self::EventAlreadyRegistered(event) => {
                write!(f, "Event '{}' already registered", event)
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
        event::Event,
        registry::{
            Registry,
            RegistryError,
        },
        state::{
            StateBuilder,
            StateId,
        },
    };

    #[test]
    fn already_registered() -> Result<(), Box<dyn Error>> {
        // Create States and Events to be double-registered
        let state_id = "state";
        let event_id = "event";

        let state_a = StateBuilder::new(state_id).build()?;
        let state_b = StateBuilder::new(state_id).build()?;
        let event_a = Event::from(event_id)?;
        let event_b = Event::from(event_id)?;

        // Create the Registry and register the elements
        let mut registry = Registry::default();
        assert_eq!(
            registry.register_state(state_a),
            Ok(()),
            "Valid State registration failed"
        );
        assert_eq!(
            registry.register_event(event_a),
            Ok(()),
            "Valid Event registration failed"
        );
        
        // Verify that double-registration fails for both elements
        assert_eq!(
            registry.register_state(state_b),
            Err(RegistryError::StateAlreadyRegistered(state_id)),
            "Failed to reject State double-registration"
        );
        assert_eq!(
            registry.register_event(event_b),
            Err(RegistryError::EventAlreadyRegistered(event_a)),
            "Failed to reject Event double-registration"
        );

        Ok(())
    }

    #[test]
    fn missing_elements() -> Result<(), Box<dyn Error>> {
        // Create an empty Registry and attempt lookups for non-existent elements
        let mut registry = Registry::default();

        assert_eq!(
            registry.get_state("nonexistent"),
            None,
            "get_state() somehow found a nonexistent State"
        );

        assert_eq!(
            registry.get_mut_state("nonexistent"),
            None,
            "get_mut_state() somehow found a nonexistent State"
        );

        assert_eq!(
            registry.get_transition("nonexistent"),
            None,
           "get_transition() somehow found a nonexistent Transition"
        );

        let empty_stateid_vec: Vec<StateId> = Vec::new();
        assert_eq!(
            registry.get_active_state_ids(),
            empty_stateid_vec,
            "get_active_state_ids() somehow found nonexistent State(s)"
        );

        Ok(())
    }
}
