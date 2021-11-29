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

use std::{error::Error, fmt};

use crate::{
    event::Event,
    state::{State, StateId},
    transition::{Transition, TransitionFingerprint},
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Registry {
    states: Vec<State>,
    events: Vec<Event>,
}

#[derive(Debug, PartialEq)]
pub enum RegistryError {
    EventAlreadyRegistered(Event),
    StateAlreadyRegistered(StateId),
    StateNotFound(StateId),
    TransitionNotFound(TransitionFingerprint),
}

///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Registry {
    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    pub fn get_events(&self) -> &Vec<Event> {
        &self.events
    }

    pub fn get_state(&self, id: &str) -> Result<&State, RegistryError> {
        for state in &self.states {
            if state.id() == id {
                return Ok(state);
            }

            if let Some(state) = Self::get_substates(state, id) {
                return Ok(state);
            }
        }

        Err(RegistryError::StateNotFound(id.to_string()))
    }

    pub fn get_mut_state(&mut self, id: &str) -> Result<&mut State, RegistryError> {
        for state in &mut self.states {
            if state.id() == id {
                return Ok(state);
            }

            if let Some(state) = Self::get_mut_substates(state, id) {
                return Ok(state);
            }
        }

        Err(RegistryError::StateNotFound(id.to_string()))
    }

    pub fn get_all_states(&self) -> Vec<&State> {
        let mut states = Vec::new();

        for state in &self.states {
            states.push(state);
            states.append(&mut Self::get_all_substates(state));
        }

        states
    }

    pub fn get_active_states(&self) -> Vec<&State> {
        let mut active_states = Vec::new();

        for state in &self.states {
            if state.is_active() {
                active_states.push(state);
                active_states.append(&mut Self::get_active_substates(state));
            }
        }

        active_states
    }

    pub fn get_transition(&self, fingerprint: &str) -> Result<&Transition, RegistryError> {
        // Search State map for a State containing this ID
        for state in &self.states {
            for transition in state.transitions() {
                if fingerprint == transition.fingerprint() {
                    return Ok(transition);
                }
            }

            if let Some(transition) = Self::get_substate_transitions(state, fingerprint) {
                return Ok(transition);
            }
        }

        Err(RegistryError::TransitionNotFound(fingerprint.to_string()))
    }

    pub fn event_is_registered(&self, event: &Event) -> bool {
        self.events.contains(event)
    }


    /*  *  *  *  *  *  *  *\
     *  Mutator Methods   *
    \*  *  *  *  *  *  *  */

    pub fn register_state(&mut self, state: State) -> Result<(), RegistryError> {
        // Ensure State is not already registered
        if self.states.contains(&state) {
            return Err(RegistryError::StateAlreadyRegistered(
                state.id().to_string(),
            ));
        }

        // Traverse State and Substates for Events
        self.register_events_and_traverse_substates(&state)?;

        // Push State into the vector
        self.states.push(state);

        Ok(())
    }


    /*  *  *  *  *  *  *  *\
     *  Helper Methods    *
    \*  *  *  *  *  *  *  */

    fn register_event(&mut self, event: Event) -> Result<(), RegistryError> {
        // Ensure Event is not already registered
        if self.events.contains(&event) {
            return Err(RegistryError::EventAlreadyRegistered(event));
        }

        // Push Event into the vector
        self.events.push(event);
        Ok(())
    }

    fn register_events_and_traverse_substates(
        &mut self,
        state: &State,
    ) -> Result<(), RegistryError> {
        // Check current State's Transitions for Events
        for transition in state.transitions() {
            for event in transition.events() {
                // Clone is necessary as this function is meant to be called before moving
                // the referenced state into the state registry vector
                self.register_event(event.clone())?;
            }
        }

        // Traverse Substates for Events
        for substate in state.substates() {
            self.register_events_and_traverse_substates(substate)?;
        }

        Ok(())
    }

    fn get_substates<'s>(state: &'s State, id: &str) -> Option<&'s State> {
        for substate in state.substates() {
            if substate.id() == id {
                return Some(substate);
            }


            if let Some(substate) = Self::get_substates(substate, id) {
                return Some(substate);
            }
        }

        None
    }


    fn get_mut_substates<'s>(state: &'s mut State, id: &str) -> Option<&'s mut State> {
        for substate in state.mut_substates() {
            if substate.id() == id {
                return Some(substate);
            }


            if let Some(substate) = Self::get_mut_substates(substate, id) {
                return Some(substate);
            }
        }

        None
    }

    fn get_all_substates(state: &State) -> Vec<&State> {
        let mut substates = Vec::new();

        for substate in state.substates() {
            substates.push(substate);
            substates.append(&mut Self::get_all_substates(substate));
        }

        substates
    }

    fn get_active_substates(state: &State) -> Vec<&State> {
        let mut active_substates = Vec::new();

        for substate in state.substates() {
            if substate.is_active() {
                active_substates.push(substate);
                active_substates.append(&mut Self::get_active_substates(substate));
            }
        }

        active_substates
    }

    //OPT: *STYLE* Poorly-named function
    fn get_substate_transitions<'s>(state: &'s State, fingerprint: &str) -> Option<&'s Transition> {
        for substate in state.substates() {
            for transition in substate.transitions() {
                if fingerprint == transition.fingerprint() {
                    return Some(transition);
                }
            }

            if let Some(transition) = Self::get_substate_transitions(substate, fingerprint) {
                return Some(transition);
            }
        }

        None
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
            Self::EventAlreadyRegistered(event) => {
                write!(f, "Event '{}' already registered", event)
            }
            Self::StateAlreadyRegistered(state_id) => {
                write!(f, "State with ID '{}' already registered", state_id)
            }
            Self::StateNotFound(state_id) => {
                write!(
                    f,
                    "Could not find any States with ID '{}' in the Registry",
                    state_id
                )
            }
            Self::TransitionNotFound(fingerprint) => {
                write!(
                    f,
                    "Could not find any Transitions with Fingerprint '{}' in the Registry",
                    fingerprint
                )
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
        registry::{Registry, RegistryError},
        state::{State, StateBuilder},
        transition::TransitionFingerprint,
    };


    #[test]
    fn already_registered() -> Result<(), Box<dyn Error>> {
        // Create States and Events to be double-registered
        let state_id = "state";
        let event_id = "event";

        let state_a = StateBuilder::new(String::from(state_id)).build()?;
        let state_b = StateBuilder::new(String::from(state_id)).build()?;
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
            registry.register_event(event_a.clone()),
            Ok(()),
            "Valid Event registration failed"
        );

        // Verify that double-registration fails for both elements
        assert_eq!(
            registry.register_state(state_b),
            Err(RegistryError::StateAlreadyRegistered(String::from(
                state_id
            ))),
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
            Err(RegistryError::StateNotFound("nonexistent".to_string())),
            "get_state() somehow found a nonexistent State"
        );

        assert_eq!(
            registry.get_mut_state("nonexistent"),
            Err(RegistryError::StateNotFound("nonexistent".to_string())),
            "get_mut_state() somehow found a nonexistent State"
        );

        assert_eq!(
            registry.get_transition(&TransitionFingerprint::default()),
            Err(RegistryError::TransitionNotFound(
                TransitionFingerprint::default()
            )),
            "get_transition() somehow found a nonexistent Transition"
        );

        let empty_stateid_vec: Vec<&State> = Vec::new();
        assert_eq!(
            registry.get_active_states(),
            empty_stateid_vec,
            "get_active_states() somehow found nonexistent State(s)"
        );

        Ok(())
    }
}
