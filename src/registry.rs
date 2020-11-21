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
    collections::HashMap,
    error::Error,
    fmt,
};

use crate::{
    event::{
        Event,
        EventId,
    },
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

//TODO: should this really be called "Registry"?
#[derive(Debug, Default, PartialEq)]
pub struct Registry {
    states: HashMap<StateId, State>,
    events: HashMap<EventId, Event>,
}

#[derive(Debug, PartialEq)]
pub enum RegistryError {
    StateAlreadyRegistered(StateId),
    EventAlreadyRegistered(EventId),
}

///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Registry {
    
    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    
    pub fn get_state(&self, id: StateId) -> Option<&State> {
        self.states.get(id)
    }

    pub fn get_mut_state(&mut self, id: StateId) -> Option<&mut State> {
        self.states.get_mut(id)
    }

    pub fn get_transition(&self, id: TransitionId) -> Option<&Transition> {
        // Search State map for a State containing this ID
        for state in self.states.values() {
            for transition in state.transitions() {
                if id == transition.id() {
                    return Some(transition);
                }
            }
        }

        None
    }

    pub fn get_active_state_ids(&self) -> Vec<StateId> {
        let mut active_state_ids = Vec::new();

        for state in self.states.values() {
            if state.is_active() {
                active_state_ids.push(state.id());
            }
        }

        active_state_ids
    }


    /*  *  *  *  *  *  *  *\
     *  Mutator Methods   *
    \*  *  *  *  *  *  *  */

    pub fn register_state(&mut self, state: State) -> Result<(), RegistryError> {
        // Ensure State is not already registered
        if self.states.contains_key(state.id()) {
            return Err(RegistryError::StateAlreadyRegistered(state.id()));
        }
        
        //TODO: Sanity check(s)

        // Add State and ID to the map
        self.states.insert(state.id(), state);
        Ok(())
    }

    pub fn register_event(&mut self, event: Event) -> Result<(), RegistryError> {
        // Ensure Event is not already registered
        if self.events.contains_key(&event.id()) {
            return Err(RegistryError::EventAlreadyRegistered(event.id()));
        }
        
        //TODO: Sanity check(s)

        // Add Event and ID to the map
        self.events.insert(event.id(), event);
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
            Self::EventAlreadyRegistered(event_id) => {
                write!(f, "Event with ID '{}' already registered", event_id)
            }            
        }
    }
}
