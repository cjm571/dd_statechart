/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : state.rs

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
    //TODO: Purpose statement

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::fmt;

use crate::{
    event::EventId,
    transition::{
        Transition,
        TransitionId,
    },
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

//TODO: Specify Result return value for short-circuiting failed transitions
/// Convenience alias for callbacks to be executed by on_entry, on_exit
pub type Callback = fn();

//TODO: Probably expand this to a more detailed struct
pub type StateId = &'static str;

/// Represents a state within the statechart.
/// 
/// May contain one or more of: initial states, transitions, entry/exit callbacks, substates
#[derive(Default)]
pub struct State {
    id:             StateId,
    is_active:      bool,
    initial:        Vec<StateId>,
    transitions:    Vec<Transition>,
    on_entry:       Vec<Callback>,
    on_exit:        Vec<Callback>,
    substates:      Vec<State>,
}

//TODO: Comment
pub enum StateError {
    FailedConditions(Vec<TransitionId>),
}

///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl State {
    pub fn new(id: StateId) -> Self {
        Self {
            id,
            is_active:      false,
            initial:        Vec::new(),
            transitions:    Vec::new(),
            on_entry:       Vec::new(),
            on_exit:        Vec::new(),
            substates:      Vec::new(),
        }
    }


    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    pub fn id(&self) -> &StateId {
        &self.id
    }

    pub fn is_active(&self) -> bool {
        self.is_active
    }


    /*  *  *  *  *  *  *  *\
     *  Mutator Methods   *
    \*  *  *  *  *  *  *  */
    pub fn activate(&mut self) {
        self.is_active = true;
    }
    
    pub fn deactivate(&mut self) {
        self.is_active = false;
    }
    

    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */

    /// Evaluates the given event for all Transitions associate with this State.
    /// 
    /// On success, returns a vector containing the IDs of all new active states.
    /// 
    /// On failure, returns a StateError.
    pub fn evaluate_event(&self, event: &EventId) -> Result<Vec<StateId>, StateError> {
        let mut new_active_state_ids: Vec<StateId> = Vec::new();

        // Iterate through translations, adding target states to the return vec if
        // their guard condition passes
        for transition in self.transitions.iter() {
            if transition.triggers().contains(event) && transition.evaluate_condition() {
                for target_state in transition.targets() {
                    new_active_state_ids.push(target_state);
                }
            }
        }

        Ok(new_active_state_ids)
    }    
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *       State        *
\*  *  *  *  *  *  *  */
impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("State")
            .field("id",            &self.id)
            .field("is_active",     &self.is_active)
            .field("initial",       &self.initial)
            .field("transitions",   &self.transitions)
            .field("on_entry",      &self.on_entry)
            .field("on_exit",       &self.on_exit)
            .field("substates",     &self.substates)
            .finish()
    }
}


/*  *  *  *  *  *  *  *\
 *     StateError     *
\*  *  *  *  *  *  *  */
impl fmt::Debug for StateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FailedConditions(transitions) => {
                f.debug_struct("FailedCondition(s)")
                    .field("transitions", transitions)
                    .finish()
            }
        }
    }
}