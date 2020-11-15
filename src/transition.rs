/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : transition.rs

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
    state::StateId,
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Convenience alias for boolean expressions to be used as transition guards
pub type Condition = fn() -> bool;

//TODO: Probably expand this to a more detailed struct
pub type TransitionId = &'static str;

/// Represents a (single-target or multicast) transition from the current
/// (source) state to a target state
#[derive(PartialEq)]
pub struct Transition {
    id:         TransitionId,
    event_id:   EventId,
    cond:       Condition,
    source_id:  StateId,
    target_ids: Vec<StateId>,
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Transition {
    /// Fully-qualified constructor.
    /// 
    /// Creates a Transition with all of the given parameters.
    pub fn new(
        id: TransitionId,
        event_id: EventId,
        cond: Condition,
        source_id: StateId,
        target_ids: Vec<StateId>) -> Self {
            Self {
                id,
                event_id,
                cond,
                source_id,
                target_ids,
            }
        }

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    pub fn id(&self) -> TransitionId {
        self.id
    }

    pub fn event_id(&self) -> EventId {
        &self.event_id
    }

    pub fn source_id(&self) -> StateId {
        self.source_id
    }

    pub fn target_ids(&self) -> Vec<StateId> {
        // Clone is necessary to avoid issues with borrow-checker
        self.target_ids.clone()
    }


    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */
    
    /// Evaluates the guard condition for this Transition
    pub fn evaluate_condition(&self) -> bool {
        (self.cond)()
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

impl fmt::Debug for Transition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Transition")
            .field("id", &self.id)
            .field("event_id", &self.event_id)
            .field("cond", &self.cond)
            .field("source_id", &self.source_id)
            .field("target_ids", &self.target_ids)
            .finish()
    }
}
