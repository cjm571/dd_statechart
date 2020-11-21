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
    This module defines a Transition object and ID. Transitions conform to 
    §3.5 <transition> of the SCXML Spec.

    Transitions are the actionable components of a StateChart. They are enabled
    by Events, and may contain executable content as well as triggering an exit
    of their source State and entry of their target State(s).

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
    error::Error,
    fmt,
};

use crate::{
    event::EventId,
    state::StateId,
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Represents a (single-target or multicast) transition from the current
/// (source) state to a target state
#[derive(PartialEq)]
pub struct Transition {
    id:         TransitionId,
    event_ids:  Vec<EventId>,
    cond:       Condition,
    source_id:  StateId,
    target_ids: Vec<StateId>,
}

pub type TransitionId = &'static str;

/// Convenience alias for boolean expressions to be used as transition guards
pub type Condition = fn() -> bool;


#[derive(Debug, PartialEq)]
pub struct TransitionBuilder {
    id:         TransitionId,
    event_ids:  Vec<EventId>,
    cond:       Condition,
    cond_set:   bool,
    source_id:  StateId,
    target_ids: Vec<StateId>,
}

#[derive(Debug, PartialEq)]
pub enum TransitionBuilderError {
    ConditionAlreadySet,
    DuplicateEventId(EventId),
    DuplicateTargetId(StateId),
    SourceTargetCollision(StateId),
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Transition {

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    pub fn id(&self) -> TransitionId {
        self.id
    }

    pub fn event_ids(&self) -> &Vec<EventId> {
        &self.event_ids
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


impl TransitionBuilder {
    pub fn new(id: TransitionId, source_state_id: StateId) -> Self {
        Self {
            id,
            event_ids:  Vec::new(),
            cond:       || {true},
            cond_set:   false,
            source_id: source_state_id,
            target_ids: Vec::new(),
        }
    }

    /*  *  *  *  *  *  *  *\
     *  Builder Methods   *
    \*  *  *  *  *  *  *  */

    pub fn build(self) -> Transition {
        Transition {
            id:         self.id,
            event_ids:  self.event_ids,
            cond:       self.cond,
            source_id:  self.source_id,
            target_ids: self.target_ids,
        }
    }

    pub fn event_id(mut self, event_id: EventId) -> Result<Self, TransitionBuilderError> {
        // Ensure the given ID is not already in the event vector
        if self.event_ids.contains(&event_id) {
            return Err(TransitionBuilderError::DuplicateEventId(event_id));
        }

        self.event_ids.push(event_id);

        Ok(self)
    }

    pub fn cond(mut self, cond: Condition) -> Result<Self, TransitionBuilderError> {
        // Ensure condition has not already been set
        if self.cond_set {
            return Err(TransitionBuilderError::ConditionAlreadySet);
        }
        
        self.cond = cond;
        self.cond_set = true;

        Ok(self)
    }

    pub fn target_id(mut self, target_id: StateId) -> Result<Self, TransitionBuilderError> {
        // Ensure the given ID is not already in the target vector
        if self.target_ids.contains(&target_id) {
            return Err(TransitionBuilderError::DuplicateTargetId(target_id));
        }

        // Ensure target is not the same as the source
        if target_id == self.source_id {
            return Err(TransitionBuilderError::SourceTargetCollision(target_id));
        }

        self.target_ids.push(target_id);

        Ok(self)
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *     Transition     *
\*  *  *  *  *  *  *  */

impl fmt::Debug for Transition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Transition")
            .field("id", &self.id)
            .field("event_id", &self.event_ids)
            .field("cond", &self.cond)
            .field("source_id", &self.source_id)
            .field("target_ids", &self.target_ids)
            .finish()
    }
}


/*  *  *  *  *  *  *  *  *  *\
 *  TransitionBuilderError  *
\*  *  *  *  *  *  *  *  *  */

impl Error for TransitionBuilderError {}

impl fmt::Display for TransitionBuilderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ConditionAlreadySet => {
                write!(f, "transition condition has already been set")
            },
            Self::DuplicateEventId(event_id) => {
                write!(f, "ID '{}' is already in the Event vector", event_id)
            },
            Self::DuplicateTargetId(target_id) => {
                write!(f, "ID '{}' is already in the target State vector", target_id)
            },
            Self::SourceTargetCollision(target_id) => {
                write!(f, "ID '{}' collides with the Transition's source State ID", target_id)
            }
        }
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod builder_tests {

    use std::error::Error;

    use crate::{
        event::Event,
        state::State,
        transition::{
            TransitionBuilder,
            TransitionBuilderError,
        },
    };

    #[test]
    fn duplicate_event() -> Result<(), Box<dyn Error>> {
        // Define Event and State
        let event = Event::new("event")?;
        let state = State::new("state");

        // Verify that duplicate event is caught
        let builder = TransitionBuilder::new("transition", state.id())
            .event_id(event.id())?;

        assert_eq!(
            builder.event_id(event.id()),
            Err(TransitionBuilderError::DuplicateEventId(event.id())),
            "Failed to catch duplicate event"
        );

        Ok(())
    }
    
    #[test]
    fn duplicate_target() -> Result<(), Box<dyn Error>> {
        // Define States
        let source = State::new("source");
        let target = State::new("target");

        // Verify that duplicate target is caught
        let builder = TransitionBuilder::new("transition", source.id())
            .target_id(target.id())?;

        assert_eq!(
            builder.target_id(target.id()),
            Err(TransitionBuilderError::DuplicateTargetId(target.id())),
            "Failed to catch duplicate target"
        );

        Ok(())
    }
    
    #[test]
    fn source_target_collision() -> Result<(), Box<dyn Error>> {
        // Define State
        let source = State::new("source");

        // Verify that source-target collision is caught
        let builder = TransitionBuilder::new("transition", source.id());

        assert_eq!(
            builder.target_id(source.id()),
            Err(TransitionBuilderError::SourceTargetCollision(source.id())),
            "Failed to catch source-target collision"
        );

        Ok(())
    }

    #[test]
    fn condition_already_set() -> Result<(), Box<dyn Error>> {
        // Define State
        let source = State::new("source");

        // Verify that already-set condition is caught
        let builder = TransitionBuilder::new("transition", source.id())
            .cond(|| {true})?;

        assert_eq!(
            builder.cond(|| {true}),
            Err(TransitionBuilderError::ConditionAlreadySet),
            "Failed to catch already-set condition"
        );

        Ok(())
    }
}
