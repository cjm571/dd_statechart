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
    datamodel::SystemVariables,
    event::Event,
    executable_content::ExecutableContent,
    interpreter::{
        Interpreter,
        InterpreterError,
    },
    state::StateId,
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Represents a (single-target or multicast) transition from the current
/// (source) state to a target state
#[derive(Clone, PartialEq)]
pub struct Transition {
    fingerprint:        TransitionFingerprint,
    events:             Vec<Event>,
    //OPT: *PERFORMANCE* Store lexed/parsed expression here?
    cond:               String,
    source_id:          StateId,
    target_ids:         Vec<StateId>,
    executable_content: Vec<ExecutableContent>,
}

/// Unique String representation of a Transition for the purpose of lookups
pub type TransitionFingerprint = String;

#[derive(Debug, PartialEq)]
pub enum TransitionError {
    // Wrappers
    InterpreterError(InterpreterError),
}


#[derive(Debug, PartialEq)]
pub struct TransitionBuilder {
    fingerprint:        TransitionFingerprint,
    events:             Vec<Event>,
    cond:               String,
    cond_set:           bool,
    source_id:          StateId,
    target_ids:         Vec<StateId>,
    executable_content: Vec<ExecutableContent>,
}

#[derive(Debug, PartialEq)]
pub enum TransitionBuilderError {
    ConditionAlreadySet(
        String, /* New (rejected) condition expression */
        String, /* Existing condition expression */
    ),
    DuplicateEventId(Event),
    DuplicateTargetId(StateId),
    NoEventCondOrTarget,
    SourceTargetCollision(StateId),
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Transition {

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    pub fn fingerprint(&self) -> &TransitionFingerprint {
        &self.fingerprint
    }

    pub fn events(&self) -> &Vec<Event> {
        &self.events
    }

    pub fn source_id(&self) -> &StateId {
        &self.source_id
    }

    pub fn target_ids(&self) -> &Vec<StateId> {
        &self.target_ids
    }

    pub fn executable_content(&self) -> &Vec<ExecutableContent> {
        &self.executable_content
    }


    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */
    
    /// Evaluates the guard condition for this Transition
    pub fn evaluate_condition(&self, sys_vars: &SystemVariables) -> Result<bool, TransitionError> {
        let interpreter = Interpreter::new(self.cond.as_str());
        
        interpreter.interpret_as_bool(sys_vars).map_err(TransitionError::InterpreterError)
        
        //FEAT: If condition has returned an error, an 'error.execution' event must be placed on the internal event queue
        //      See §5.9.1
    }
}

//FEAT: Make this a non-consuming builder
impl TransitionBuilder {
    pub fn new(source_state_id: &str) -> Self {
        Self {
            fingerprint:        TransitionFingerprint::default(),
            events:             Vec::new(),
            cond:               String::from("true"),
            cond_set:           false,
            source_id:          source_state_id.to_string(),
            target_ids:         Vec::new(),
            executable_content: Vec::new(),
        }
    }

    /*  *  *  *  *  *  *  *\
     *  Builder Methods   *
    \*  *  *  *  *  *  *  */

    pub fn build(self) -> Result<Transition, TransitionBuilderError> {
        // Per §3.5, Transitions must have at least one of 'event', 'cond', or 'target'
        if self.events.is_empty() && !self.cond_set && self.target_ids.is_empty() {
            return Err(TransitionBuilderError::NoEventCondOrTarget);
        }

        // Assemble the components of the fingerprint
        let mut fingerprint_components = vec![self.source_id.clone()];
        for event in &self.events {
            fingerprint_components.push(event.id());
        }
        fingerprint_components.push(self.cond.clone());
        for target_id in &self.target_ids {
            fingerprint_components.push(target_id.clone());
        }

        // Build the Transition
        Ok ( Transition {
            fingerprint:        fingerprint_components.into_iter().collect::<String>(),
            events:             self.events,
            cond:               self.cond,
            source_id:          self.source_id.clone(),
            target_ids:         self.target_ids,
            executable_content: self.executable_content,
        })
    }

    pub fn event(mut self, event: &Event) -> Result<Self, TransitionBuilderError> {
        // Clone the Event since we'll have to own it in order to push it into the vector
        // or return it as an error
        let owned_event = event.clone();

        // Ensure the given ID is not already in the event vector
        if self.events.contains(&owned_event) {
            return Err(TransitionBuilderError::DuplicateEventId(owned_event));
        }

        self.events.push(owned_event);

        Ok(self)
    }

    pub fn cond(mut self, cond: &str) -> Result<Self, TransitionBuilderError> {
        // Ensure condition has not already been set
        if self.cond_set {
            return Err(TransitionBuilderError::ConditionAlreadySet(cond.to_string(), self.cond));
        }
        
        self.cond = String::from(cond);
        self.cond_set = true;

        Ok(self)
    }

    pub fn target_id(mut self, target_id: &str) -> Result<Self, TransitionBuilderError> {
        // Clone the ID since we'll have to own it in order to push it into the vector
        // or return it as an error
        let owned_id = target_id.to_string();

        // Ensure the given ID is not already in the target vector
        if self.target_ids.contains(&owned_id) {
            return Err(TransitionBuilderError::DuplicateTargetId(owned_id));
        }

        // Ensure target is not the same as the source
        if target_id == self.source_id {
            return Err(TransitionBuilderError::SourceTargetCollision(owned_id));
        }

        self.target_ids.push(owned_id);

        Ok(self)
    }

    pub fn executable_content(mut self, content: ExecutableContent) -> Self {
        // Intentionally moving the ExecutableContent, since it doesn't make sense for
        // the client to keep using it after passing in here
        self.executable_content.push(content);

        self
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
            .field("fingerprint",   &self.fingerprint)
            .field("event",         &self.events)
            .field("cond",          &self.cond)
            .field("source_id",     &self.source_id)
            .field("target_ids",    &self.target_ids)
            .finish()
    }
}


/*  *  *  *  *  *  *  *  *  *\
 *      TransitionError     *
\*  *  *  *  *  *  *  *  *  */

impl Error for TransitionError {}

impl fmt::Display for TransitionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // Wrappers
            Self::InterpreterError(interp_err) => {
                write!(f, "InterpreterError '{:?}' encountered while processing a Transition", interp_err)
            },
        }
    }
}

impl From<InterpreterError> for TransitionError {
    fn from(src: InterpreterError) -> Self {
        Self::InterpreterError(src)
    }
}


/*  *  *  *  *  *  *  *  *  *\
 *  TransitionBuilderError  *
\*  *  *  *  *  *  *  *  *  */

impl Error for TransitionBuilderError {}

impl fmt::Display for TransitionBuilderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ConditionAlreadySet(new_cond, existing_cond) => {
                write!(f, "Transition condition '{}' rejected, existing condition '{}' already set", new_cond, existing_cond)
            },
            Self::DuplicateEventId(event) => {
                write!(f, "Event '{}' is already in the Event vector", event)
            },
            Self::DuplicateTargetId(target_id) => {
                write!(f, "ID '{}' is already in the target State vector", target_id)
            },
            Self::NoEventCondOrTarget => {
                write!(f, "Attempted to build a Transition that did not have at least one of 'event', 'cond', or 'target'")
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
        transition::{
            TransitionBuilder,
            TransitionBuilderError,
        },
    };

    #[test]
    fn duplicate_event() -> Result<(), Box<dyn Error>> {
        // Define Event
        let event = Event::from("event")?;

        // Verify that duplicate event is caught
        let builder = TransitionBuilder::new("state")
            .event(&event)?;

        assert_eq!(
            builder.event(&event),
            Err(TransitionBuilderError::DuplicateEventId(event)),
            "Failed to catch duplicate event"
        );

        Ok(())
    }
    
    #[test]
    fn duplicate_target() -> Result<(), Box<dyn Error>> {
        // Verify that duplicate target is caught
        let target_id = String::from("target");
        let builder = TransitionBuilder::new("source")
            .target_id(&target_id)?;

        assert_eq!(
            builder.target_id(&target_id),
            Err(TransitionBuilderError::DuplicateTargetId(target_id)),
            "Failed to catch duplicate target"
        );

        Ok(())
    }
    
    #[test]
    fn source_target_collision() -> Result<(), Box<dyn Error>> {
        // Verify that source-target collision is caught
        let source_id = String::from("source");
        let builder = TransitionBuilder::new(source_id.as_str());

        assert_eq!(
            builder.target_id(&source_id),
            Err(TransitionBuilderError::SourceTargetCollision(source_id)),
            "Failed to catch source-target collision"
        );

        Ok(())
    }

    #[test]
    fn condition_already_set() -> Result<(), Box<dyn Error>> {
        // Verify that already-set condition is caught
        let builder = TransitionBuilder::new("source")
            .cond("true")?;

        assert_eq!(
            builder.cond("true"),
            Err(TransitionBuilderError::ConditionAlreadySet("true".to_string(), "true".to_string())),
            "Failed to catch already-set condition"
        );

        Ok(())
    }
}
