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

//TODO: Specify Result error type (generic, probably?) for short-circuiting failed transitions
/// Convenience alias for callbacks to be executed by on_entry, on_exit
pub type Callback = fn() -> Result<(), ()>;

//TODO: Probably expand this to a more detailed struct
pub type StateId = &'static str;

/// Represents a state within the statechart.
/// 
/// May contain one or more of: initial states, transitions, entry/exit callbacks, substates
#[derive(Default, PartialEq)]
pub struct State {
    id:             StateId,
    is_active:      bool,
    initial_ids:    Vec<StateId>,
    transitions:    Vec<Transition>,
    on_entry:       Vec<Callback>,
    on_exit:        Vec<Callback>,
    //TODO: Substates
}

//TODO: Comment
#[derive(PartialEq)]
pub enum StateError {
    FailedCallback(usize),
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
            initial_ids:    Vec::new(),
            transitions:    Vec::new(),
            on_entry:       Vec::new(),
            on_exit:        Vec::new(),
        }
    }


    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    pub fn id(&self) -> StateId {
        self.id
    }

    pub fn is_active(&self) -> bool {
        self.is_active
    }

    pub fn transitions(&self) -> &Vec<Transition> {
        &self.transitions
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

    //TODO: Turn this into builder method?
    pub fn add_transition(&mut self, transition: Transition) {
        self.transitions.push(transition);
    }
    

    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */

    pub fn enter(&mut self) -> Result<(), StateError> {
        // Execute any on_enter callbacks
        self.execute_on_entry()?;

        // Activate the State
        self.activate();

        Ok(())
    }

    pub fn exit(&mut self) -> Result<(), StateError> {
        // Execute any on_exit callbacks
        self.execute_on_exit()?;
        
        // Deactivate the State
        self.deactivate();

        Ok(())
    }
    

    /// Evaluates the given Event against this State's set of Transitions to determine
    /// if any should be Enabled.
    ///
    /// Per §3.13 of th SCXML Standard, the "document order" of Transitions must
    /// be considered when resolving conflicts between Transitions. So the first
    /// Transition to be enabled will short-circuit the evaluation of further
    /// Transitions.
    ///
    //TODO: Fix comment for final implementation of this function
    /// On success, returns a vector the target State ID(s) of the Enabled Transition.
    /// Note that this vector may be empty if no Transitions match the given Event.
    ///
    /// On failure, returns a vector of Transitions that matched the given Event, but
    /// failed their respective Condition.
    pub fn evaluate_event(&self, event_id: EventId) -> Result<Option<TransitionId>, StateError> {
        // Check for Event match in each of this State's Transitions
        let mut enable_candidates = Vec::new();
        for transition in &self.transitions {
            if transition.event_id() == event_id {
                enable_candidates.push(transition);
            }
        }

        // If no candidates were identified, stop here and return an empty vector
        if enable_candidates.is_empty() {
            return Ok(None);
        }

        // Check candidates' Conditions
        let mut failed_candidates = Vec::new();
        for candidate in enable_candidates {
            if candidate.evaluate_condition() {
                // Short-circuit and return the Target of the first Transition to be Enabled
                return Ok(Some(candidate.id()))
            }
            else {
                // Add failed candidates' IDs to a list for potential error output
                failed_candidates.push(candidate.id());
            }
        }

        // All candidates failed to pass their Condition, return an error
        Err(StateError::FailedConditions(failed_candidates))
    }

    
    /*  *  *  *  *  *  *  *\
     *  Helper Methods    *
    \*  *  *  *  *  *  *  */

    fn execute_on_entry(&self) -> Result<(), StateError> {
        for (i, entry_func) in self.on_entry.iter().enumerate() {
            if let Some(_err) = (entry_func)().err() {
                // Callback failed, return error indicating the callback index that failed
                return Err(StateError::FailedCallback(i));
            }
        }

        Ok(())
    }

    fn execute_on_exit(&self) -> Result<(), StateError> {
        for (i, exit_func) in self.on_exit.iter().enumerate() {
            if let Some(_err) = (exit_func)().err() {
                // Callback failed, return error indicating the callback index that failed
                return Err(StateError::FailedCallback(i));
            }
        }

        Ok(())
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
            .field("initial_ids",   &self.initial_ids)
            .field("transitions",   &self.transitions)
            .field("on_entry",      &self.on_entry)
            .field("on_exit",       &self.on_exit)
            .finish()
    }
}


/*  *  *  *  *  *  *  *\
 *     StateError     *
\*  *  *  *  *  *  *  */
impl fmt::Debug for StateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FailedCallback(idx) => {
                f.debug_struct("FailedCallback:")
                    .field("Index", idx)
                    .finish()
            },
            Self::FailedConditions(transitions) => {
                f.debug_struct("FailedCondition(s)")
                    .field("transitions", transitions)
                    .finish()
            }
        }
    }
}
