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
    This module defines a State object and ID. States conform to §3.3 <state>
    of the SCXML Spec.

    States are the keystone object of a StateChart, and may contain multiple
    Transitions, callbacks, and sub-States. They are entered and exited via
    activation of Transitions by Events.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
    error::Error,
    fmt,
};

use crate::{
    event::Event,
    transition::{
        Transition,
        TransitionId,
    },
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

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

pub type StateId = &'static str;

//TODO: Specify Result error type (generic, probably?) for short-circuiting failed transitions
/// Convenience alias for callbacks to be executed by on_entry, on_exit
pub type Callback = fn() -> Result<(), ()>;

#[derive(PartialEq)]
pub enum StateError {
    FailedCallback(usize),
    FailedConditions(Vec<TransitionId>),
}


#[derive(Debug, PartialEq)]
pub struct StateBuilder {
    id:             StateId,
    is_active:      bool,
    initial_ids:    Vec<StateId>,
    transitions:    Vec<Transition>,
    on_entry:       Vec<Callback>,
    on_exit:        Vec<Callback>,
}

#[derive(Debug, PartialEq)]
pub enum StateBuilderError {
    DuplicateInitialId(StateId),
    DuplicateTransition(TransitionId),
    TransitionSourceMismatch(TransitionId, StateId),
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl State {

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
    /// On success, returns a vector the target State ID(s) of the Enabled Transition.
    /// Note that this vector may be empty if no Transitions match the given Event.
    ///
    /// On failure, returns a vector of Transitions that matched the given Event, but
    /// failed their respective Condition.
    pub fn evaluate_event(&self, event: Event) -> Result<Option<TransitionId>, StateError> {
        // Check for Event match in each of this State's Transitions
        let mut enable_candidates = Vec::new();
        for transition in &self.transitions {
            for transition_event in transition.events() {
                if transition_event == &event {
                    enable_candidates.push(transition);
                }
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


impl StateBuilder {
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
     *  Builder Methods   *
    \*  *  *  *  *  *  *  */

    pub fn build(self) -> Result<State, StateBuilderError> {
        // Ensure all Transitions' source States match this one
        for transition in &self.transitions {
            if transition.source_id() != self.id {
                return Err(StateBuilderError::TransitionSourceMismatch(transition.id(), transition.source_id()));
            }
        }

        //TODO: When substates implemented, need to ensure that initial vector contains
        //      only IDs that match substates' IDs

        Ok(
            State {
                id:             self.id,
                is_active:      self.is_active,
                initial_ids:    self.initial_ids,
                transitions:    self.transitions,
                on_entry:       self.on_entry,
                on_exit:        self.on_exit,
            }
        )        
    }

    pub fn initial(mut self, initial_id: StateId) -> Result<Self, StateBuilderError> {
        // Ensure the ID is not a duplicate
        if self.initial_ids.contains(&initial_id) {
            return Err(StateBuilderError::DuplicateInitialId(initial_id));
        }
        
        self.initial_ids.push(&initial_id);

        Ok(self)
    }

    pub fn transition(mut self, transition: Transition) -> Result<Self, StateBuilderError> {
        // Ensure the Transition is not a duplicate
        if self.transitions.contains(&transition) {
            return Err(StateBuilderError::DuplicateTransition(transition.id()));
        }

        self.transitions.push(transition);

        Ok(self)
    }

    pub fn on_entry(mut self, callback: Callback) -> Self {
        // Callbacks are effectively unique, even if their signature/contents is identical
        // so we cannot check for duplicates. Just push onto the vector.
        self.on_entry.push(callback);

        self
    }

    pub fn on_exit(mut self, callback: Callback) -> Self {
        // Callbacks are effectively unique, even if their signature/contents is identical
        // so we cannot check for duplicates. Just push onto the vector.
        self.on_exit.push(callback);

        self
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

impl Error for StateError {}

impl fmt::Debug for StateError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

impl fmt::Display for StateError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::FailedCallback(idx) => {
                write!(f, "callback at index {} failed", idx)
            },
            Self::FailedConditions(transitions) => {
                write!(f, "conditions failed in transition(s) {:?}", transitions)
            }
        }
    }
}



/*  *  *  *  *  *  *  *\
 *  StateBuilderError *
\*  *  *  *  *  *  *  */

impl Error for StateBuilderError {}

impl fmt::Display for StateBuilderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::DuplicateInitialId(state_id) => {
                write!(f, "State '{}' is a duplicate of an existing initial State", state_id)
            },
            Self::DuplicateTransition(transition_id) => {
                write!(f, "Transition '{}' is a duplicate of an existing Transition", transition_id)
            },
            Self::TransitionSourceMismatch(transition_id, state_id) => {
                write!(f, "Transition '{}' source State '{}' does not match parent State", transition_id, state_id)
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
        StateChartBuilder,
        StateChartError,
        event::Event,
        state::{
            StateBuilder,
            StateError,
        },
        transition::TransitionBuilder,
    };


    /// Constvenience function for use as an "anti-null conditional"
    const fn always_false() -> bool { false }


    #[test]
    fn failed_condition() -> Result<(), Box<dyn Error>> {
        // Define Event and State IDs
        let go_to_unreachable = Event::from("go_to_unreachable")?;
        let initial_state_id = "INITIAL";
        let unreachable_state_id = "UNREACHABLE";

        // Build initial->unreachable Transition
        let initial_to_unreachable_id = "initial_to_unreachable";
        let initial_to_unreachable = TransitionBuilder::new(initial_to_unreachable_id, initial_state_id)
            .event(go_to_unreachable)?
            .cond(always_false)?
            .target_id(unreachable_state_id)?
            .build();

            
        // Build States
        let initial = StateBuilder::new(initial_state_id)
            .transition(initial_to_unreachable)?
            .build()?;
        let unreachable = StateBuilder::new(unreachable_state_id).build()?;

        // Build StateChart
        let mut hapless_statechart = StateChartBuilder::new("hapless")
            .initial(vec![initial.id()])
            .state(initial)?
            .state(unreachable)?
            .event(go_to_unreachable)?
            .build().unwrap();

        // Broadcast the event and verify that the transition failed its guard condition
        assert_eq!(
            hapless_statechart.process_external_event(go_to_unreachable),
            Err(StateChartError::StateError(StateError::FailedConditions(vec![initial_to_unreachable_id]))),
            "Failed to detect failed Transition due to failed Condition." 
        );

        Ok(())
    }

    #[test]
    fn failed_on_exit() -> Result<(), Box<dyn Error>> {
        // Define Event and State IDs
        let initial_to_terminal = Event::from("initial_to_terminal")?;
        let initial_state_id = "INITIAL";
        let terminal_state_id = "TERMINAL";

        // Build Transition
        let hapless_transition = TransitionBuilder::new("hapless", initial_state_id)
            .event(initial_to_terminal)?
            .target_id(terminal_state_id)?
            .build();

        // Build States, one of which will fail its 2nd on_exit callback
        let initial = StateBuilder::new(initial_state_id)
            .transition(hapless_transition)?
            .on_exit(|| {Ok(())})
            .on_exit(|| {Err(())})
            .build()?;
        let terminal = StateBuilder::new(terminal_state_id).build()?;
        
        // Build the StateChart and process the Event
        let mut statechart = StateChartBuilder::new("failed_on_exit")
            .initial(vec![initial.id()])
            .state(initial)?
            .state(terminal)?
            .event(initial_to_terminal)?
            .build().unwrap();
        
        assert_eq!(
            statechart.process_external_event(initial_to_terminal),
            Err(StateChartError::StateError(StateError::FailedCallback(1))),
            "Failed to catch a failed on_exit callback"
        );

        Ok(())
    }

    
    #[test]
    fn failed_on_entry() -> Result<(), Box<dyn Error>> {
        // Define Event and State IDs
        let initial_to_terminal = Event::from("initial_to_terminal")?;
        let initial_state_id = "INITIAL";
        let terminal_state_id = "TERMINAL";

        // Build Transition
        let hapless_transition = TransitionBuilder::new("hapless", initial_state_id)
            .event(initial_to_terminal)?
            .target_id(terminal_state_id)?
            .build();

        // Build States, one of which will fail its 2nd on_entry callback
        let initial = StateBuilder::new(initial_state_id)
            .transition(hapless_transition)?
            .build()?;
        let terminal = StateBuilder::new(terminal_state_id)
            .on_entry(|| {Ok(())})
            .on_entry(|| {Err(())})
            .build()?;
        
        // Build the StateChart and process the Event
        let mut statechart = StateChartBuilder::new("failed_on_entry")
            .initial(vec![initial.id()])
            .state(initial)?
            .state(terminal)?
            .event(initial_to_terminal)?
            .build().unwrap();
        
        assert_eq!(
            statechart.process_external_event(initial_to_terminal),
            Err(StateChartError::StateError(StateError::FailedCallback(1))),
            "Failed to catch a failed on_entry callback"
        );

        Ok(())
    }
}

#[cfg(test)]
mod builder_tests {
    
    use std::error::Error;

    use crate::{
        state::{
            StateBuilder,
            StateBuilderError,
        },
        transition::TransitionBuilder,
    };

    #[test]
    fn duplicate_initial() -> Result<(), Box<dyn Error>> {
        let duplicate_id = "duplicate";
        let builder = StateBuilder::new("state")
            .initial(duplicate_id)?;

        // Verify a duplicate ID is caught
        assert_eq!(
            builder.initial(duplicate_id),
            Err(StateBuilderError::DuplicateInitialId(duplicate_id)),
            "Failed to catch duplicate initial State"
        );

        Ok(())
    }

    #[test]
    fn duplicate_transition() -> Result<(), Box<dyn Error>> {
        let state_id = "source";
        let transition_id = "transition";

        // Build Transitions to be duplicated
        let original_transition = TransitionBuilder::new(transition_id, state_id).build();
        let duplicate_transition = TransitionBuilder::new(transition_id, state_id).build();

        // Verify the duplicate transition is caught
        let builder = StateBuilder::new(state_id)
            .transition(original_transition)?;

        assert_eq!(
            builder.transition(duplicate_transition),
            Err(StateBuilderError::DuplicateTransition(transition_id)),
            "Failed to catch duplicate Transition"
        );

        Ok(())
    }

    #[test]
    fn source_mismatch() -> Result<(), Box<dyn Error>> {
        let correct_source_id = "source";
        let wrong_source_id = "wrong";
        let transition_id = "transition";

        // Build Transition with an incorrect source
        let transition = TransitionBuilder::new(transition_id, wrong_source_id).build();

        // Verify mismatch is caught
        let builder = StateBuilder::new(correct_source_id)
            .transition(transition)?;
        
        assert_eq!(
            builder.build(),
            Err(StateBuilderError::TransitionSourceMismatch(transition_id, wrong_source_id)),
            "Failed to catch source State mismatch"
        );

        Ok(())
    }
}