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
    datamodel::SystemVariables,
    event::Event,
    transition::{
        Transition,
        TransitionError,
        TransitionId,
    },
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Represents a state within the statechart.
/// 
/// May contain one or more of: initial states, transitions, entry/exit callbacks, substates
#[derive(Clone, Default, PartialEq)]
pub struct State {
    id:             StateId,
    substates:      Vec<State>,
    is_active:      bool,
    initial_id:     Option<StateId>,
    transitions:    Vec<Transition>,
    on_entry:       Vec<Callback>,
    on_exit:        Vec<Callback>,
}

pub type StateId = String;

//OPT: *DESIGN* Would it be useful to specify an error type here? Even if it's just Box<dyn Error>?
pub type Callback = fn() -> Result<(), ()>;

#[derive(Debug, PartialEq)]
pub enum StateError {
    FailedCallback(usize),
    SubstatesSpecifiedWithoutInitial,

    // Wrappers
    TransitionError(TransitionError),
}


#[derive(Debug, PartialEq)]
pub struct StateBuilder {
    id:             StateId,
    substates:      Vec<State>,
    is_active:      bool,
    initial_id:     Option<StateId>,
    transitions:    Vec<Transition>,
    on_entry:       Vec<Callback>,
    on_exit:        Vec<Callback>,
}

#[derive(Debug, PartialEq)]
pub enum StateBuilderError {
    DuplicateSubstate(StateId),
    DuplicateTransition(TransitionId),
    TransitionSourceMismatch(TransitionId, StateId),
    InitialIsNotChild(StateId),
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl State {

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    pub fn id(&self) -> StateId {
        self.id.clone()
    }

    pub fn is_active(&self) -> bool {
        self.is_active
    }

    pub fn substates(&self) -> &Vec<State> {
        &self.substates
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

    pub fn mut_substates(&mut self) -> &mut Vec<State> {
        &mut self.substates
    }
    

    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */

    pub fn enter(&mut self) -> Result<(), StateError> {
        // Execute any on_enter callbacks
        self.execute_on_entry()?;

        // Activate the State
        self.activate();

        // If substates exist, enter the initial substate
        for substate in &mut self.substates {
            if let Some(initial_id) = &self.initial_id {
                if &substate.id() == initial_id {
                    substate.enter()?;
                }
            }
            else {
                return Err(StateError::SubstatesSpecifiedWithoutInitial);
            }
        }

        Ok(())
    }

    pub fn exit(&mut self) -> Result<(), StateError> {
        // If substates exist, exit the active substate(s)
        for substate in &mut self.substates {
            if substate.is_active() {
                substate.exit()?;
            }
        }
        
        // Execute any on_exit callbacks
        self.execute_on_exit()?;
        
        // Deactivate the State
        self.deactivate();

        Ok(())
    }
    

    pub fn evaluate_event(&self, event: Option<Event>, sys_vars: &SystemVariables) -> Result<Option<TransitionId>, StateError> {
        let mut enable_candidates = Vec::new();
        
        // Handle evaluation of a non-null Event
        if let Some(event) = &event {
            // Check for Event match in each of this State's Transitions
            for transition in &self.transitions {
                for transition_event in transition.events() {
                    if transition_event == event {
                        enable_candidates.push(transition);
                    }
                }
            }
        }
        // Handle evaluation of a "Null" Event
        else {
            // Check for eventless Transitions in this State
            for transition in &self.transitions {
                if transition.events().is_empty() {
                    enable_candidates.push(transition);
                }
            }
        }

        // Check candidates' Conditions
        for candidate in enable_candidates {
            if candidate.evaluate_condition(sys_vars)? {
                // Short-circuit and return the Target of the first Transition to be Enabled
                return Ok(Some(candidate.id()))
            }
        }

        // Either no candidates identified, or none passed their guard condition
        Ok(None)
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
            substates:      Vec::new(),
            initial_id:     None,
            transitions:    Vec::new(),
            on_entry:       Vec::new(),
            on_exit:        Vec::new(),
        }
    }


    /*  *  *  *  *  *  *  *\
     *  Builder Methods   *
    \*  *  *  *  *  *  *  */

    //TODO: Make this a non-consuming Builder
    pub fn build(mut self) -> Result<State, StateBuilderError> {
        // No-child `initial` sanity check
        if self.substates.is_empty() && self.initial_id.is_some() {
            return Err(StateBuilderError::InitialIsNotChild(self.initial_id.unwrap()));
        }

        // Sanity checks for child states, if they exist
        if !self.substates.is_empty() {
            // If no initial ID was provided, set to first doc-order child
            if self.initial_id.is_none() {
                self.initial_id = Some(self.substates.first().unwrap().id());
            }

            // Ensure that initial ID matches a child State
            let mut child_matched = false;
            for substate in &self.substates {
                //TODO: Initial ID cannot be copied due to StateId == String. should be &str
                if substate.id() == self.initial_id.clone().unwrap() {
                    child_matched = true;
                }
            }
            if !child_matched {
                return Err(StateBuilderError::InitialIsNotChild(self.initial_id.unwrap()));
            }
        }

        // Ensure all Transitions' source States match this one
        for transition in &self.transitions {
            if transition.source_id() != self.id {
                return Err(StateBuilderError::TransitionSourceMismatch(transition.id(), transition.source_id()));
            }
        }

        Ok(
            State {
                id:             self.id,
                substates:      self.substates,
                is_active:      self.is_active,
                initial_id:     self.initial_id,
                transitions:    self.transitions,
                on_entry:       self.on_entry,
                on_exit:        self.on_exit,
            }
        )        
    }

    pub fn substate(mut self, state: State) -> Result<Self, StateBuilderError> {
        // Ensure the substate is not a duplicate
        if state.id() == self.id {
            return Err(StateBuilderError::DuplicateSubstate(state.id()));
        }
        for substate in &self.substates {
            if state.id() == substate.id() {
                return Err(StateBuilderError::DuplicateSubstate(state.id()));
            }
        }

        self.substates.push(state);

        Ok(self)
    }

    pub fn initial(mut self, state_id: StateId) -> Self {
        self.initial_id = Some(state_id);

        self
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
            .field("substates",     &self.substates)
            .field("is_active",     &self.is_active)
            .field("initial_id",    &self.initial_id)
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

impl fmt::Display for StateError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::FailedCallback(idx) => {
                write!(f, "callback at index {} failed", idx)
            },
            Self::SubstatesSpecifiedWithoutInitial => {
                write!(f, "State has substates but no Initial State")
            },
            
            // Wrappers
            Self::TransitionError(trans_err) => {
                write!(f, "TransitionError '{:?}' encountered while processing State", trans_err)
            },
        }
    }
}

impl From<TransitionError> for StateError {
    fn from(src: TransitionError) -> Self {
        Self::TransitionError(src)
    }
}



/*  *  *  *  *  *  *  *\
 *  StateBuilderError *
\*  *  *  *  *  *  *  */

impl Error for StateBuilderError {}

impl fmt::Display for StateBuilderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::DuplicateSubstate(state_id) => {
                write!(f, "Substate '{}' is a duplicate of an existing Substate or the parent", state_id)
            },
            Self::DuplicateTransition(transition_id) => {
                write!(f, "Transition '{:?}' is a duplicate of an existing Transition", transition_id)
            },
            Self::TransitionSourceMismatch(transition_id, state_id) => {
                write!(f, "Transition '{:?}' source State '{}' does not match parent State", transition_id, state_id)
            },
            Self::InitialIsNotChild(initial_id) => {
                write!(f, "Initial ID '{}' does not match any child State IDs", initial_id)
            },
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


    #[test]
    fn failed_on_exit() -> Result<(), Box<dyn Error>> {
        //TODO: Extraneous clones
        // Define Event and State IDs
        let initial_to_terminal = Event::from("initial_to_terminal")?;
        let initial_state_id = String::from("INITIAL");
        let terminal_state_id = String::from("TERMINAL");

        // Build Transition
        let hapless_transition = TransitionBuilder::new(initial_state_id.clone())
            .event(initial_to_terminal.clone())?
            .target_id(terminal_state_id.clone())?
            .build();

        // Build States, one of which will fail its 2nd on_exit callback
        let initial = StateBuilder::new(initial_state_id)
            .transition(hapless_transition)?
            .on_exit(|| {Ok(())})
            .on_exit(|| {Err(())})
            .build()?;
        let terminal = StateBuilder::new(terminal_state_id).build()?;
        
        // Build the StateChart and process the Event
        let mut statechart = StateChartBuilder::default()
            .initial(initial.id())
            .state(initial)?
            .state(terminal)?
            .build().unwrap();
        
        assert_eq!(
            statechart.process_external_event(&initial_to_terminal),
            Err(StateChartError::StateError(StateError::FailedCallback(1))),
            "Failed to catch a failed on_exit callback"
        );

        Ok(())
    }

    
    #[test]
    fn failed_on_entry() -> Result<(), Box<dyn Error>> {
        // Define Event and State IDs
        let initial_to_terminal = Event::from("initial_to_terminal")?;
        let initial_state_id = String::from("INITIAL");
        let terminal_state_id = String::from("TERMINAL");

        // Build Transition
        let hapless_transition = TransitionBuilder::new(initial_state_id.clone())
            .event(initial_to_terminal.clone())?
            .target_id(terminal_state_id.clone())?
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
        let mut statechart = StateChartBuilder::default()
            .initial(initial.id())
            .state(initial)?
            .state(terminal)?
            .build().unwrap();
        
        assert_eq!(
            statechart.process_external_event(&initial_to_terminal),
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
    fn duplicate_transition() -> Result<(), Box<dyn Error>> {
        let state_id = String::from("source");

        // Build Transitions to be duplicated
        //TODO: extraneous clone()s
        let original_transition = TransitionBuilder::new(state_id.clone()).build();
        let duplicate_transition = TransitionBuilder::new( state_id.clone()).build();

        // Verify the duplicate transition is caught
        let builder = StateBuilder::new(state_id)
            .transition(original_transition.clone())?;

        assert_eq!(
            builder.transition(duplicate_transition),
            Err(StateBuilderError::DuplicateTransition(original_transition.id())),
            "Failed to catch duplicate Transition"
        );

        Ok(())
    }

    #[test]
    fn source_mismatch() -> Result<(), Box<dyn Error>> {
        //TODO: Extraneous clones
        let correct_source_id = String::from("source");
        let wrong_source_id = String::from("wrong");

        // Build Transition with an incorrect source
        let transition = TransitionBuilder::new(wrong_source_id.clone()).build();

        // Verify mismatch is caught
        let builder = StateBuilder::new(correct_source_id)
            .transition(transition.clone())?;
        
        assert_eq!(
            builder.build(),
            Err(StateBuilderError::TransitionSourceMismatch(transition.id(), wrong_source_id)),
            "Failed to catch source State mismatch"
        );

        Ok(())
    }

    #[test]
    fn duplicate_substate() -> Result<(), Box<dyn Error>> {
        //TODO: Extraneous clones()
        // Verify that duplicates of parent state are caught
        let parent_id  = String::from("parent");
        let parent_dup_state = StateBuilder::new(parent_id.clone()).build()?;

        let parent_dup_builder = StateBuilder::new(parent_id.clone());

        assert_eq!(
            parent_dup_builder.substate(parent_dup_state),
            Err(StateBuilderError::DuplicateSubstate(parent_id.clone())),
            "Failed to catch substate duplicate of parent"
        );

        // Verify that duplicate substates are caught
        let duplicate_id = String::from("duplicate");
        let dup_state_a = StateBuilder::new(duplicate_id.clone()).build()?;
        let dup_state_b = StateBuilder::new(duplicate_id.clone()).build()?;

        let substate_dup_builder = StateBuilder::new(parent_id)
            .substate(dup_state_a)?;

        assert_eq!(
            substate_dup_builder.substate(dup_state_b),
            Err(StateBuilderError::DuplicateSubstate(duplicate_id)),
            "Failed to catch substate duplicate of existing substate"
        );

        Ok(())
    }

    #[test]
    fn initial_not_child() -> Result<(), Box<dyn Error>> {
        //TODO: Extraneous clones()
        let nonchild_id = String::from("nonchild");

        let child = StateBuilder::new(String::from("child")).build()?;

        let builder = StateBuilder::new(String::from("parent"))
            .substate(child)?
            .initial(nonchild_id.clone());

        assert_eq!(
            builder.build(),
            Err(StateBuilderError::InitialIsNotChild(nonchild_id)),
            "Failed to catch initial ID that matched no children"
        );

        Ok(())
    }
}
