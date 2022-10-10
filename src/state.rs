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

use std::collections::VecDeque;
use std::io::Write;
use std::{error::Error, fmt};

use crate::{
    datamodel::SystemVariables,
    event::Event,
    executable_content::{ExecutableContent, ExecutableContentError},
    transition::{Transition, TransitionError, TransitionFingerprint},
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Represents a state within the statechart.
///
/// May contain one or more of: initial states, transitions, entry/exit callbacks, substates
#[derive(Clone, Default, PartialEq)]
pub struct State {
    id: StateId,
    substates: Vec<State>,
    is_active: bool,
    initial_id: Option<StateId>,
    transitions: Vec<Transition>,
    onentry: Vec<ExecutableContent>,
    onexit: Vec<ExecutableContent>,
}

//OPT: *STYLE* Create StateIdRef type that is an alias for &str
//             Search for all instances of &str to find things to replace with this type
pub type StateId = String;

#[derive(Debug, PartialEq)]
pub enum StateError {
    SubstatesSpecifiedWithoutInitial,

    // Wrappers
    TransitionError(TransitionError),
    ExecutableContentError(ExecutableContentError),
}


#[derive(Debug, PartialEq)]
pub struct StateBuilder {
    id: StateId,
    substates: Vec<State>,
    is_active: bool,
    initial_id: Option<StateId>,
    transitions: Vec<Transition>,
    onentry: Vec<ExecutableContent>,
    onexit: Vec<ExecutableContent>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StateBuilderError {
    DuplicateSubstate(StateId),
    DuplicateTransition(TransitionFingerprint),
    InitialIsNotChild(StateId),
    InitialSetWithoutChildStates(StateId),
    TransitionSourceMismatch(TransitionFingerprint, StateId),
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl State {
    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    pub fn id(&self) -> &str {
        &self.id
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

    pub fn enter<W>(
        &mut self,
        sys_vars: &mut SystemVariables,
        internal_queue: &mut VecDeque<Event>,
        writer: &mut W,
    ) -> Result<(), StateError>
    where
        W: Write,
    {
        // Execute any on_enter callbacks
        self.execute_onentry(sys_vars, internal_queue, writer)?;

        // Activate the State
        self.activate();

        // If substates exist, enter the initial substate
        for substate in &mut self.substates {
            if let Some(initial_id) = &self.initial_id {
                if substate.id() == initial_id {
                    substate.enter(sys_vars, internal_queue, writer)?;
                }
            } else {
                return Err(StateError::SubstatesSpecifiedWithoutInitial);
            }
        }

        Ok(())
    }

    pub fn exit<W>(
        &mut self,
        sys_vars: &mut SystemVariables,
        internal_queue: &mut VecDeque<Event>,
        writer: &mut W,
    ) -> Result<(), StateError>
    where
        W: Write,
    {
        // If substates exist, exit the active substate(s)
        for substate in &mut self.substates {
            if substate.is_active() {
                substate.exit(sys_vars, internal_queue, writer)?;
            }
        }

        // Execute any onexit callbacks
        self.execute_onexit(sys_vars, internal_queue, writer)?;

        // Deactivate the State
        self.deactivate();

        Ok(())
    }

    pub fn evaluate_event(
        &self,
        event: Option<&Event>,
        sys_vars: &SystemVariables,
    ) -> Result<Option<&TransitionFingerprint>, StateError> {
        let mut enable_candidates = Vec::new();

        // Handle evaluation of a non-null Event
        if let Some(event) = &event {
            // Check for Event match in each of this State's Transitions
            for transition in &self.transitions {
                for transition_event in transition.events() {
                    if &transition_event == event {
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
                return Ok(Some(candidate.fingerprint()));
            }
        }

        // Either no candidates identified, or none passed their guard condition
        Ok(None)
    }


    /*  *  *  *  *  *  *  *\
     *  Helper Methods    *
    \*  *  *  *  *  *  *  */

    fn execute_onentry<W>(
        &self,
        sys_vars: &mut SystemVariables,
        internal_queue: &mut VecDeque<Event>,
        writer: &mut W,
    ) -> Result<(), StateError>
    where
        W: Write,
    {
        for exec_content in &self.onentry {
            exec_content.execute(sys_vars, internal_queue, writer)?;
        }

        Ok(())
    }

    fn execute_onexit<W>(
        &self,
        sys_vars: &mut SystemVariables,
        internal_queue: &mut VecDeque<Event>,
        writer: &mut W,
    ) -> Result<(), StateError>
    where
        W: Write,
    {
        for exec_content in &self.onexit {
            exec_content.execute(sys_vars, internal_queue, writer)?;
        }

        Ok(())
    }
}


impl StateBuilder {
    pub fn new(id: StateId) -> Self {
        Self {
            id,
            is_active: false,
            substates: Vec::new(),
            initial_id: None,
            transitions: Vec::new(),
            onentry: Vec::new(),
            onexit: Vec::new(),
        }
    }


    /*  *  *  *  *  *  *  *\
     *  Builder Methods   *
    \*  *  *  *  *  *  *  */

    pub fn build(mut self) -> Result<State, StateBuilderError> {
        // No-child `initial` sanity check
        if let Some(initial_id) = self
            .initial_id
            .as_ref()
            .filter(|_| self.substates.is_empty())
        {
            return Err(StateBuilderError::InitialSetWithoutChildStates(
                initial_id.clone(),
            ));
        }

        // Sanity checks for child states, if at least one exists
        if let Some(first_child) = self.substates.first() {
            // Ensure that initial ID matches a child State
            if let Some(initial_id) = self.initial_id.as_ref() {
                let mut child_matched = false;
                for substate in &self.substates {
                    if substate.id() == initial_id {
                        child_matched = true;
                    }
                }
                if !child_matched {
                    return Err(StateBuilderError::InitialIsNotChild(initial_id.clone()));
                }
            } else {
                // If no initial ID was provided, set to first doc-order child
                self.initial_id = Some(first_child.id().to_string());
            }
        }

        // Ensure all Transitions' source States match this one
        for transition in &self.transitions {
            if transition.source_id() != &self.id {
                return Err(StateBuilderError::TransitionSourceMismatch(
                    transition.fingerprint().clone(),
                    transition.source_id().clone(),
                ));
            }
        }

        Ok(State {
            id: self.id,
            substates: self.substates,
            is_active: self.is_active,
            initial_id: self.initial_id,
            transitions: self.transitions,
            onentry: self.onentry,
            onexit: self.onexit,
        })
    }

    pub fn substate(mut self, state: State) -> Result<Self, StateBuilderError> {
        // Ensure the substate is not a duplicate
        if state.id() == self.id {
            return Err(StateBuilderError::DuplicateSubstate(state.id().to_string()));
        }
        for substate in &self.substates {
            if state.id() == substate.id() {
                return Err(StateBuilderError::DuplicateSubstate(state.id().to_string()));
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
            return Err(StateBuilderError::DuplicateTransition(
                transition.fingerprint().clone(),
            ));
        }

        self.transitions.push(transition);

        Ok(self)
    }

    pub fn onentry(mut self, exec_content: ExecutableContent) -> Self {
        self.onentry.push(exec_content);

        self
    }

    pub fn onexit(mut self, exec_content: ExecutableContent) -> Self {
        self.onexit.push(exec_content);

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
            .field("id", &self.id)
            .field("substates", &self.substates)
            .field("is_active", &self.is_active)
            .field("initial_id", &self.initial_id)
            .field("transitions", &self.transitions)
            .field("onentry", &self.onentry)
            .field("onexit", &self.onexit)
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
            Self::SubstatesSpecifiedWithoutInitial => {
                write!(f, "State has substates but no Initial State")
            }

            // Wrappers
            Self::TransitionError(trans_err) => {
                write!(
                    f,
                    "TransitionError '{:?}' encountered while processing State",
                    trans_err
                )
            }
            Self::ExecutableContentError(exec_err) => {
                write!(
                    f,
                    "ExecutableContentError '{:?}' encountered while processing State",
                    exec_err
                )
            }
        }
    }
}

impl From<TransitionError> for StateError {
    fn from(src: TransitionError) -> Self {
        Self::TransitionError(src)
    }
}

impl From<ExecutableContentError> for StateError {
    fn from(src: ExecutableContentError) -> Self {
        Self::ExecutableContentError(src)
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
                write!(
                    f,
                    "Substate '{}' is a duplicate of an existing Substate or the parent",
                    state_id
                )
            }
            Self::DuplicateTransition(transition_id) => {
                write!(
                    f,
                    "Transition '{:?}' is a duplicate of an existing Transition",
                    transition_id
                )
            }
            Self::InitialIsNotChild(initial_id) => {
                write!(
                    f,
                    "Initial ID '{}' does not match any child State IDs",
                    initial_id
                )
            }
            Self::InitialSetWithoutChildStates(initial_id) => {
                write!(
                    f,
                    "Initial ID '{}' was set despite no child States existing",
                    initial_id
                )
            }
            Self::TransitionSourceMismatch(transition_id, state_id) => {
                write!(
                    f,
                    "Transition '{:?}' source State '{}' does not match parent State",
                    transition_id, state_id
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

    use std::{collections::VecDeque, error::Error};

    use crate::{executable_content::ExecutableContent, state::StateBuilder, SystemVariables};


    type TestResult = Result<(), Box<dyn Error>>;


    #[test]
    fn failed_onexit() -> TestResult {
        // Create dummy params
        let mut sys_vars = SystemVariables::default();
        let mut internal_queue = VecDeque::new();
        let mut buffer = Vec::new();

        // Build State, which will fail its 2nd onexit callback
        let mut terminal = StateBuilder::new("terminal".to_string())
            .onexit(ExecutableContent::Log(
                String::default(),
                "'VALID'".to_string(),
            ))
            .onexit(ExecutableContent::Assign(
                "test".to_string(),
                "'unterm".to_string(),
            ))
            .build()?;

        // Enter the State and verify the onexit fails as expected
        assert_eq!(
            terminal
                .exit(&mut sys_vars, &mut internal_queue, &mut buffer)
                .is_err(),
            true,
            "Failed to catch a failed onexit callback"
        );
        assert_eq!(String::from_utf8(buffer)?, "VALID\n".to_string(),);

        Ok(())
    }


    #[test]
    fn failed_onentry() -> TestResult {
        // Create dummy params
        let mut sys_vars = SystemVariables::default();
        let mut internal_queue = VecDeque::new();
        let mut buffer = Vec::new();

        // Build State, which will fail its 2nd onentry callback
        let mut terminal = StateBuilder::new("terminal".to_string())
            .onentry(ExecutableContent::Log(
                String::default(),
                "'VALID'".to_string(),
            ))
            .onentry(ExecutableContent::Assign(
                "test".to_string(),
                "'unterm".to_string(),
            ))
            .build()?;

        // Enter the State and verify the onentry fails as expected
        assert_eq!(
            terminal
                .enter(&mut sys_vars, &mut internal_queue, &mut buffer)
                .is_err(),
            true,
            "Failed to catch a failed onentry callback"
        );
        assert_eq!(String::from_utf8(buffer)?, "VALID\n".to_string(),);

        Ok(())
    }
}

#[cfg(test)]
mod builder_tests {

    use std::error::Error;

    use crate::{
        state::{StateBuilder, StateBuilderError},
        transition::TransitionBuilder,
    };

    type TestResult = Result<(), Box<dyn Error>>;

    #[test]
    fn duplicate_transition() -> TestResult {
        let state_id = String::from("source");

        // Build Transitions to be duplicated
        let original_transition = TransitionBuilder::new(state_id.as_str())
            .target_id("dummy_target")?
            .build()?;
        let duplicate_transition = TransitionBuilder::new(state_id.as_str())
            .target_id("dummy_target")?
            .build()?;

        // Verify the duplicate transition is caught
        let builder = StateBuilder::new(state_id).transition(original_transition.clone())?;

        assert_eq!(
            builder.transition(duplicate_transition),
            Err(StateBuilderError::DuplicateTransition(
                original_transition.fingerprint().clone()
            )),
            "Failed to catch duplicate Transition"
        );

        Ok(())
    }

    #[test]
    fn duplicate_substate() -> TestResult {
        // Verify that duplicates of parent state are caught
        let parent_id = String::from("parent");
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

        let substate_dup_builder = StateBuilder::new(parent_id).substate(dup_state_a)?;

        assert_eq!(
            substate_dup_builder.substate(dup_state_b),
            Err(StateBuilderError::DuplicateSubstate(duplicate_id)),
            "Failed to catch substate duplicate of existing substate"
        );

        Ok(())
    }

    #[test]
    fn initial_not_child() -> TestResult {
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

    #[test]
    fn initial_set_without_child_states() -> TestResult {
        let builder = StateBuilder::new("childless".to_string()).initial("nonexistent".to_string());

        assert_eq!(
            builder.build(),
            Err(StateBuilderError::InitialSetWithoutChildStates(
                "nonexistent".to_string()
            ))
        );

        Ok(())
    }

    #[test]
    fn source_mismatch() -> TestResult {
        let correct_source_id = String::from("source");
        let wrong_source_id = String::from("wrong");

        // Build Transition with an incorrect source
        let transition = TransitionBuilder::new(wrong_source_id.as_str())
            .target_id("dummy_target")?
            .build()?;

        // Verify mismatch is caught
        let builder = StateBuilder::new(correct_source_id).transition(transition.clone())?;

        assert_eq!(
            builder.build(),
            Err(StateBuilderError::TransitionSourceMismatch(
                transition.fingerprint().clone(),
                wrong_source_id
            )),
            "Failed to catch source State mismatch"
        );

        Ok(())
    }
}
