/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : executable_content.rs

Copyright (C) 2021 CJ McAllister
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
    This module defines a data structure and methods to record actions to be
    performed before/during/after Transitions.

    Conforms to §4 Executable Content of the SCXML spec.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::collections::VecDeque;
use std::io::{self, Write};
use std::{error::Error, fmt};

use crate::{
    datamodel::SystemVariables,
    event::Event,
    interpreter::{Interpreter, InterpreterError},
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, PartialEq)]
pub enum ExecutableContent {
    Assign(
        String, /* Identifier ('location' in SCXML parlance) */
        String, /* Value expression string */
    ),
    Cancel,  /* FEAT: <cancel> */
    ForEach, /* FEAT: <foreach> */
    BranchTable(
        Vec<BranchTableEntry>, /* Table of if-elseif-else branches */
    ),
    Log(
        String, /* Label */
        String, /* Value expression string */
    ),
    Raise(Event /* Event to raise */),
    Script, /* FEAT: <script> */
    Send,   /* FEAT: <send> */
}

#[derive(Clone, Debug, PartialEq)]
pub struct BranchTableEntry {
    cond: String,                 /* Conditional expression string */
    exec: Vec<ExecutableContent>, /* Sub-Executable Content */
}

#[derive(Debug, PartialEq)]
pub enum ExecutableContentError {
    // Wrappers
    InterpreterError(InterpreterError),
    IoError(io::ErrorKind),
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl ExecutableContent {
    pub fn execute<W>(
        &self,
        sys_vars: &mut SystemVariables,
        internal_queue: &mut VecDeque<Event>,
        writer: &mut W,
    ) -> Result<(), ExecutableContentError>
    where
        W: Write,
    {
        match self {
            Self::Assign(location, expr) => Self::execute_assign(location, expr, sys_vars),
            Self::BranchTable(branch_table) => {
                Self::execute_if(branch_table, sys_vars, internal_queue, writer)
            }
            Self::Log(label, expr) => Self::execute_log(label, expr, sys_vars, writer),
            Self::Raise(event) => Ok(Self::execute_raise(event, internal_queue)),
            _ => todo!(
                "Attempted to execute unimplemented ExecutableContent '{:?}'",
                self
            ),
        }
    }


    /*  *  *  *  *  *  *  *\
     *   Helper Methods   *
    \*  *  *  *  *  *  *  */

    fn execute_assign(
        location: &str,
        expr: &str,
        sys_vars: &mut SystemVariables,
    ) -> Result<(), ExecutableContentError> {
        // Interpret the expression to get the value to associate with the identifier
        let interpreter = Interpreter::new(expr);
        let value = interpreter.interpret(sys_vars)?;

        // Set the value in the data model
        sys_vars.set_data_member(location, value);

        Ok(())
    }

    fn execute_if<W>(
        branch_table: &[BranchTableEntry],
        sys_vars: &mut SystemVariables,
        internal_queue: &mut VecDeque<Event>,
        writer: &mut W,
    ) -> Result<(), ExecutableContentError>
    where
        W: Write,
    {
        // Iterate through the branch table, which is in document order, and execute the first entry whose
        // cond evaluates to 'true'
        for branch in branch_table {
            if Interpreter::new(branch.cond()).interpret_as_bool(sys_vars)? {
                // Branch passed, execute its content in order and break loop
                for exec_content_element in branch.exec_content() {
                    exec_content_element.execute(sys_vars, internal_queue, writer)?;
                }

                break;
            }
        }

        Ok(())
    }

    fn execute_log<W>(
        label: &str,
        expr: &str,
        sys_vars: &mut SystemVariables,
        writer: &mut W,
    ) -> Result<(), ExecutableContentError>
    where
        W: Write,
    {
        let mut has_content = false;

        // Interpret the expression to get the result to be logged
        let interpreter = Interpreter::new(expr);
        let value = interpreter.interpret(sys_vars)?;

        // Output the components of a log message if they exist
        if !label.is_empty() {
            write!(writer, "{}: ", label)?;
            has_content = true;
        }
        if !expr.is_empty() {
            write!(writer, "{}", value)?;
            has_content = true;
        }

        // Output a newline if anything was output
        if has_content {
            writeln!(writer)?;
        }

        Ok(())
    }

    fn execute_raise(event: &Event, internal_queue: &mut VecDeque<Event>) {
        // Push clone of event onto the back of the internal queue
        internal_queue.push_back(event.clone());
    }
}


impl BranchTableEntry {
    pub fn new(cond: String, exec: Vec<ExecutableContent>) -> Self {
        Self { cond, exec }
    }

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    pub fn cond(&self) -> &String {
        &self.cond
    }

    pub fn exec_content(&self) -> &Vec<ExecutableContent> {
        &self.exec
    }

    /*  *  *  *  *  *  *  *\
     *  Mutator Methods   *
    \*  *  *  *  *  *  *  */

    pub fn set_cond(&mut self, cond: String) {
        self.cond = cond;
    }

    pub fn push_exec_content(&mut self, exec: ExecutableContent) {
        self.exec.push(exec);
    }
}

///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *  *  *\
 *  ExecutableContentError  *
\*  *  *  *  *  *  *  *  *  */

impl Error for ExecutableContentError {}

impl fmt::Display for ExecutableContentError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // Wrappers
            Self::InterpreterError(interp_err) => {
                write!(
                    f,
                    "InterpreterError '{:?}' encountered within Executable Content",
                    interp_err
                )
            }
            Self::IoError(io_err) => {
                write!(
                    f,
                    "IO Error '{:?}' encountered within Executable Content",
                    io_err
                )
            }
        }
    }
}

impl From<InterpreterError> for ExecutableContentError {
    fn from(src: InterpreterError) -> Self {
        Self::InterpreterError(src)
    }
}

impl From<std::io::Error> for ExecutableContentError {
    fn from(src: std::io::Error) -> Self {
        Self::IoError(src.kind())
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;
    use std::error::Error;
    use std::io;

    use crate::{
        datamodel::SystemVariables, executable_content::ExecutableContent,
        interpreter::EcmaScriptValue,
    };


    type TestResult = Result<(), Box<dyn Error>>;

    mod assign {

        use super::*;

        #[test]
        fn basic_assignment() -> TestResult {
            let location = "loc".to_string();
            let initial_val = EcmaScriptValue::Boolean(false);
            let expr = "true".to_string();
            let mut internal_queue = VecDeque::new();

            // Create a basic assignment statement and valid data model for it to act on
            let assignment = ExecutableContent::Assign(location.clone(), expr.clone());
            let mut sys_vars = SystemVariables::default();
            sys_vars.set_data_member(&location, initial_val);

            // Display initial conditions
            eprintln!("=== Initial Data Model:\n{:?}", sys_vars._x());

            // Execute the assignment
            assignment.execute(&mut sys_vars, &mut internal_queue, &mut io::stdout())?;

            // Display final conditions
            eprintln!("=== Final Data Model:\n{:?}", sys_vars._x());

            // Verify successful assignment
            assert_eq!(
                sys_vars.get_data_member(&location),
                Some(&EcmaScriptValue::Boolean(true))
            );

            Ok(())
        }

        #[test]
        fn meta_assignment() -> TestResult {
            let location = "loc".to_string();
            let initial_val = EcmaScriptValue::Number(0.0);
            let expr = "loc + 1".to_string();
            let mut internal_queue = VecDeque::new();

            // Create a meta assignment statement and valid data model for it to act on
            let assignment = ExecutableContent::Assign(location.clone(), expr.clone());
            let mut sys_vars = SystemVariables::default();
            sys_vars.set_data_member(&location, initial_val.clone());

            // Display initial conditions
            eprintln!("=== Initial Data Model:\n{:?}", sys_vars._x());

            // Execute the assignment several times
            let executions = 4;
            for _ in 0..executions {
                assignment.execute(&mut sys_vars, &mut internal_queue, &mut io::stdout())?;
            }

            // Display final conditions
            eprintln!("=== Final Data Model:\n{:?}", sys_vars._x());

            // Verify successful assignment
            assert_eq!(
                sys_vars.get_data_member(&location),
                Some(&(initial_val + EcmaScriptValue::Number(executions as f64)))
            );

            Ok(())
        }
    }

    mod log {
        use super::*;

        use crate::interpreter::Interpreter;

        #[test]
        fn basic_logging() -> TestResult {
            let label = "LABEL".to_string();
            let expr = "\'This is a log message.\'".to_string();
            let mut internal_queue = VecDeque::new();

            // Create a buffer to capture the log output
            let mut buffer = Vec::new();

            // Create a log action and a valid data model
            let log_action = ExecutableContent::Log(label.clone(), expr.clone());
            let mut sys_vars = SystemVariables::default();

            // Execute the log action
            log_action.execute(&mut sys_vars, &mut internal_queue, &mut buffer)?;

            // Capture output and verify
            let output = String::from_utf8(buffer)?;
            assert_eq!(
                output,
                format!(
                    "{}: {}\n",
                    label,
                    Interpreter::new(&expr).interpret(&sys_vars)?
                )
            );

            Ok(())
        }

        #[test]
        fn empty_logging() -> TestResult {
            let label = "".to_string();
            let expr = "".to_string();
            let mut internal_queue = VecDeque::new();

            // Create a buffer to capture the log output
            let mut buffer = Vec::new();

            // Create a log action and a valid data model
            let log_action = ExecutableContent::Log(label, expr);
            let mut sys_vars = SystemVariables::default();

            // Execute the log action
            log_action.execute(&mut sys_vars, &mut internal_queue, &mut buffer)?;

            // Capture output and verify
            let output = String::from_utf8(buffer)?;
            assert_eq!(output, String::default());

            Ok(())
        }

        #[test]
        fn parsed_logging() -> TestResult {
            let label = "RESULT".to_string();
            let expr = "5.5 + 3".to_string();
            let mut internal_queue = VecDeque::new();

            // Create a buffer to capture the log output
            let mut buffer = Vec::new();

            // Create a log action and a valid data model
            let log_action = ExecutableContent::Log(label.clone(), expr.clone());
            let mut sys_vars = SystemVariables::default();

            // Execute the log action
            log_action.execute(&mut sys_vars, &mut internal_queue, &mut buffer)?;

            // Capture output and verify
            let output = String::from_utf8(buffer)?;
            assert_eq!(
                output,
                format!(
                    "{}: {}\n",
                    label,
                    Interpreter::new(&expr).interpret(&sys_vars)?
                )
            );

            Ok(())
        }
    }
}
