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
    //TODO: Purpose statement

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
    error::Error,
    fmt,
};

use crate::{
    datamodel::SystemVariables,
    interpreter::{
        Interpreter,
        InterpreterError,
    },
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, PartialEq)]
pub enum ExecutableContent {
    Assign(
        String, /* Identifier ('location' in SCXML parlance ) */
        String, /* Value expression string */
    ),
    Cancel,     /* Unimplemented */
    ElseIf,     /* Unimplemented */
    Else,       /* Unimplemented */
    ForEach,    /* Unimplemented */
    If,         /* Unimplemented */
    Log,        /* Unimplemented */
    Raise,      /* Unimplemented */
    Script,     /* Unimplemented */
    Send,       /* Unimplemented */
}

#[derive(Debug, PartialEq)]
pub enum ExecutableContentError {
    // Wrappers
    InterpreterError(InterpreterError),
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl ExecutableContent {
    pub fn execute(&self, sys_vars: &mut SystemVariables) -> Result<(), ExecutableContentError> {
        match self {
            Self::Assign(location, expr) => {
                // Interpret the expression to get the value to associate with the identifier
                let interpreter = Interpreter::new(expr);
                let value = interpreter.interpret(sys_vars)?;

                // Set the value in the data model
                sys_vars.set_data_member(location.clone(), value);

                Ok(())
            },
            _ => unimplemented!(),
        }
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
            Self::InterpreterError(interp_err) => {
                write!(f, "InterpreterError '{}' encountered within Executable Content", interp_err)
            },
        }
    }
}

impl From<InterpreterError> for ExecutableContentError {
    fn from(src: InterpreterError) -> Self {
        Self::InterpreterError(src)
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use std::error::Error;

    use crate::{
        datamodel::SystemVariables,
        executable_content::ExecutableContent,
        interpreter::EcmaScriptValue,
    };


    type TestResult = Result<(), Box<dyn Error>>;


    #[test]
    fn basic_assignment() -> TestResult {
        let location = "loc".to_string();
        let initial_val = EcmaScriptValue::Boolean(false);
        let expr = "true".to_string();

        // Create a basic assignment statement and valid data model for it to act on
        let assignment = ExecutableContent::Assign(location.clone(), expr.clone());
        let mut sys_vars = SystemVariables::default();
        sys_vars.set_data_member(location.clone(), initial_val);

        // Display initial conditions
        eprintln!("=== Initial Data Model:\n{:?}", sys_vars._x());
        
        // Execute the assignment
        assignment.execute(&mut sys_vars)?;

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

        // Create a meta assignment statement and valid data model for it to act on
        let assignment = ExecutableContent::Assign(location.clone(), expr.clone());
        let mut sys_vars = SystemVariables::default();
        sys_vars.set_data_member(location.clone(), initial_val.clone());

        // Display initial conditions
        eprintln!("=== Initial Data Model:\n{:?}", sys_vars._x());
        
        // Execute the assignment several times
        let executions = 4;
        for _ in 0..executions {
            assignment.execute(&mut sys_vars)?;
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
