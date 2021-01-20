/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : condition.rs

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
    This module defines a Condition object, which represents the parsed boolean
    expression of the 'cond' attribute of a Transition.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
    error::Error,
    fmt,
};

use crate::datamodel::SystemVariables;


///////////////////////////////////////////////////////////////////////////////
//  Named Constant
///////////////////////////////////////////////////////////////////////////////

//OPT: *DESIGN* This should probably be an enum for better future-proofing
//FEAT: Handle _event, _name, In(), etc.
const RESERVED_OPERANDS: [&str; 2] = ["true", "false"];


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

#[derive(Clone, PartialEq)]
pub struct Condition {
    operand_a: String,
    operand_b: Option<String>,
    operation: Option<Operation>,
}

#[derive(Debug, PartialEq)]
pub enum ConditionError {
    InvalidArgumentCount,
    InvalidOperation(String),
}

#[derive(Clone, Debug, PartialEq)]
enum Operation {
    GreaterThan,
    LessThan,
    EqualTo,
    NotEqualTo,
    GreaterThanOrEqualTo,
    LessThanOrEqualTo,
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Condition {
    pub fn new(cond_str: &str) -> Result<Self, ConditionError>  {
        // Split on whitespace
        let mut split_str = cond_str.split_ascii_whitespace();

        // Parse first operand
        let operand_a;
        if let Some(operand_str) = split_str.next() {
            operand_a = String::from(operand_str);
        }
        else {
            return Err(ConditionError::InvalidArgumentCount);
        }

        // Determine if this is a singlet, or more
        if split_str.clone().count() > 1 {
            let operation;
            if let Some(operation_str) = split_str.next() {
                operation = match operation_str {
                    ">" =>  Operation::GreaterThan,
                    "<" =>  Operation::LessThan,
                    "==" => Operation::EqualTo,
                    "!=" => Operation::NotEqualTo,
                    ">=" => Operation::GreaterThanOrEqualTo,
                    "<=" => Operation::LessThanOrEqualTo,
                    _ =>    return Err(ConditionError::InvalidOperation(String::from(operation_str))),
                }
            }
            else {
                return Err(ConditionError::InvalidArgumentCount);
            }
    
            let operand_b;
            if let Some(operand_str) = split_str.next() {
                operand_b = String::from(operand_str);
            }
            else {
                return Err(ConditionError::InvalidArgumentCount);
            }
    
            // Ensure there are no additional arguments
            if let Some(_str) = split_str.next() {
                return Err(ConditionError::InvalidArgumentCount);
            }
    
            Ok(
                Self {
                    operand_a,
                    operand_b: Some(operand_b),
                    operation: Some(operation),
                }
            )
        }
        else {
            Ok(
                Self {
                operand_a,
                operand_b: None,
                operation: None,
                }
            )
        }        
    }

    
    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */

    pub fn evaluate(&self, sys_vars: &SystemVariables) -> bool {
        // Handle based on operand count
        if self.operand_b.is_none() {
            self.evaluate_singlet(sys_vars)
        }
        else {
            self.evaluate_triplet(sys_vars)
        }        
    }

    
    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */

    fn evaluate_singlet(&self, sys_vars: &SystemVariables) -> bool {
        // Look up the operands in the System Variables map,
        // if it's not a reserved string
        if RESERVED_OPERANDS.contains(&self.operand_a.as_str()) {
            //OPT: *DESIGN* See comment on RESERVED_OPERANDS
            self.operand_a == "true"
        }
        else {
            // Not a reserved keyword, check against "1"
            sys_vars._x().get(&self.operand_a).unwrap() == &1
        }
    }

    fn evaluate_triplet(&self, sys_vars: &SystemVariables) -> bool {
        // Look up the operands in the System Variables map,
        // if it's not a reserved string
        let value_a;
        if RESERVED_OPERANDS.contains(&self.operand_a.as_str()) {
            if self.operand_a == "true" {
                value_a = &1;
            }
            else if self.operand_a == "false" {
                value_a = &0;
            }
            else {
                //OPT: *STYLE* probably need an error here
                return false;
            }
        }
        else {
            value_a = sys_vars._x().get(&self.operand_a).unwrap();
        }

        let value_b;
        if let Some(operand_b) = &self.operand_b {
            if RESERVED_OPERANDS.contains(&operand_b.as_str()) {
                if operand_b == "true" {
                    value_b = &1;
                }
                else if operand_b == "false" {
                    value_b = &0;
                }
                else {
                    //OPT: *STYLE* probably need an error here
                    return false;
                }
            }
            else {
                value_b = sys_vars._x().get(operand_b).unwrap();
            }
        }
        else {
            //OPT: *STYLE* probably need an error here
            return false;
        }
        

        // Perform appropriate operation
        match self.operation {
            Some(Operation::GreaterThan) => {
                value_a > value_b
            },
            Some(Operation::LessThan) => {
                value_a < value_b
            },
            Some(Operation::EqualTo) => {
                value_a == value_b
            },
            Some(Operation::NotEqualTo) => {
                value_a != value_b
            },
            Some(Operation::GreaterThanOrEqualTo) => {
                value_a >= value_b
            },
            Some(Operation::LessThanOrEqualTo) => {
                value_a <= value_b
            },
            None => {
                //OPT: *STYLE* probably need an error here
                false
            }
        }
    }
    
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *     Condition      *
\*  *  *  *  *  *  *  */

impl fmt::Debug for Condition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let (Some(operation), Some(operand_b)) = (&self.operation, &self.operand_b) {
            write!(f, "{} {} {}", self.operand_a, operation, operand_b)
        }
        else {
            write!(f, "{}", self.operand_a)
        }
    }
}

impl Default for Condition {
    fn default() -> Self {
        Self {
            operand_a: String::from("true"),
            operand_b: None,
            operation: None,
        }
    }
}

impl ToString for Condition {
    fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}


/*  *  *  *  *  *  *  *\
 *  ConditionError    *
\*  *  *  *  *  *  *  */

impl Error for ConditionError {}

impl fmt::Display for ConditionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidArgumentCount => {
                write!(f, "Invalid number of arguments in Condition statement")
            },
            Self::InvalidOperation(op) => {
                write!(f, "Invalid operation '{}' in Condition statement", op)
            },
        }
    }
}


/*  *  *  *  *  *  *  *\
 *     Operation      *
\*  *  *  *  *  *  *  */

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operation::GreaterThan => {
                write!(f, ">")
            },
            Operation::LessThan => {
                write!(f, "<")
            },
            Operation::EqualTo => {
                write!(f, "==")
            },
            Operation::NotEqualTo => {
                write!(f, "!=")
            },
            Operation::GreaterThanOrEqualTo => {
                write!(f, ">=")
            },
            Operation::LessThanOrEqualTo => {
                write!(f, "<=")
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

    use crate::condition::{
        Condition,
        ConditionError,
    };


    #[test]
    fn invalid_arg_count() -> Result<(), Box<dyn Error>> {
        
        // Valid arg count
        assert_eq!(
            Condition::new("a > b").is_ok(),
            true
        );

        // Arg count too high
        assert_eq!(
            Condition::new("a > b > c"),
            Err(ConditionError::InvalidArgumentCount)
        );

        Ok(())
    }
}
