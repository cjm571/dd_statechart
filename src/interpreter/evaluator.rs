/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : interpreter/evaluator.rs

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
    any::Any,
    cmp::Ordering,
    error::Error,
    fmt,
    ops::{
        Add,
        Sub,
    },
};

use crate::interpreter::parser::{
    ArithmeticOperator,
    Expression,
    Literal,
    LogicalOperator,
    Operator,
    Unary,
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

pub struct Evaluator {
    expr: Expression,
}


#[derive(Debug, PartialEq)]
pub enum EvaluatorError {
    StringNegation(String),
    UnknownNegation(Box<Expression>),
    IllegalOperation(
        ArithmeticOperator, // Operator
        IntermediateValue,  // Left Operand
        IntermediateValue,  // Right Operand
    ),

    CouldNotDowncast,
}

//FIXME: PUT THIS SOMEWHERE BETTER
#[derive(Debug)]
pub enum IntermediateValue {
    String(String),
    Integer(i32),
    Float(f64),
    Boolean(bool),
    Null,
}

impl PartialEq for IntermediateValue {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::String(self_value) => {
                match other {
                    Self::String(other_value) => {
                        // Simple string-to-string comparison
                        self_value == other_value
                    },
                    Self::Integer(other_value) => {
                        // Attempt to parse String as i32 and compare values
                        if let Ok(parsed_value) = self_value.parse::<i32>() {
                            &parsed_value == other_value
                        }
                        else {
                            false
                        }
                    },
                    Self::Float(other_value) => {
                        // Attempt to parse String as f64 and compare values
                        if let Ok(parsed_value) = self_value.parse::<f64>() {
                            &parsed_value == other_value
                        }
                        else {
                            false
                        }
                    },
                    Self::Boolean(other_value_is_true) => {
                        // Attempt to parse String as i32 AND f64, as both
                        // 1 and 1.0  == true, and 0 and 0.0 == false
                        if let Ok(parsed_value) = self_value.parse::<i32>() {
                            if *other_value_is_true {
                                parsed_value == 1
                            }
                            else {
                                parsed_value == 0
                            }
                        }
                        else if let Ok(parsed_value) = self_value.parse::<f64>() {
                            if *other_value_is_true {
                                parsed_value == 1.0
                            }
                            else {
                                parsed_value == 0.0
                            }
                        }
                        else {
                            false
                        }
                    },
                    Self::Null => {
                        // All strings != 'null'
                        false
                    },
                }
            },
            Self::Integer(self_value) => {
                match other {
                    Self::String(other_value) => {
                        // Leverage existing comparison
                        Self::String(other_value.clone()) == *self
                    },
                    Self::Integer(other_value) => {
                        // Simple comparison
                        self_value == other_value
                    },
                    Self::Float(other_value) => {
                        // Casted comparison
                        *self_value as f64 == *other_value
                    },
                    Self::Boolean(other_value_is_true) => {
                        // Check against '1' and '0'
                        if *other_value_is_true {
                            self_value == &1
                        }
                        else {
                            self_value == &0
                        }
                    },
                    Self::Null => {
                        // All integers are != 'null'
                        false
                    },
                }
            },
            Self::Float(self_value) => {
                match other {
                    Self::String(other_value) => {
                        // Leverage existing comparison
                        Self::String(other_value.clone()) == *self
                    },
                    Self::Integer(other_value) => {
                        // Leverage existing comparison
                        Self::Integer(*other_value) == *self
                    },
                    Self::Float(other_value) => {
                        // Simple comparison
                        self_value == other_value
                    },
                    Self::Boolean(other_value_is_true) => {
                        // Check against '1.0' and '0.0'
                        if *other_value_is_true {
                            self_value == &1.0
                        }
                        else {
                            self_value == &0.0
                        }
                    },
                    Self::Null => {
                        // All floats are != 'null'
                        false
                    },
                }
            },
            Self::Boolean(self_value_is_true) => {
                match other {
                    Self::String(other_value) => {
                        // Leverage existing comparison
                        Self::String(other_value.clone()) == *self
                    },
                    Self::Integer(other_value) => {
                        // Leverage existing comparison
                        Self::Integer(*other_value) == *self
                    },
                    Self::Float(other_value) => {
                        // Leverage existing comparison
                        Self::Float(*other_value) == *self
                    },
                    Self::Boolean(other_value_is_true) => {
                        // Simple comparison
                        self_value_is_true == other_value_is_true
                    },
                    Self::Null => {
                        // All bools are != 'null'
                        false
                    },
                }
            },
            Self::Null => {
                // Leverage existing comparison
                Self::Null == *self
            },
        }
    }
}

impl PartialOrd for IntermediateValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            Self::String(self_value) => {
                match other {
                    Self::String(other_value) => {
                        // Compare strings in lexicographical order
                        for (self_char, other_char) in self_value.chars().zip(other_value.chars()) {
                            match self_char.cmp(&other_char) {
                                Ordering::Equal => continue,
                                non_equivalence => return Some(non_equivalence),
                            }
                        }

                        // Got to the end of both equal strings
                        Some(Ordering::Equal)
                    },
                    Self::Integer(other_value) => {
                        // Attempt to parse String as i32 and compare values
                        if let Ok(parsed_value) = self_value.parse::<i32>() {
                            parsed_value.partial_cmp(other_value)
                        }
                        else {
                            // Non-parseable strings are considered less than any integer
                            Some(Ordering::Less)
                        }
                    },
                    Self::Float(other_value) => {
                        // Attempt to parse String as f64 and compare values
                        if let Ok(parsed_value) = self_value.parse::<f64>() {
                            parsed_value.partial_cmp(other_value)
                        }
                        else {
                            // Non-parseable strings are considered less than any integer
                            Some(Ordering::Less)
                        }
                    },
                    Self::Boolean(_other_value_is_true) => {
                        // All strings are considered less than bools
                        Some(Ordering::Less)
                    },
                    Self::Null => {
                        //FIXME: Document this nonconformance
                        // ECMAScript does really fucking stupid shit with comparisons to 'null'
                        // I'm just gonna ban it
                        None
                    },
                }
            },
            Self::Integer(self_value) => {
                match other {
                    Self::String(other_value) => {
                        // Leverage existing comparison
                        Self::String(other_value.clone()).partial_cmp(self)
                    },
                    Self::Integer(other_value) => {
                        // Simple comparison
                        self_value.partial_cmp(other_value)
                    },
                    Self::Float(other_value) => {
                        // Casted comparison
                        (*self_value as f64).partial_cmp(other_value)
                    },
                    Self::Boolean(other_value_is_true) => {
                        // Treat 'true' as '1', and 'false' as '0'
                        if *other_value_is_true {
                            self_value.partial_cmp(&1)
                        }
                        else {
                            self_value.partial_cmp(&0)
                        }
                    },
                    Self::Null => {
                        //FIXME: Document this nonconformance
                        // ECMAScript does really fucking stupid shit with comparisons to 'null'
                        // I'm just gonna ban it
                        None
                    },
                }
            },
            Self::Float(self_value) => {
                match other {
                    Self::String(other_value) => {
                        // Leverage existing comparison
                        Self::String(other_value.clone()).partial_cmp(self)
                    },
                    Self::Integer(other_value) => {
                        // Leverage existing comparison
                        Self::Integer(*other_value).partial_cmp(self)
                    },
                    Self::Float(other_value) => {
                        // Simple comparison
                        self_value.partial_cmp(other_value)
                    },
                    Self::Boolean(other_value_is_true) => {
                        // Treat 'true' as '1.0', and 'false' as '0.0'
                        if *other_value_is_true {
                            self_value.partial_cmp(&1.0)
                        }
                        else {
                            self_value.partial_cmp(&0.0)
                        }
                    },
                    Self::Null => {
                        //FIXME: Document this nonconformance
                        // ECMAScript does really fucking stupid shit with comparisons to 'null'
                        // I'm just gonna ban it
                        None
                    },
                }
            },
            Self::Boolean(_self_value_is_true) => {
                match other {
                    Self::String(other_value) => {
                        // Leverage existing comparison
                        Self::String(other_value.clone()).partial_cmp(self)
                    },
                    Self::Integer(other_value) => {
                        // Leverage existing comparison
                        Self::Integer(*other_value).partial_cmp(self)
                    },
                    Self::Float(other_value) => {
                        // Leverage existing comparison
                        Self::Float(*other_value).partial_cmp(self)
                    },
                    Self::Boolean(other_value_is_true) => {
                        // Leverage existing comparison
                        Self::Boolean(*other_value_is_true).partial_cmp(self)
                    },
                    Self::Null => {
                        //FIXME: Document this nonconformance
                        // ECMAScript does really fucking stupid shit with comparisons to 'null'
                        // I'm just gonna ban it
                        None
                    },
                }
            },
            Self::Null => {
                //FIXME: Document this nonconformance
                // ECMAScript does really fucking stupid shit with comparisons to 'null'
                // I'm just gonna ban it
                None
            },
        }
    }
}

//FIXME: Strings that can be parsed into numbers should be added instead of concatenated
impl Add<Self> for IntermediateValue {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Self::String(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Concatenate
                        Self::String(self_value + &rhs_value)
                    },
                    Self::Integer(rhs_value) => {
                        // Concatenate with stringified i32
                        Self::String(self_value + &rhs_value.to_string())
                    },
                    Self::Float(rhs_value) => {
                        // Concatenate with stringified f64
                        Self::String(self_value + &rhs_value.to_string())
                    },
                    Self::Boolean(rhs_value) => {
                        // Concatenate with stringified bool
                        Self::String(self_value + &rhs_value.to_string())
                    },
                    Self::Null => {
                        // Concatenate with "null"
                        Self::String(self_value + "null")
                    },
                }
            },
            Self::Integer(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Leverage existing functionality
                        Self::String(rhs_value).add(self)
                    },
                    Self::Integer(rhs_value) => {
                        // Simple addition
                        Self::Integer(self_value + rhs_value)
                    },
                    Self::Float(rhs_value) => {
                        // Casted addition
                        Self::Float(self_value as f64 + rhs_value)
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1, 'false' as 0
                        Self::Integer(self_value + rhs_value as i32)
                    },
                    Self::Null => {
                        // Essentially a NOP
                        self
                    },
                }
            },
            Self::Float(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Leverage existing functionality
                        Self::String(rhs_value).add(self)
                    },
                    Self::Integer(rhs_value) => {
                        // Leverage existing functionality
                        Self::Integer(rhs_value).add(self)
                    },
                    Self::Float(rhs_value) => {
                        // Leverage existing functionality
                        Self::Float(rhs_value).add(self)
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1.0, 'false' as 0.0
                        Self::Float(self_value + rhs_value as i32 as f64)
                    },
                    Self::Null => {
                        // Essentially a NOP
                        self
                    },
                }
            },
            Self::Boolean(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Leverage existing functionality
                        Self::String(rhs_value).add(self)
                    },
                    Self::Integer(rhs_value) => {
                        // Leverage existing functionality
                        Self::Integer(rhs_value).add(self)
                    },
                    Self::Float(rhs_value) => {
                        // Leverage existing functionality
                        Self::Float(rhs_value).add(self)
                    },
                    Self::Boolean(rhs_value) => {
                        // Leverage existing functionality
                        Self::Boolean(rhs_value).add(self)
                    },
                    Self::Null => {
                        // Forces a conversion to an i32
                        Self::Integer(self_value as i32)
                    },
                }
            },
            Self::Null => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Leverage existing functionality
                        Self::String(rhs_value).add(self)
                    },
                    Self::Integer(rhs_value) => {
                        // Leverage existing functionality
                        Self::Integer(rhs_value).add(self)
                    },
                    Self::Float(rhs_value) => {
                        // Leverage existing functionality
                        Self::Float(rhs_value).add(self)
                    },
                    Self::Boolean(rhs_value) => {
                        // Leverage existing functionality
                        Self::Boolean(rhs_value).add(self)
                    },
                    Self::Null => {
                        // Evaluates to 0
                        Self::Integer(0)
                    },
                }
            },
        }
    }
}

impl Sub<Self> for IntermediateValue {
    type Output = Result<Self, EvaluatorError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Self::String(self_value) => {
                // Can only operate on strings that can be parsed into numbers
                if let Ok(parsed_self_value_i32) = self_value.parse::<i32>() {
                    match rhs {
                        Self::String(rhs_value) => {
                            // Can only operate on strings that can be parsed into numbers
                            if let Ok(parsed_rhs_value_i32) = rhs_value.parse::<i32>() {
                                // Simple subtraction
                                Ok(Self::Integer(parsed_self_value_i32 - parsed_rhs_value_i32))
                            }
                            else if let Ok(parsed_rhs_value_f64) = rhs_value.parse::<f64>() {
                                // Casted subtraction
                                Ok(Self::Float(parsed_self_value_i32 as f64 - parsed_rhs_value_f64))
                            }
                            else {
                                // Right operand could not be parsed into int or float, therefore subtraction is illegal
                                Err (
                                    EvaluatorError::IllegalOperation (
                                        ArithmeticOperator::Minus,
                                        Self::String(self_value),
                                        Self::String(rhs_value),
                                    )
                                )
                            }
                        },
                        Self::Integer(rhs_value) => {
                            // Simple subtraction
                            Ok(Self::Integer(parsed_self_value_i32 - rhs_value))
                        },
                        Self::Float(rhs_value) => {
                            // Casted subtraction
                            Ok(Self::Float(parsed_self_value_i32 as f64 - rhs_value))
                        },
                        Self::Boolean(rhs_value) => {
                            // Treat 'true' as 1, 'false' as 0
                            Ok(Self::Integer(parsed_self_value_i32 - rhs_value as i32))
                        },
                        Self::Null => {
                            // Forces conversion to number
                            Ok(Self::Integer(parsed_self_value_i32))
                        },
                    }
                }
                else if let Ok(parsed_self_value_f64) = self_value.parse::<f64>() {
                    match rhs {
                        Self::String(rhs_value) => {
                            // Can only operate on strings that can be parsed into numbers
                            if let Ok(parsed_rhs_value_i32) = rhs_value.parse::<i32>() {
                                // Casted subtraction
                                Ok(Self::Float(parsed_self_value_f64 - parsed_rhs_value_i32 as f64))
                            }
                            else if let Ok(parsed_rhs_value_f64) = rhs_value.parse::<f64>() {
                                // Simple subtraction
                                Ok(Self::Float(parsed_self_value_f64 - parsed_rhs_value_f64))
                            }
                            else {
                                // Right operand could not be parsed into int or float, therefore subtraction is illegal
                                Err (
                                    EvaluatorError::IllegalOperation (
                                        ArithmeticOperator::Minus,
                                        Self::String(self_value),
                                        Self::String(rhs_value),
                                    )
                                )
                            }
                        },
                        Self::Integer(rhs_value) => {
                            // Casted subtraction
                            Ok(Self::Float(parsed_self_value_f64 - rhs_value as f64))
                        },
                        Self::Float(rhs_value) => {
                            // Simple subtraction
                            Ok(Self::Float(parsed_self_value_f64 - rhs_value))
                        },
                        Self::Boolean(rhs_value) => {
                            // Treat 'true' as 1.0, 'false' as 0.0
                            Ok(Self::Float(parsed_self_value_f64 - rhs_value as i32 as f64))
                        },
                        Self::Null => {
                            // Forces conversion to number
                            Ok(Self::Float(parsed_self_value_f64))
                        },
                    }
                }
                else {
                    // Left operand could not be parsed into int or float, therefore subtraction is illegal
                    Err (
                        EvaluatorError::IllegalOperation (
                            ArithmeticOperator::Minus,
                            Self::String(self_value),
                            rhs,
                        )
                    )
                }
            },
            Self::Integer(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Can only operate on strings that can be parsed into numbers
                        if let Ok(parsed_value) = rhs_value.parse::<i32>() {
                            // Simple subtraction
                            Ok(Self::Integer(self_value - parsed_value))
                        }
                        else if let Ok(parsed_value) = rhs_value.parse::<f64>() {
                            // Casted subtraction
                            Ok(Self::Float(self_value as f64 - parsed_value))
                        }
                        else {
                            // Right operand could not be parsed into int or float, therefore subtraction is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Minus,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Integer(rhs_value) => {
                        // Simple subtraction
                        Ok(Self::Integer(self_value - rhs_value))
                    },
                    Self::Float(rhs_value) => {
                        // Casted subtraction
                        Ok(Self::Float(self_value as f64 - rhs_value))
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1, 'false' as 0
                        Ok(Self::Integer(self_value - rhs_value as i32))
                    },
                    Self::Null => {
                        // Essentially a NOP
                        Ok(self)
                    },
                }
            },
            Self::Float(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Can only operate on strings that can be parsed into numbers
                        if let Ok(parsed_value) = rhs_value.parse::<i32>() {
                            // Casted subtraction
                            Ok(Self::Float(self_value - parsed_value as f64))
                        }
                        else if let Ok(parsed_value) = rhs_value.parse::<f64>() {
                            // Simple subtraction
                            Ok(Self::Float(self_value - parsed_value))
                        }
                        else {
                            // Right operand could not be parsed into int or float, therefore subtraction is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Minus,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Integer(rhs_value) => {
                        // Casted subtraction
                        Ok(Self::Float(self_value - rhs_value as f64))
                    },
                    Self::Float(rhs_value) => {
                        // Simple subtraction
                        Ok(Self::Float(self_value - rhs_value))
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1.0, 'false' as 0.0
                        Ok(Self::Float(self_value - rhs_value as i32 as f64))
                    },
                    Self::Null => {
                        // Essentially a NOP
                        Ok(self)
                    },
                }
            },
            Self::Boolean(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Can only operate on strings that can be parsed into numbers
                        if let Ok(parsed_value) = rhs_value.parse::<i32>() {
                            // Treat 'true' as 1, 'false' as 0
                            Ok(Self::Integer(self_value as i32 - parsed_value))
                        }
                        else if let Ok(parsed_value) = rhs_value.parse::<f64>() {
                            // Treat 'true' as 1.0, 'false' as 0.0
                            Ok(Self::Float(self_value as i32 as f64 - parsed_value))
                        }
                        else {
                            // Right operand could not be parsed into int or float, therefore subtraction is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Minus,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Integer(rhs_value) => {
                        // Treat 'true' as 1, 'false' as 0
                        Ok(Self::Integer(self_value as i32 - rhs_value))
                    },
                    Self::Float(rhs_value) => {
                        // Treat 'true' as 1.0, 'false' as 0.0
                        Ok(Self::Float(self_value as i32 as f64 - rhs_value))
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1, 'false' as 0
                        Ok(Self::Integer(self_value as i32 - rhs_value as i32))
                    },
                    Self::Null => {
                        // Forces conversion to an i32
                        Ok(Self::Integer(self_value as i32))
                    },
                }
            },
            Self::Null => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Can only operate on strings that can be parsed into numbers
                        if let Ok(parsed_value) = rhs_value.parse::<i32>() {
                            // Essentially a negation
                            Ok(Self::Integer(-parsed_value))
                        }
                        else if let Ok(parsed_value) = rhs_value.parse::<f64>() {
                            // Essentially a negation
                            Ok(Self::Float(-parsed_value))
                        }
                        else {
                            // Right operand could not be parsed into int or float, therefore subtraction is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Minus,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Integer(rhs_value) => {
                        // Essentially a negation
                        Ok(Self::Integer(-rhs_value))
                    },
                    Self::Float(rhs_value) => {
                        // Essentially a negation
                        Ok(Self::Float(-rhs_value))
                    },
                    Self::Boolean(rhs_value) => {
                        // Convert to i32 and negate
                        Ok(Self::Integer(-(rhs_value as i32)))
                    },
                    Self::Null => {
                        // Evaluates to 0
                        Ok(Self::Integer(0))
                    },
                }
            },
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Evaluator {
    pub fn new(expr: Expression)  -> Self {
        Self { expr }
    }


    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */

    pub fn evaluate(&self) -> Result<Box<dyn Any>, EvaluatorError> {
        //OPT: *DESIGN* Bad clone()... Figure out how to use a reference correctly
        match Self::eval_expr(self.expr.clone())? {
            IntermediateValue::String(value)    => Ok(Box::new(value)),
            IntermediateValue::Integer(value)   => Ok(Box::new(value)),
            IntermediateValue::Float(value)     => Ok(Box::new(value)),
            IntermediateValue::Boolean(value)   => Ok(Box::new(value)),
            IntermediateValue::Null             => Ok(Box::new(IntermediateValue::Null)),
        }
    }

    
    /*  *  *  *  *  *  *  *\
     *   Helper Methods   *
    \*  *  *  *  *  *  *  */
    
    fn eval_expr(expr: Expression) -> Result<IntermediateValue, EvaluatorError> {
        match expr {
            Expression::Literal(literal)        => Ok(Self::eval_literal(literal)),
            Expression::Unary(unary)            => Ok(Self::eval_unary(unary)?),
            Expression::Binary(left, op, right) => Ok(Self::eval_binary(op, *left, *right)?),
            Expression::Grouping(inner_expr)    => Self::eval_expr(*inner_expr),
        }
    }

    fn eval_literal(literal: Literal) -> IntermediateValue {
        match literal {
            Literal::String(value)  => IntermediateValue::String(value),
            Literal::Integer(value) => IntermediateValue::Integer(value),
            Literal::Float(value)   => IntermediateValue::Float(value),
            Literal::True           => IntermediateValue::Boolean(true),
            Literal::False          => IntermediateValue::Boolean(false),
            Literal::Null           => IntermediateValue::Null,
        }
    }

    fn eval_unary(unary: Unary) -> Result<IntermediateValue, EvaluatorError> {
        match unary {
            Unary::Negation(expr) => {
                match Self::eval_expr(*expr)? {
                    IntermediateValue::String(value) => {
                        // String negation is an error
                        Err(EvaluatorError::StringNegation(value))
                    },
                    IntermediateValue::Integer(value) => {
                        // Negate value and return
                        Ok(IntermediateValue::Integer(-value))
                    },
                    IntermediateValue::Float(value) => {
                        // Negate value and return
                        Ok(IntermediateValue::Float(-value))
                    },
                    IntermediateValue::Boolean(value_is_true) => {
                        if value_is_true {
                            // Negation of 'true' evaluates to '-1'
                            Ok(IntermediateValue::Integer(-1))
                        }
                        else {
                            // Negation of 'false' evaluates to '-0'
                            Ok(IntermediateValue::Integer(-0))
                        }
                    },
                    IntermediateValue::Null => {
                        // Negation of 'null' evaluates to '-0'
                        Ok(IntermediateValue::Integer(-0))
                    },
                }
            },
            Unary::Not(expr) => {
                match Self::eval_expr(*expr)? {
                    IntermediateValue::String(_value) => {
                        // !String evaluates to 'false', because reasons
                        Ok(IntermediateValue::Boolean(false))
                    },
                    IntermediateValue::Integer(value) => {
                        // !Integer evaluates to 'true' for 0, 'false' for all other values
                        if value == 0 {
                            Ok(IntermediateValue::Boolean(true))
                        }
                        else {
                            Ok(IntermediateValue::Boolean(false))
                        }
                    },
                    IntermediateValue::Float(value) => {
                        // !Float evaluates to 'true' for 0.0, 'false' for all other values
                        if value == 0.0 {
                            Ok(IntermediateValue::Boolean(true))
                        }
                        else {
                            Ok(IntermediateValue::Boolean(false))
                        }
                    },
                    IntermediateValue::Boolean(value_is_true) => {
                        // Invert value and return
                        Ok(IntermediateValue::Boolean(!value_is_true))
                    },
                    IntermediateValue::Null => {
                        // !'null' evaluates to 'true'
                        Ok(IntermediateValue::Boolean(true))
                    },
                }
            },
        }
    }

    fn eval_binary(op: Operator, left: Expression, right: Expression) -> Result<IntermediateValue, EvaluatorError> {
        let left_value = Self::eval_expr(left)?;
        let right_value = Self::eval_expr(right)?;

        match op {
            /* Arithmetic Operations */
            Operator::Arithmetic(math_op) => {
                Self::eval_math_op(math_op, left_value, right_value)
            },

            /* Logical Operations */
            Operator::Logical(logic_op) => {
                Ok(IntermediateValue::Boolean(Self::eval_logic_op(logic_op, left_value, right_value)))
            },
        }
    }

    fn eval_math_op(op: ArithmeticOperator, left: IntermediateValue, right: IntermediateValue) -> Result<IntermediateValue, EvaluatorError> {
        match op {
            ArithmeticOperator::Plus    => Ok(left + right),
            ArithmeticOperator::Minus   => left - right,
            _ => unimplemented!("Only plus so far!")
        }
    }

    fn eval_logic_op(op: LogicalOperator, left: IntermediateValue, right: IntermediateValue) -> bool {
        // Perform logical operation per the 'op' parameter, and return the result
        match op {
            LogicalOperator::EqualTo                => left == right,
            LogicalOperator::NotEqualTo             => left != right,
            LogicalOperator::GreaterThan            => left > right,
            LogicalOperator::GreaterThanOrEqualTo   => left >= right,
            LogicalOperator::LessThan               => left < right,
            LogicalOperator::LessThanOrEqualTo      => left <= right,
        }
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////


/*  *  *  *  *  *  *  *\
 *   EvaluatorError   *
\*  *  *  *  *  *  *  */

impl Error for EvaluatorError {}

impl fmt::Display for EvaluatorError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::StringNegation(string) => {
                write!(f, "Attempted to negate String '{}', which is invalid", string)
            },
            Self::UnknownNegation(expr) => {
                write!(f, "Attempted to negate Expression '{}', which is invalid", expr)
            },
            Self::IllegalOperation(op, left, right) => {
                write!(f, "Unsupported operation '{}' on operands L: '{:?}' and R: '{:?}'", op, left, right)
            },
            Self::CouldNotDowncast => {
                write!(f, "NON-ERROR: Could not downcast parameters to their respective type parameters")
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

    use crate::interpreter::{
        evaluator::{
            Evaluator,
        },
        parser::{
            ArithmeticOperator,
            Expression,
            Literal,
            LogicalOperator,
            Operator,
        },
    };


    type TestResult = Result<(), Box<dyn Error>>;

    
    //OPT: *TESTING* Develop a method to test large variety of value/type combos
    #[test]
    fn logical_evaluation() -> TestResult {
        let float_val = 2.0;
        let int_val = 2;

        // Create an Evaluator object loaded with a binary logical expression
        let expr = Expression::Binary(
            Box::new(Expression::Literal(Literal::Float(float_val))),
            Operator::Logical(LogicalOperator::EqualTo),
            Box::new(Expression::Literal(Literal::Integer(int_val))),
        );
        let evaluator = Evaluator::new(expr.clone());

        eprintln!("Evaluating Expression '{}'...", expr.clone());

        // Attempt to evaluate the expression
        let result = evaluator.evaluate()?;
        let casted_result = result.downcast_ref::<bool>().unwrap();

        eprintln!("Expression '{}' == {}", expr, casted_result);

        assert_eq!(
            casted_result,
            &(float_val == int_val as f64)
        );

        Ok(())
    }
    
    #[test]
    fn arithmetic_evaluation() -> TestResult {
        let float_val = 2.5;
        let int_val = 2;

        // Create an Evaluator object loaded with a binary arithmetic expression
        let expr = Expression::Binary(
            Box::new(Expression::Literal(Literal::String(float_val.to_string()))),
            Operator::Arithmetic(ArithmeticOperator::Minus),
            Box::new(Expression::Literal(Literal::Integer(int_val))),
        );
        let evaluator = Evaluator::new(expr.clone());

        eprintln!("Evaluating Expression '{}'...", expr.clone());

        // Attempt to evaluate the expression
        let result = evaluator.evaluate()?;
        let casted_result = result.downcast_ref::<f64>().unwrap();

        eprintln!("Expression '{}' == {}", expr, casted_result);

        assert_eq!(
            casted_result,
            &(float_val - int_val as f64)
        );

        Ok(())
    }
}