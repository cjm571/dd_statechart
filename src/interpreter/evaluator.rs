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
        Div,
        Mul,
        Sub,
    },
};

use crate::interpreter::{
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

//FEAT: Implement BigInt type
#[derive(Debug)]
pub enum IntermediateValue {
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
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
            IntermediateValue::Number(value)    => Ok(Box::new(value)),
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
            Literal::Integer(value) => IntermediateValue::Number(value as f64),
            Literal::Float(value)   => IntermediateValue::Number(value),
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
                    IntermediateValue::Number(value) => {
                        // Negate value and return
                        Ok(IntermediateValue::Number(-value))
                    },
                    IntermediateValue::Boolean(value_is_true) => {
                        if value_is_true {
                            // Negation of 'true' evaluates to '-1.0'
                            Ok(IntermediateValue::Number(-1.0))
                        }
                        else {
                            // Negation of 'false' evaluates to '-0.0'
                            Ok(IntermediateValue::Number(-0.0))
                        }
                    },
                    IntermediateValue::Null => {
                        // Negation of 'null' evaluates to '-0.0'
                        Ok(IntermediateValue::Number(-0.0))
                    },
                }
            },
            Unary::Not(expr) => {
                match Self::eval_expr(*expr)? {
                    IntermediateValue::String(_value) => {
                        // !String evaluates to 'false', because reasons
                        Ok(IntermediateValue::Boolean(false))
                    },
                    IntermediateValue::Number(value) => {
                        // !Number evaluates to 'true' for 0, 'false' for all other values
                        if (value - 0.0).abs() < f64::EPSILON {
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
            ArithmeticOperator::Star    => left * right,
            ArithmeticOperator::Slash   => left / right,
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


/*  *  *  *  *  *  *  *\
 *  IntermediateValue *
\*  *  *  *  *  *  *  */

impl PartialEq for IntermediateValue {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::String(self_value) => {
                match other {
                    Self::String(other_value) => {
                        // Simple string-to-string comparison
                        self_value == other_value
                    },
                    Self::Number(other_value) => {
                        // Attempt to parse String as f64 and compare values
                        if let Ok(parsed_value) = self_value.parse::<f64>() {
                            &parsed_value == other_value
                        }
                        else {
                            false
                        }
                    },
                    Self::Boolean(other_value_is_true) => {
                        // Check against '1.0' and '0.0'
                        if let Ok(parsed_value) = self_value.parse::<f64>() {
                            if *other_value_is_true {
                                (parsed_value - 1.0).abs() < f64::EPSILON
                            }
                            else {
                                (parsed_value - 0.0).abs() < f64::EPSILON
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
            Self::Number(self_value) => {
                match other {
                    Self::String(other_value) => {
                        // Leverage existing comparison
                        Self::String(other_value.clone()) == *self
                    },
                    Self::Number(other_value) => {
                        // Simple comparison
                        self_value == other_value
                    },
                    Self::Boolean(other_value_is_true) => {
                        // Check against '1.0' and '0.0'
                        if *other_value_is_true {
                            (*self_value - 1.0).abs() < f64::EPSILON
                        }
                        else {
                            (*self_value - 0.0).abs() < f64::EPSILON
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
                    Self::Number(other_value) => {
                        // Leverage existing comparison
                        Self::Number(*other_value) == *self
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
                    Self::Number(other_value) => {
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
                        // Attempt to parse string as number, as they can be compared to 'null'
                        // as if it were 0
                        if let Ok(parsed_value) = self_value.parse::<f64>() {
                            parsed_value.partial_cmp(&0.0)
                        }
                        else {
                            // Non-numeric strings always return false
                            None
                        }
                    },
                }
            },
            Self::Number(self_value) => {
                match other {
                    Self::String(other_value) => {
                        // Leverage existing comparison
                        Self::String(other_value.clone()).partial_cmp(self)
                    },
                    Self::Number(other_value) => {
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
                        // 'null' can be treated as if it were 0.0
                        self_value.partial_cmp(&0.0)
                    },
                }
            },
            Self::Boolean(self_value) => {
                match other {
                    Self::String(other_value) => {
                        // Leverage existing comparison
                        Self::String(other_value.clone()).partial_cmp(self)
                    },
                    Self::Number(other_value) => {
                        // Leverage existing comparison
                        Self::Number(*other_value).partial_cmp(self)
                    },
                    Self::Boolean(other_value) => {
                        // Leverage existing comparison
                        Self::Boolean(*other_value).partial_cmp(self)
                    },
                    Self::Null => {
                        // 'null' can be treated as if it were 0
                        (*self_value as i32 as f64).partial_cmp(&0.0)
                    },
                }
            },
            Self::Null => {
                match other {
                    Self::String(other_value) => {
                        // Leverage existing comparison
                        Self::String(other_value.clone()).partial_cmp(self)
                    },
                    Self::Number(other_value) => {
                        // Leverage existing comparison
                        Self::Number(*other_value).partial_cmp(self)
                    },
                    Self::Boolean(other_value) => {
                        // Leverage existing comparison
                        Self::Boolean(*other_value).partial_cmp(self)
                    },
                    Self::Null => {
                        // 'null' always returns false when compared to itself
                        None
                    },
                }
            },
        }
    }
}
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
                    Self::Number(rhs_value) => {
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
            Self::Number(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Leverage existing functionality
                        Self::String(rhs_value).add(self)
                    },
                    Self::Number(rhs_value) => {
                        // Leverage existing functionality
                        Self::Number(rhs_value).add(self)
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1.0, 'false' as 0.0
                        Self::Number(self_value + rhs_value as i32 as f64)
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
                    Self::Number(rhs_value) => {
                        // Leverage existing functionality
                        Self::Number(rhs_value).add(self)
                    },
                    Self::Boolean(rhs_value) => {
                        // Leverage existing functionality
                        Self::Boolean(rhs_value).add(self)
                    },
                    Self::Null => {
                        // Forces a conversion to an f64
                        Self::Number(self_value as i32 as f64)
                    },
                }
            },
            Self::Null => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Leverage existing functionality
                        Self::String(rhs_value).add(self)
                    },
                    Self::Number(rhs_value) => {
                        // Leverage existing functionality
                        Self::Number(rhs_value).add(self)
                    },
                    Self::Boolean(rhs_value) => {
                        // Leverage existing functionality
                        Self::Boolean(rhs_value).add(self)
                    },
                    Self::Null => {
                        // Evaluates to 0
                        Self::Number(0.0)
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
                if let Ok(parsed_self_value_f64) = self_value.parse::<f64>() {
                    match rhs {
                        Self::String(rhs_value) => {
                            // Can only operate on strings that can be parsed into numbers
                            if let Ok(parsed_rhs_value_f64) = rhs_value.parse::<f64>() {
                                // Simple subtraction
                                Ok(Self::Number(parsed_self_value_f64 - parsed_rhs_value_f64))
                            }
                            else {
                                // Right operand could not be parsed into number, therefore subtraction is illegal
                                Err (
                                    EvaluatorError::IllegalOperation (
                                        ArithmeticOperator::Minus,
                                        Self::String(self_value),
                                        Self::String(rhs_value),
                                    )
                                )
                            }
                        },
                        Self::Number(rhs_value) => {
                            // Simple subtraction
                            Ok(Self::Number(parsed_self_value_f64 - rhs_value))
                        },
                        Self::Boolean(rhs_value) => {
                            // Treat 'true' as 1.0, 'false' as 0.0
                            Ok(Self::Number(parsed_self_value_f64 - rhs_value as i32 as f64))
                        },
                        Self::Null => {
                            // Forces conversion to number
                            Ok(Self::Number(parsed_self_value_f64))
                        },
                    }
                }
                else {
                    // Left operand could not be parsed into number, therefore subtraction is illegal
                    Err (
                        EvaluatorError::IllegalOperation (
                            ArithmeticOperator::Minus,
                            Self::String(self_value),
                            rhs,
                        )
                    )
                }
            },
            Self::Number(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Can only operate on strings that can be parsed into numbers
                        if let Ok(parsed_value) = rhs_value.parse::<f64>() {
                            // Simple subtraction
                            Ok(Self::Number(self_value - parsed_value))
                        }
                        else {
                            // Right operand could not be parsed into number, therefore subtraction is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Minus,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Number(rhs_value) => {
                        // Simple subtraction
                        Ok(Self::Number(self_value - rhs_value))
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1.0, 'false' as 0.0
                        Ok(Self::Number(self_value - rhs_value as i32 as f64))
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
                         if let Ok(parsed_value) = rhs_value.parse::<f64>() {
                            // Treat 'true' as 1.0, 'false' as 0.0
                            Ok(Self::Number(self_value as i32 as f64 - parsed_value))
                        }
                        else {
                            // Right operand could not be parsed into number, therefore subtraction is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Minus,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Number(rhs_value) => {
                        // Treat 'true' as 1.0, 'false' as 0.0
                        Ok(Self::Number(self_value as i32 as f64 - rhs_value))
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1, 'false' as 0
                        Ok(Self::Number(self_value as i32 as f64 - rhs_value as i32 as f64))
                    },
                    Self::Null => {
                        // Forces conversion to an f64
                        Ok(Self::Number(self_value as i32 as f64))
                    },
                }
            },
            Self::Null => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Can only operate on strings that can be parsed into numbers
                        if let Ok(parsed_value) = rhs_value.parse::<f64>() {
                            // Essentially a negation
                            Ok(Self::Number(-parsed_value))
                        }
                        else {
                            // Right operand could not be parsed into number, therefore subtraction is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Minus,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Number(rhs_value) => {
                        // Essentially a negation
                        Ok(Self::Number(-rhs_value))
                    },
                    Self::Boolean(rhs_value) => {
                        // Convert to f64 and negate
                        Ok(Self::Number(-(rhs_value as i32 as f64)))
                    },
                    Self::Null => {
                        // Evaluates to 0
                        Ok(Self::Number(0.0))
                    },
                }
            },
        }
    }
}
impl Mul<Self> for IntermediateValue {
    type Output = Result<Self, EvaluatorError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Self::String(self_value) => {
                // Can only operate on strings that can be parsed into numbers
                if let Ok(parsed_self_value_f64) = self_value.parse::<f64>() {
                    match rhs {
                        Self::String(rhs_value) => {
                            // Can only operate on strings that can be parsed into numbers
                            if let Ok(parsed_rhs_value_f64) = rhs_value.parse::<f64>() {
                                // Simple multiplication
                                Ok(Self::Number(parsed_self_value_f64 * parsed_rhs_value_f64))
                            }
                            else {
                                // Right operand could not be parsed into number, therefore multiplication is illegal
                                Err (
                                    EvaluatorError::IllegalOperation (
                                        ArithmeticOperator::Star,
                                        Self::String(self_value),
                                        Self::String(rhs_value),
                                    )
                                )
                            }
                        },
                        Self::Number(rhs_value) => {
                            // Simple multiplication
                            Ok(Self::Number(parsed_self_value_f64 * rhs_value))
                        },
                        Self::Boolean(rhs_value) => {
                            // Treat 'true' as 1.0, 'false' as 0.0
                            Ok(Self::Number(parsed_self_value_f64 * rhs_value as i32 as f64))
                        },
                        Self::Null => {
                            // Evaluates to 0
                            Ok(Self::Number(0.0))
                        },
                    }
                }
                else {
                    // Left operand could not be parsed into number, therefore multiplication is illegal
                    Err (
                        EvaluatorError::IllegalOperation (
                            ArithmeticOperator::Star,
                            Self::String(self_value),
                            rhs,
                        )
                    )
                }
            },
            Self::Number(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Can only operate on strings that can be parsed into numbers
                        if let Ok(parsed_value) = rhs_value.parse::<f64>() {
                            // Simple multiplication
                            Ok(Self::Number(self_value * parsed_value))
                        }
                        else {
                            // Right operand could not be parsed into number, therefore multiplication is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Star,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Number(rhs_value) => {
                        // Simple multiplication
                        Ok(Self::Number(self_value * rhs_value))
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1.0, 'false' as 0.0
                        Ok(Self::Number(self_value * rhs_value as i32 as f64))
                    },
                    Self::Null => {
                        // Evaluates to 0
                        Ok(Self::Number(0.0))
                    },
                }
            },
            Self::Boolean(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // Can only operate on strings that can be parsed into numbers
                        if let Ok(parsed_value) = rhs_value.parse::<f64>() {
                            // Treat 'true' as 1.0, 'false' as 0.0
                            Ok(Self::Number(self_value as i32 as f64 * parsed_value))
                        }
                        else {
                            // Right operand could not be parsed into number, therefore multiplication is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Star,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Number(rhs_value) => {
                        // Treat 'true' as 1.0, 'false' as 0.0
                        Ok(Self::Number(self_value as i32 as f64 * rhs_value))
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1, 'false' as 0
                        Ok(Self::Number(self_value as i32 as f64 * rhs_value as i32 as f64))
                    },
                    Self::Null => {
                        // Evaluates to 0
                        Ok(Self::Number(0.0))
                    },
                }
            },
            Self::Null => {
                match rhs {
                    // Multiplying by 'null' evaluates to '0' against all but non-numeric strings,
                    // which is 'NaN'
                    Self::String(rhs_value) => {
                        if rhs_value.parse::<f64>().is_ok() {
                            Ok(Self::Number(0.0))
                        }
                        else {
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Star,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Number(_)     => Ok(Self::Number(0.0)),
                    Self::Boolean(_)    => Ok(Self::Number(0.0)),
                    Self::Null          => Ok(Self::Number(0.0)),
                }
            },
        }
    }
}
impl Div<Self> for IntermediateValue {
    type Output = Result<Self, EvaluatorError>;

    fn div(self, rhs: Self) -> Self::Output {
        // Check for divide-by-zero before evaluating further
        match &rhs {
            Self::String(rhs_value) => {
                if rhs_value.parse::<f64>().map_or(false, |v| (v - 0.0).abs() < f64::EPSILON) {
                    return Ok(Self::Number(f64::INFINITY));
                }
            },
            Self::Number(rhs_value) if (rhs_value - 0.0).abs() < f64::EPSILON => {
                return Ok(Self::Number(f64::INFINITY));
            },
            _ => {
                /* Not dividing by 0, carry on */
            },
        }

        match self {
            Self::String(self_value) => {
                // ECMAScript forces division to produce a float result, so just 
                // attempt to parse everything as f64
                if let Ok(parsed_self_value) = self_value.parse::<f64>() {
                    match rhs {
                        Self::String(rhs_value) => {
                            // ECMAScript forces division to produce a float result, so just 
                            // attempt to parse everything as f64
                            if let Ok(parsed_rhs_value) = rhs_value.parse::<f64>() {
                                // Simple division
                                Ok(Self::Number(parsed_self_value / parsed_rhs_value))
                            }
                            else {
                                // Right operand could not be parsed into float, therefore division is illegal
                                Err (
                                    EvaluatorError::IllegalOperation (
                                        ArithmeticOperator::Slash,
                                        Self::String(self_value),
                                        Self::String(rhs_value),
                                    )
                                )
                            }
                        },
                        Self::Number(rhs_value) => {
                            // Simple division
                            Ok(Self::Number(parsed_self_value / rhs_value))
                        },
                        Self::Boolean(rhs_value) => {
                            // Treat 'true' as 1, 'false' as 0
                            Ok(Self::Number(parsed_self_value / rhs_value as i32 as f64))
                        },
                        Self::Null => {
                            // Evaluates to Infinity
                            Ok(Self::Number(f64::INFINITY))
                        },
                    }
                }
                else {
                    // Left operand could not be parsed into number, therefore division is illegal
                    Err (
                        EvaluatorError::IllegalOperation (
                            ArithmeticOperator::Slash,
                            Self::String(self_value),
                            rhs,
                        )
                    )
                }
            },
            Self::Number(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // ECMAScript forces division to produce a float result, so just 
                        // attempt to parse everything as f64
                        if let Ok(parsed_rhs_value) = rhs_value.parse::<f64>() {
                            // Simple division
                            Ok(Self::Number(self_value / parsed_rhs_value))
                        }
                        else {
                            // Right operand could not be parsed into number, therefore division is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Slash,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Number(rhs_value) => {
                        // Simple division
                        Ok(Self::Number(self_value / rhs_value))
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1.0, 'false' as 0.0
                        Ok(Self::Number(self_value / rhs_value as i32 as f64))
                    },
                    Self::Null => {
                        // Evaluates to Infinity
                        Ok(Self::Number(f64::INFINITY))
                    },
                }
            },
            Self::Boolean(self_value) => {
                match rhs {
                    Self::String(rhs_value) => {
                        // ECMAScript forces division to produce a float result, so just 
                        // attempt to parse everything as f64
                        if let Ok(parsed_rhs_value) = rhs_value.parse::<f64>() {
                            // Casted division
                            Ok(Self::Number(self_value as i32 as f64 / parsed_rhs_value))
                        }
                        else {
                            // Right operand could not be parsed into number, therefore division is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Slash,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Number(rhs_value) => {
                        // Treat 'true' as 1.0, 'false' as 0.0
                        Ok(Self::Number(self_value as i32 as f64 / rhs_value))
                    },
                    Self::Boolean(rhs_value) => {
                        // Treat 'true' as 1, 'false' as 0
                        Ok(Self::Number(self_value as i32 as f64 / rhs_value as i32 as f64))
                    },
                    Self::Null => {
                        // 'true' / null == Infinity
                        if self_value {
                            Ok(Self::Number(f64::INFINITY))
                        }
                        // 'false' / null == NaN
                        else {
                            Err(
                                EvaluatorError::IllegalOperation(
                                    ArithmeticOperator::Slash,
                                    self,
                                    rhs,
                                )
                            )
                        }
                    },
                }
            },
            Self::Null => {
                match rhs {
                    Self::String(rhs_value) => {
                        // ECMAScript forces division to produce a float result, so just 
                        // attempt to parse everything as f64
                        if rhs_value.parse::<f64>().is_ok() {
                            // Evaluates to 0
                            Ok(Self::Number(0.0))
                        }
                        else {
                            // Right operand could not be parsed into number, therefore division is illegal
                            Err (
                                EvaluatorError::IllegalOperation (
                                    ArithmeticOperator::Slash,
                                    self,
                                    Self::String(rhs_value),
                                )
                            )
                        }
                    },
                    Self::Number(_) => {
                        // Evaluates to 0
                        Ok(Self::Number(0.0))
                    },
                    Self::Boolean(rhs_value) => {
                        // null / 'true' == 0
                        if rhs_value {
                            Ok(Self::Number(0.0))
                        }
                        // null /'false' == NaN
                        else {
                            Err(
                                EvaluatorError::IllegalOperation(
                                    ArithmeticOperator::Slash,
                                    self,
                                    rhs,
                                )
                            )
                        }
                    },
                    Self::Null => {
                        // null / null == NaN
                        Err(
                            EvaluatorError::IllegalOperation(
                                ArithmeticOperator::Slash,
                                self,
                                rhs,
                            )
                        )
                    },
                }
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
        ArithmeticOperator,
        Expression,
        Literal,
        LogicalOperator,
        Operator,
        evaluator::{
            Evaluator,
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

        eprintln!("Expression '{}' == {:?}", expr, casted_result);

        assert_eq!(
            casted_result,
            &(float_val == int_val as f64)
        );

        Ok(())
    }
    
    #[test]
    fn arithmetic_evaluation() -> TestResult {
        let float_val = 4.0;
        let int_val = 0;

        // Create an Evaluator object loaded with a binary arithmetic expression
        let expr = Expression::Binary(
            Box::new(Expression::Literal(Literal::String(float_val.to_string()))),
            Operator::Arithmetic(ArithmeticOperator::Slash),
            Box::new(Expression::Literal(Literal::Integer(int_val))),
        );
        let evaluator = Evaluator::new(expr.clone());

        eprintln!("Evaluating Expression '{}'...", expr.clone());

        // Attempt to evaluate the expression
        let result = evaluator.evaluate()?;
        let casted_result = result.downcast_ref::<f64>().unwrap();

        eprintln!("Expression '{}' == {:?}", expr, casted_result);

        assert_eq!(
            casted_result,
            &(f64::INFINITY)
        );

        Ok(())
    }

    #[test]
    fn string_concatenation() -> TestResult {
        let float_val = 2.5;
        let int_val = 2;

        // Create an Evaluator object loaded with a binary arithmetic expression
        let expr = Expression::Binary(
            Box::new(Expression::Literal(Literal::String(float_val.to_string()))),
            Operator::Arithmetic(ArithmeticOperator::Plus),
            Box::new(Expression::Literal(Literal::String(int_val.to_string()))),
        );
        let evaluator = Evaluator::new(expr.clone());

        eprintln!("Evaluating Expression '{}'...", expr.clone());

        // Attempt to evaluate the expression
        let result = evaluator.evaluate()?;
        let casted_result = result.downcast_ref::<String>().unwrap();

        eprintln!("Expression '{}' == {:?}", expr, casted_result);

        assert_eq!(
            casted_result,
            &(float_val.to_string() + &int_val.to_string())
        );

        Ok(())
    }

    #[test]
    fn null_comparison() -> TestResult {
        let int_val = 0;

        // Create an Evaluator object loaded with a binary arithmetic expression
        let expr = Expression::Binary(
            Box::new(Expression::Literal(Literal::String(int_val.to_string()))),
            Operator::Logical(LogicalOperator::EqualTo),
            Box::new(Expression::Literal(Literal::Null)),
        );
        let evaluator = Evaluator::new(expr.clone());

        eprintln!("Evaluating Expression '{}'...", expr.clone());

        // Attempt to evaluate the expression
        let result = evaluator.evaluate()?;
        let casted_result = result.downcast_ref::<bool>().unwrap();

        eprintln!("Expression '{}' == {:?}", expr, casted_result);

        assert_eq!(
            casted_result,
            &false
        );

        Ok(())
    }
}
