/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : interpreter/mod.rs

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
    This module defines a barebones interpreter for single-line ECMAScript
    expressions.

    //TODO: More, probably

    NONCONFORMANCES:
    1. Expressions that evaluate to 'NaN' are currently treated as errors

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
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

use crate::datamodel::SystemVariables;


///////////////////////////////////////////////////////////////////////////////
//  Module Declarations
///////////////////////////////////////////////////////////////////////////////

mod evaluator;
mod lexer;
mod parser;

use evaluator::{
    Evaluator,
    EvaluatorError,
};
use lexer::{
    Lexer,
    LexerError,
};
use parser::{
    Parser,
    ParserError,
};

//FIXME: Is this Clone necessary?
//FEAT: Implement BigInt type
#[derive(Clone, Debug)]
pub enum EcmaScriptValue {
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
}


//FIXME: Don't like this style... Feel like a memberless struct would be cleaner
///////////////////////////////////////////////////////////////////////////////
//  Static Functions
///////////////////////////////////////////////////////////////////////////////

pub fn interpret(input_str: &str, sys_vars: &SystemVariables) -> Result<EcmaScriptValue, InterpreterError> {
    // Scan input string for tokens
    let mut lexer = Lexer::new(input_str);
    let tokens = lexer.scan()?;

    // Parse tokens for expression
    let mut parser = Parser::new(&tokens);
    let expr = parser.parse()?;

    // Evaluate expression
    let evaluator = Evaluator::new(expr, sys_vars);
    Ok(evaluator.evaluate()?)
}

pub fn interpret_as_bool(input_str: &str, sys_vars: &SystemVariables) -> Result<bool, InterpreterError> {
    // Scan input string for tokens
    let mut lexer = Lexer::new(input_str);
    let tokens = lexer.scan()?;

    // Parse tokens for expression
    let mut parser = Parser::new(&tokens);
    let expr = parser.parse()?;

    // Evaluate expression
    let evaluator = Evaluator::new(expr, sys_vars);
    let result = evaluator.evaluate()?;

    // Leverage ECMAScript rules attached to the IV enum
    Ok(result == EcmaScriptValue::Boolean(true))
}


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq)]
pub enum InterpreterError {
    InvalidOperatorConversion(Token),

    EvaluatorError(EvaluatorError),
    LexerError(LexerError),
    ParserError(ParserError),
}

//OPT: *DESIGN* May be useful to stratify into arithmetic, logical, etc.
//              Could avoid duplication of subsets of the enum in submodules
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Single-Character Tokens
    // NOTE: Semicolons are not supported
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Slash,
    Star,

    // Potentially Double-Character Tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    GreaterThan,
    GreaterThanOrEqualTo,
    LessThan,
    LessThanOrEqualTo,

    // Literals
    Identifier(String),
    String(String),
    Number(f64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    Unary(Unary),
    Binary(Box<Self>, Operator, Box<Self>),
    Grouping(Box<Self>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Unary {
    Negation(Box<Expression>),
    Not(Box<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Logical(LogicalOperator),
    Arithmetic(ArithmeticOperator),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LogicalOperator {
    EqualTo,
    NotEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
    LessThan,
    LessThanOrEqualTo,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArithmeticOperator {
    Plus,
    Minus,
    Star,
    Slash,
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *  InterpreterError  *
\*  *  *  *  *  *  *  */

impl Error for InterpreterError {}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidOperatorConversion(token) => {
                write!(f, "Invalid Token->Operator conversion for token '{:?}'", token)
            },
            Self::EvaluatorError(eval_err) => {
                write!(f, "{}", eval_err)
            },
            Self::LexerError(lexer_err) => {
                write!(f, "{}", lexer_err)
            },
            Self::ParserError(parse_err) => {
                write!(f, "{}", parse_err)
            },
        }
    }
}

impl From<EvaluatorError> for InterpreterError {
    fn from(src: EvaluatorError) -> Self {
        Self::EvaluatorError(src)
    }
}
impl From<LexerError> for InterpreterError {
    fn from(src: LexerError) -> Self {
        Self::LexerError(src)
    }
}
impl From<ParserError> for InterpreterError {
    fn from(src: ParserError) -> Self {
        Self::ParserError(src)
    }
}


/*  *  *  *  *  *  *  *\
 *     Expression     *
\*  *  *  *  *  *  *  */

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Literal(literal)          => write!(f, "{}", literal),
            Self::Identifier(name)          => write!(f, "var({})", name),
            Self::Unary(unary)              => write!(f, "({})", unary),
            Self::Binary(left, op, right)   => write!(f, "({} {} {})", op, left, right),
            Self::Grouping(expr)            => write!(f, "(group {})", expr),
        }
    }
}


/*  *  *  *  *  *  *  *\
 *      Literal       *
\*  *  *  *  *  *  *  */

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Number(val)       => write!(f, "{:?}", val),
            Self::String(string)    => write!(f, "{:?}", string),
            Self::True              => write!(f, "true"),
            Self::False             => write!(f, "false"),
            Self::Null              => write!(f, "null"),
        }
    }
}


/*  *  *  *  *  *  *  *\
 *       Unary        *
\*  *  *  *  *  *  *  */

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Negation(expr)    => write!(f, "- {}", expr),
            Self::Not(expr)         => write!(f, "! {}", expr),
        }
    }
}


/*  *  *  *  *  *  *  *\
 *      Operator      *
\*  *  *  *  *  *  *  */

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Logical(op)       => write!(f, "{}", op),
            Self::Arithmetic(op)    => write!(f, "{}", op),
        }
    }
}


/*  *  *  *  *  *  *  *\
 *   LogicalOperator  *
\*  *  *  *  *  *  *  */

impl fmt::Display for LogicalOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::EqualTo               => write!(f, "=="),
            Self::NotEqualTo            => write!(f, "!="),
            Self::GreaterThan           => write!(f, ">"),
            Self::GreaterThanOrEqualTo  => write!(f, ">="),
            Self::LessThan              => write!(f, "<"),
            Self::LessThanOrEqualTo     => write!(f, "<="),
        }
    }
}



/*  *  *  *  *  *  *  *\
 * ArithmeticOperator *
\*  *  *  *  *  *  *  */

impl fmt::Display for ArithmeticOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Plus  => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star  => write!(f, "*"),
            Self::Slash => write!(f, "/"),
        }
    }
}



/*  *  *  *  *  *  *  *\
 *   EcmaScriptValue  *
\*  *  *  *  *  *  *  */

impl PartialEq for EcmaScriptValue {
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
impl PartialOrd for EcmaScriptValue {
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
impl Add<Self> for EcmaScriptValue {
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
impl Sub<Self> for EcmaScriptValue {
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
impl Mul<Self> for EcmaScriptValue {
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
impl Div<Self> for EcmaScriptValue {
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
        lexer::Lexer,
        parser::Parser,
    };


    type TestResult = Result<(), Box<dyn Error>>;


    #[test]
    fn no_identifiers() -> TestResult {
        let cond_str = "-5 != (10 / 2 * (3 + 1)) == true";
        eprintln!("Processing string '{}'...", cond_str);

        // Create the Lexer instance and scan for Tokens
        let mut lexer = Lexer::new(cond_str);
        let tokens = lexer.scan()?;

        // Create the Parser instance and parse the Tokens into an Expression
        let mut parser = Parser::new(&tokens);
        let expr = parser.parse()?;

        // Pretty-Print the Expression
        eprintln!("*** Result ***\n{}", expr);
        
        Ok(())
    }
}
