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
    //TODO: Purpose statement

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
    convert::TryFrom,
    error::Error,
    fmt,
};


///////////////////////////////////////////////////////////////////////////////
//  Module Declarations
///////////////////////////////////////////////////////////////////////////////

pub mod evaluator;
pub mod lexer;
pub mod parser;


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq)]
pub enum InterpreterError {
    InvalidOperatorConversion(Token)
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
    Integer(i32),
    Float(f64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Unary(Unary),
    Binary(Box<Self>, Operator, Box<Self>),
    Grouping(Box<Self>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Integer(i32),
    Float(f64),
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
        }
    }
}


/*  *  *  *  *  *  *  *\
 *     Expression     *
\*  *  *  *  *  *  *  */

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Literal(literal)          => write!(f, "{}", literal),
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
            Self::Integer(val)      => write!(f, "{:?}", val),
            Self::Float(val)        => write!(f, "{:?}", val),
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

//OPT: *DESIGN* Is this appropriate? Can't do a 100% reliable token->operator conversion without context...
impl TryFrom<&Token> for Operator {
    type Error = InterpreterError;

    fn try_from(src: &Token) -> Result<Self, Self::Error> {
        match src {
            Token::EqualEqual           => Ok(Self::Logical(LogicalOperator::EqualTo)),
            Token::BangEqual            => Ok(Self::Logical(LogicalOperator::NotEqualTo)),
            Token::GreaterThan          => Ok(Self::Logical(LogicalOperator::GreaterThan)),
            Token::GreaterThanOrEqualTo => Ok(Self::Logical(LogicalOperator::GreaterThanOrEqualTo)),
            Token::LessThan             => Ok(Self::Logical(LogicalOperator::LessThan)),
            Token::LessThanOrEqualTo    => Ok(Self::Logical(LogicalOperator::LessThanOrEqualTo)),
            Token::Plus                 => Ok(Self::Arithmetic(ArithmeticOperator::Plus)),
            Token::Minus                => Ok(Self::Arithmetic(ArithmeticOperator::Minus)),
            Token::Star                 => Ok(Self::Arithmetic(ArithmeticOperator::Star)),
            Token::Slash                => Ok(Self::Arithmetic(ArithmeticOperator::Slash)),
            _                           => Err(InterpreterError::InvalidOperatorConversion(src.clone())),
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
