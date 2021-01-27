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


///////////////////////////////////////////////////////////////////////////////
//  Module Declarations
///////////////////////////////////////////////////////////////////////////////

pub mod lexer;
pub mod parser;


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

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
    Float(f32),
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
