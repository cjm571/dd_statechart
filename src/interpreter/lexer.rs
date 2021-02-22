/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : interpreter/lexer.rs

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
    iter::Peekable,
    str::Chars,
};

use crate::interpreter::Token;


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

pub struct Lexer<'c> {
    expr_iter:  Peekable<Chars<'c>>,
    position:   u32,
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    UnexpectedCharacter(String, u32),
    FailedToConsumePeekedValue, //OPT: *DESIGN* This error should be impossible, see if there's a way to eliminate it
    UnterminatedStringLiteral(String, u32),
}



///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl<'c> Lexer<'c> {
    pub fn new(expr_str: &'c str) -> Self {
        Self {
            expr_iter:  expr_str.chars().peekable(),
            position:   0,
        }
    }
    

    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */
    
    pub fn scan(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();

        // Iterate through the end of the expression string
        while self.expr_iter.peek().is_some() {
            // Only push tokens - whitespace is skipped
            if let Some(token) = self.scan_single()? {
                tokens.push(token);
            }
        }
        
        Ok(tokens)
    }

    
    /*  *  *  *  *  *  *  *\
     *   Helper Methods   *
    \*  *  *  *  *  *  *  */

    fn scan_single(&mut self) -> Result<Option<Token>, LexerError> {
        if let Some(character) = self.consume_character() {
            
            // Construct a token based on matching the first character, interrogating subsequent characters if necessary
            let token = match character {
                // Single-Character Tokens
                '(' => Some(Token::LeftParen),
                ')' => Some(Token::RightParen),
                '{' => Some(Token::LeftBrace),
                '}' => Some(Token::RightBrace),
                '[' => Some(Token::LeftBracket),
                ']' => Some(Token::RightBracket),
                ',' => Some(Token::Comma),
                '.' => Some(Token::Dot),
                '-' => Some(Token::Minus),
                '+' => Some(Token::Plus),
                '/' => Some(Token::Slash),
                '*' => Some(Token::Star),

                // Potentially Double-Character Tokens
                '!' => {
                    if self.match_next('=') {
                        Some(Token::BangEqual)
                    }
                    else {
                        Some(Token::Bang)
                    }
                },
                '=' => {
                    if self.match_next('=') {
                        Some(Token::EqualEqual)
                    }
                    else {
                        Some(Token::Equal)
                    }
                },
                '>' => {
                    if self.match_next('=') {
                        Some(Token::GreaterThanOrEqualTo)
                    }
                    else {
                        Some(Token::GreaterThan)
                    }
                },
                '<' => {
                    if self.match_next('=') {
                        Some(Token::LessThanOrEqualTo)
                    }
                    else {
                        Some(Token::LessThan)
                    }
                },

                // String Literals
                '\'' => {
                    Some (
                        self.capture_string_literal()?
                    )
                },

                // Numerical Literals
                x if x.is_digit(10) => {
                    Some (
                        self.capture_numerical_literal(x)?
                    )
                },

                // Identifiers
                x if x.is_alphabetic() || x == '_' => {
                    Some (
                        self.capture_identifier(x)
                    )
                },

                // Whitespace
                x if x.is_ascii_whitespace() => None,

                // Unexpected Character
                _ => {
                    return Err (
                        LexerError::UnexpectedCharacter (
                            String::from(character),
                            self.position
                        )
                    )
                },
            };

            // Successfully scanned a token or whitespace, return accordingly
            Ok(token)
        }
        else {
            // NOTE: This should never happen since we peeked the value in the while statement
            Err(LexerError::FailedToConsumePeekedValue)
        }
    }

    fn match_next(&mut self, expected: char) -> bool {
        // Peek next character and check for match against the expected
        match self.expr_iter.peek() {
            Some(next_char) => {
                if next_char != &expected {
                    // Match failed, do not consumed peeked value
                    return false;
                }

                // Match succeeded, consume the peeked value and update position
                self.consume_character();

                true
            },
            None => {
                // We've reached the end of the expression string,
                // which is effectively a failed match
                false
            }
        }
    }

    fn capture_string_literal(&mut self) -> Result<Token, LexerError> {
        let mut string_chars = Vec::new();
        let start = self.position;
        
        //OPT: *STYLE* Replace a lot of these while-is-some loops with if let None = loop {} trickery
        // Continually peek the next character until closing ' or end of expression is encountered
        while self.expr_iter.peek().is_some() && self.expr_iter.peek().unwrap() != &'\'' {
            // Consume the character and add it to the char vector
            string_chars.push(self.consume_character().unwrap());
        }

        // Ensure we have not reached the end of expression without a closing '
        if self.expr_iter.peek().is_none() {
            return Err(LexerError::UnterminatedStringLiteral(string_chars.into_iter().collect(), start));
        }

        // Consume (and discard) the closing '
        self.consume_character();

        Ok (
            Token::String (
                string_chars.into_iter().collect()
            )
        )
    }

    fn capture_numerical_literal(&mut self, first_char: char) -> Result<Token, LexerError> {
        let mut numerical_chars = vec![first_char];
        let mut is_float = false;

        // Continually peek the next character until a non-digit or non-dot is encountered
        while self.expr_iter.peek().is_some() && 
        (self.expr_iter.peek().unwrap().is_digit(10) || self.expr_iter.peek().unwrap() == &'.') {
            if self.expr_iter.peek().unwrap() == &'.' {
                if is_float {
                    // Already encountered a '.', another is invalid
                    return Err(LexerError::UnexpectedCharacter(String::from('.'), self.position+1));
                }
                else {
                    // Encountered first '.', denote literal as a float
                    is_float = true;
                }
            }
            
            numerical_chars.push(self.consume_character().unwrap());
        }
        let numerical_string: String = numerical_chars.into_iter().collect();

        // Handle according to numerical type
        if is_float {
            Ok(Token::Float(numerical_string.parse::<f64>().unwrap()))
        }
        else {
            Ok(Token::Integer(numerical_string.parse::<i32>().unwrap()))
        }
    }

    fn capture_identifier(&mut self, first_char: char) -> Token {
        let mut identifier_chars = vec![first_char];

        while self.expr_iter.peek().is_some() && self.expr_iter.peek().unwrap().is_alphanumeric() {
            identifier_chars.push(self.consume_character().unwrap());
        }

        Token::Identifier(identifier_chars.into_iter().collect())
    }

    // Advance iterator and update position
    fn consume_character(&mut self) -> Option<char> {
        self.position += 1;

        self.expr_iter.next()
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *     LexerError     *
\*  *  *  *  *  *  *  */

impl Error for LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedCharacter(lexeme, position) => {
                write!(f, "Unexpected character '{}' at position {} in expression", lexeme, position)
            },
            Self::FailedToConsumePeekedValue => {
                write!(f, "Somehow failed to consume the expression iterator after successfully peeking...")
            },
            Self::UnterminatedStringLiteral(string, start_pos) => {
                write!(f, "Unterminated string literal '{}' starting at position {}", string, start_pos)
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
    
    use crate::interpreter::lexer::{
        Lexer,
        LexerError,
    };


    type TestResult = Result<(), Box<dyn Error>>;


    #[test]
    fn output_test() -> TestResult {
        let expr_str = "_event.data[0] == 'OPER.PLUS'";
        let mut lexer = Lexer::new(expr_str);

        eprintln!("Tokens from '{}':\n{:?}", expr_str, lexer.scan());

        Ok(())
    }

    #[test]
    fn unexpected_char() -> TestResult {
        let mut lexer_a = Lexer::new("`");
        let mut lexer_b = Lexer::new("5.5.5");
        
        assert_eq!(
            lexer_a.scan(),
            Err(LexerError::UnexpectedCharacter(String::from('`'), 1)),
        );
        
        assert_eq!(
            lexer_b.scan(),
            Err(LexerError::UnexpectedCharacter(String::from('.'), 4)),
        );

        Ok(())
    }

    #[test]
    fn unterm_string_literal() -> TestResult {
        let mut lexer = Lexer::new("'unterm_string_literal");
        
        assert_eq!(
            lexer.scan(),
            Err(LexerError::UnterminatedStringLiteral(String::from("unterm_string_literal"), 1)),
        );

        Ok(())
    }
}