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
    This module comprises the first phase of the ECMAScript Interpreter.

    It consumes the characters of an expression string and generates Tokens
    from them, to be passed on to the Parser for the next phase of
    interpretation.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
    error::Error,
    fmt,
    iter::Peekable,
    num::ParseFloatError,
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
    UnexpectedCharacter(
        String, /* Unexpected Character */
        u32,    /* Character Position */
    ),
    UnterminatedStringLiteral(
        String, /* Unterminated String */
        u32,    /* Start of String */
    ),

    // Wrappers
    ParseFloatError(
        ParseFloatError,    /* Wrapped Error */
        u32,                /* Start of Number */
    ),
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
        while let Some(token) = self.scan_single()? {
            tokens.push(token);
        }

        Ok(tokens)
    }


    /*  *  *  *  *  *  *  *\
     *   Helper Methods   *
    \*  *  *  *  *  *  *  */

    fn scan_single(&mut self) -> Result<Option<Token>, LexerError> {
        // Construct a token based on matching the first character, interrogating subsequent characters if necessary
        let token = match self.consume_character() {
            // Single-Character Tokens
            Some('(') => Some(Token::LeftParen),
            Some(')') => Some(Token::RightParen),
            Some('{') => Some(Token::LeftBrace),
            Some('}') => Some(Token::RightBrace),
            Some('[') => Some(Token::LeftBracket),
            Some(']') => Some(Token::RightBracket),
            Some(',') => Some(Token::Comma),
            Some('.') => Some(Token::Dot),
            Some('-') => Some(Token::Minus),
            Some('+') => Some(Token::Plus),
            Some('/') => Some(Token::Slash),
            Some('*') => Some(Token::Star),

            // Potentially Double-Character Tokens
            Some('!') => {
                if self.match_next('=') {
                    Some(Token::BangEqual)
                }
                else {
                    Some(Token::Bang)
                }
            },
            Some('=') => {
                if self.match_next('=') {
                    Some(Token::EqualEqual)
                }
                else {
                    Some(Token::Equal)
                }
            },
            Some('>') => {
                if self.match_next('=') {
                    Some(Token::GreaterThanOrEqualTo)
                }
                else {
                    Some(Token::GreaterThan)
                }
            },
            Some('<') => {
                if self.match_next('=') {
                    Some(Token::LessThanOrEqualTo)
                }
                else {
                    Some(Token::LessThan)
                }
            },

            // String Literals
            Some('\'') => {
                Some (
                    self.capture_string_literal()?
                )
            },

            // Numerical Literals
            Some(x) if x.is_digit(10) => {
                Some (
                    self.capture_numerical_literal(x)?
                )
            },

            // Identifiers
            Some(x) if x.is_alphabetic() || x == '_' => {
                Some (
                    self.capture_identifier(x)
                )
            },

            // Whitespace
            Some(x) if x.is_ascii_whitespace() => {
                // Allow whitespace character to be consumed and scan the next token
                self.scan_single()?
            },

            // Unexpected Character
            Some(unexpected_char) => {
                return Err (
                    LexerError::UnexpectedCharacter (
                        unexpected_char.to_string(),
                        self.position,
                    )
                )
            },

            // End of expression string
            None => None,
        };

        // Successfully scanned a token or whitespace, return accordingly
        Ok(token)
    }

    fn match_next(&mut self, expected: char) -> bool {
        // Peek next character and check for match against the expected
        if self.expr_iter.peek().map_or_else(|| false, |v| v == &expected) {
            // Match succeeded, consume peeked value and update position
            self.consume_character();

            true
        }
        else {
            // Match failed or reached end of expression string
            false
        }
    }

    fn capture_string_literal(&mut self) -> Result<Token, LexerError> {
        let mut string_chars = Vec::new();
        let start = self.position;

        // Continually consume the next character until closing ' or end of expression is encountered
        // If EoE is encountered by this loop condition, we have an unterminated string literal
        while let Some(next_char) = Some(self.consume_character().ok_or_else(
                                        || LexerError::UnterminatedStringLiteral(
                                            string_chars.clone().into_iter().collect(),
                                            start
                                        )
                                    )?).filter(|v| v != &'\'') {
            // Consume the character and add it to the char vector
            string_chars.push(next_char);
        }

        Ok (
            Token::String (
                string_chars.into_iter().collect()
            )
        )
    }

    fn capture_numerical_literal(&mut self, first_char: char) -> Result<Token, LexerError> {
        let start = self.position;
        let mut numerical_chars = vec![first_char];
        let mut decimal_encountered = false;

        // Continually peek the next character until a non-digit or non-dot is encountered
        while let Some(next_char) = self.expr_iter.peek().filter(|v| v.is_digit(10) || v == &&'.') {
            if next_char == &'.' {
                if decimal_encountered {
                    // Already encountered a '.', another is invalid
                    return Err(LexerError::UnexpectedCharacter(next_char.to_string(), self.position+1));
                }
                else {
                    decimal_encountered = true;
                }
            }

            // Push the character onto the vector and consume
            numerical_chars.push(*next_char);
            self.consume_character();
        }

        // Collect string and parse into a Number
        let numerical_string: String = numerical_chars.into_iter().collect();
        numerical_string.parse::<f64>().map(Token::Number).map_err(|err| LexerError::ParseFloatError(err, start))
    }

    fn capture_identifier(&mut self, first_char: char) -> Token {
        let mut identifier_chars = vec![first_char];

        // Continually consume the next character until one that isn't alphanumeric or a '_' is encountered
        while let Some(next_char) = self.expr_iter.peek().filter(|v| v.is_alphanumeric() || v == &&'_') {
            // Push the character onto the vector and consume
            identifier_chars.push(*next_char);
            self.consume_character();
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
            Self::UnterminatedStringLiteral(string, start_pos) => {
                write!(f, "Unterminated string literal '{}' starting at position {}", string, start_pos)
            },

            // Wrappers
            Self::ParseFloatError(parse_err, start) => {
                write!(f, "ParseFloatError '{:?}' encountered in number beginning at position {}", parse_err, start)
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
