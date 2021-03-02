/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : interpreter/parser.rs

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

    Precedence Hierarchy:
    expression     → equality ;
    equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    term           → factor ( ( "-" | "+" ) factor )* ;
    factor         → unary ( ( "/" | "*" ) unary )* ;
    unary          → ( "!" | "-" ) unary
                     | primary ;
    primary        → NUMBER | STRING | "true" | "false" | "null"
                     | "(" expression ")" ;

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
    convert::TryFrom,
    error::Error,
    fmt,
    iter::Peekable,
    slice::Iter,
};

use crate::interpreter::{
    Expression,
    InterpreterError,
    Literal,
    Operator,
    Token,
    Unary,
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

pub struct Parser<'i> {
    token_iter: Peekable<Iter<'i, Token>>,
    position:   u32,
}

//OPT: *STYLE*  Parameterize this enum for better Display output
#[derive(Debug, PartialEq)]
pub enum ParserError {
    InvalidOperatorConversion(Token),
    UnterminatedGroup,
    InvalidPrimaryToken(Token),
}

pub type ParserResult = Result<Expression, ParserError>;


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl<'i> Parser<'i> {
    pub fn new(tokens: &'i [Token]) -> Self {
        Self {
            token_iter: tokens.iter().peekable(),
            position:   0,
        }
    }


    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */

    pub fn parse(&mut self) -> ParserResult {
        self.expression()
    }

    
    /*  *  *  *  *  *  *  *\
     *   Helper Methods   *
    \*  *  *  *  *  *  *  */

    // expression → equality ;
    fn expression(&mut self) -> ParserResult {
        self.equality()
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> ParserResult {
        let mut expr = self.comparison()?;

        let equivalence_tokens = [
            Some(&&Token::EqualEqual),
            Some(&&Token::BangEqual),
            ];
        while equivalence_tokens.contains(&self.token_iter.peek()) {
            let operator = Operator::try_from(self.consume_token().unwrap()).unwrap();
            let right = self.comparison()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> ParserResult {
        let mut expr = self.term()?;

        let comparison_tokens = [
            Some(&&Token::GreaterThan),
            Some(&&Token::GreaterThanOrEqualTo),
            Some(&&Token::LessThan),
            Some(&&Token::LessThanOrEqualTo),
        ];
        while comparison_tokens.contains(&self.token_iter.peek()) {
            let operator = Operator::try_from(self.consume_token().unwrap()).unwrap();
            let right = self.term()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // term → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> ParserResult {
        let mut expr = self.factor()?;

        let arithmetic_tokens = [
            Some(&&Token::Minus),
            Some(&&Token::Plus),
            ];
        while arithmetic_tokens.contains(&self.token_iter.peek()) {
            let operator = Operator::try_from(self.consume_token().unwrap()).unwrap();
            let right = self.factor()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // factor → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> ParserResult {
        let mut expr = self.unary()?;

        let arithmetic_tokens = [
            Some(&&Token::Slash),
            Some(&&Token::Star),
            ];
        while arithmetic_tokens.contains(&self.token_iter.peek()) {
            let operator = Operator::try_from(self.consume_token().unwrap()).unwrap();
            let right = self.unary()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // unary → ( "!" | "-" ) unary
    //         | primary ;
    fn unary(&mut self) -> ParserResult {
        match self.token_iter.peek() {
            Some(&&Token::Bang)  => {
                self.consume_token();
                Ok(Expression::Unary(Unary::Not(Box::new(self.unary()?))))
            },
            Some(&&Token::Minus) => {
                self.consume_token();
                Ok(Expression::Unary(Unary::Negation(Box::new(self.unary()?))))
            },
            _ => self.primary(),
        }
    }

    // primary → NUMBER | STRING | "true" | "false" | "null"
    //           | "(" expression ")" ;
    fn primary(&mut self) -> ParserResult {
        match self.consume_token() {
            Some(&Token::Integer(value))                => Ok(Expression::Literal(Literal::Integer(value))),
            Some(&Token::Float(value))                  => Ok(Expression::Literal(Literal::Float(value))),
            Some(&Token::String(ref string))            => Ok(Expression::Literal(Literal::String(string.clone()))),
            Some(&Token::Identifier(ref identifier))    => {
                match identifier.as_str() {
                    "true"  => Ok(Expression::Literal(Literal::True)),
                    "false" => Ok(Expression::Literal(Literal::False)),
                    "null"  => Ok(Expression::Literal(Literal::Null)),
                    _       => Ok(Expression::Identifier(identifier.clone())),
                }
            },
            Some(&Token::LeftParen)                     => {
                let expr = self.expression()?;

                // Check for trailing ')', return error if it's missing
                if let Some(Token::RightParen) = self.token_iter.peek() {
                    // Found the trailing ')', consume it and return the Grouping
                    self.consume_token();

                    Ok(Expression::Grouping(Box::new(expr)))
                }
                else {
                    Err(ParserError::UnterminatedGroup)
                }
            }
            Some(unexpected) => Err(ParserError::InvalidPrimaryToken(unexpected.clone())),
            _ => panic!("EoE!?"),
        }
    }

    fn consume_token(&mut self) -> Option<&Token> {
        self.position += 1;

        self.token_iter.next()
    }

}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////


/*  *  *  *  *  *  *  *\
 *    ParserError     *
\*  *  *  *  *  *  *  */

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidOperatorConversion(token) => {
                write!(f, "Invalid Token->Operator conversion for token '{:?}'", token)
            },
            Self::UnterminatedGroup => {
                write!(f, "Unterminated expression group")
            },
            Self::InvalidPrimaryToken(token) => {
                write!(f, "Invalid token '{:?}' encountered at the Primary precedence level", token)
            },
        }
    }
}

impl From<InterpreterError> for ParserError {
    fn from(src: InterpreterError) -> Self {
        match src {
            InterpreterError::InvalidOperatorConversion(token) => {
                ParserError::InvalidOperatorConversion(token)
            }
            _ => {
                //FIXME: This sucks, should probably be re-designed
                panic!("Uh, not great")
            }
        }
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {

    use std::{
        convert::TryFrom,
        error::Error,
    };

    use crate::interpreter::{
        Token,
        Expression,
        InterpreterError,
        Literal,
        Unary,
        Operator,
        ArithmeticOperator,
        lexer::Lexer,
        parser::{
            Parser,
            ParserError,
        }
    };


    type TestResult = Result<(), Box<dyn Error>>;


    #[test]
    fn output_test() -> TestResult {
        let expr_str = "-123 * (45.67)";

        // Tokenize the expression string
        let mut lexer = Lexer::new(expr_str);
        let tokens = lexer.scan()?;
        
        // Parse the tokens
        let mut parser = Parser::new(&tokens);
        let expr = parser.parse()?;

        eprintln!("Expression '{}' \"pretty\"-printed:\n{}", expr_str, expr);

        let pretty_expr = format!("{}", expr);
        assert_eq!(
            pretty_expr,
            "(* (- 123) (group 45.67))"
        );

        Ok(())
    }

    #[test]
    fn invalid_operator_conversion() -> TestResult {
        let valid_token = Token::Plus;
        let invalid_token = Token::LeftBrace;

        assert_eq!(
            Operator::try_from(&valid_token),
            Ok(Operator::Arithmetic(ArithmeticOperator::Plus))
        );

        assert_eq!(
            Operator::try_from(&invalid_token),
            Err(InterpreterError::InvalidOperatorConversion(invalid_token))
        );
        
        Ok(())
    }

    #[test]
    fn unterm_group() -> TestResult {
        let valid_tokens = vec![
            Token::Integer(1),
            Token::Plus,
            Token::LeftParen,
            Token::Integer(2),
            Token::RightParen,
        ];
        let invalid_tokens = vec![
            Token::Integer(1),
            Token::Plus,
            Token::LeftParen,
            Token::Integer(2),
        ];
        
        let mut valid_parser = Parser::new(&valid_tokens);
        assert_eq!(
            valid_parser.parse(),
            Ok(
                Expression::Binary(
                    Box::new(Expression::Literal(Literal::Integer(1))),
                    Operator::Arithmetic(ArithmeticOperator::Plus),
                    Box::new(Expression::Grouping(
                        Box::new(Expression::Literal(Literal::Integer(2)))
                    ))
                )
            )
        );

        let mut invalid_parser = Parser::new(&invalid_tokens);
        assert_eq!(
            invalid_parser.parse(),
            Err(ParserError::UnterminatedGroup)
        );
        
        Ok(())
    }

    #[test]
    fn invalid_primary_token() -> TestResult {
        let valid_tokens = vec![
            Token::Minus,
            Token::Integer(1),
        ];
        let invalid_tokens = vec![
            Token::Minus,
            Token::Plus,
        ];

        let mut valid_parser = Parser::new(&valid_tokens);
        assert_eq!(
            valid_parser.parse(),
            Ok(
                Expression::Unary(Unary::Negation(
                    Box::new(Expression::Literal(Literal::Integer(1)))
                ))
            )
        );

        let mut invalid_parser = Parser::new(&invalid_tokens);
        assert_eq!(
            invalid_parser.parse(),
            Err(ParserError::InvalidPrimaryToken(Token::Plus))
        );

        Ok(())
    }
}
