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
    This module defines the rules behind evaluating ECMAScript expressions.
    
    It leverages Rust's robust enums and trait system to represent ECMAScript
    values as Intermediate values, and the trait system allows for easy use
    of the values in the evaluation functions.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{
    error::Error,
    fmt,
};

use crate::{
    datamodel::SystemVariables,
    interpreter::{
        ArithmeticOperator,
        EcmaScriptValue,
        Expression,
        Literal,
        LogicalOperator,
        Operator,
        Unary,
    },
};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

pub struct Evaluator<'sv> {
    expr:       Expression,
    sys_vars:   &'sv SystemVariables,
}

#[derive(Debug, PartialEq)]
pub enum EvaluatorError {
    StringNegation(String),
    UnknownNegation(Box<Expression>),
    IllegalOperation(
        ArithmeticOperator, // Operator
        EcmaScriptValue,  // Left Operand
        EcmaScriptValue,  // Right Operand
    ),
    IdentifierNotFound(String /* Name */),

    CouldNotDowncast,
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

//OPT: *PERFORMANCE* Expressions can get pretty big... would be best to use a reference here if possible
impl<'sv> Evaluator<'sv> {
    pub fn new(expr: Expression, sys_vars: &'sv SystemVariables)  -> Self {
        Self {
            expr,
            sys_vars,
        }
    }


    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */

    pub fn evaluate(&self) -> Result<EcmaScriptValue, EvaluatorError> {
        //OPT: *DESIGN* Bad clone()... Figure out how to use a reference correctly
        self.eval_subexpr(self.expr.clone())
    }
    
    /*  *  *  *  *  *  *  *\
     *  Helper Functions  *
    \*  *  *  *  *  *  *  */
    
    fn eval_subexpr(&self, expr: Expression) -> Result<EcmaScriptValue, EvaluatorError> {
        match expr {
            Expression::Literal(literal)        => Ok(Self::eval_literal(literal)),
            Expression::Identifier(name)        => self.eval_identifier(name),
            Expression::Unary(unary)            => self.eval_unary(unary),
            Expression::Binary(left, op, right) => self.eval_binary(op, *left, *right),
            Expression::Grouping(inner_expr)    => self.eval_subexpr(*inner_expr),
        }
    }

    fn eval_identifier(&self, name: String) -> Result<EcmaScriptValue, EvaluatorError> {
        // Look up the identifier in the data map
        if let Some(value) = self.sys_vars._x().get(&name) {
            Ok(value.clone())
        }
        else {
            // Identifier was not found in the data map, return error
            Err(EvaluatorError::IdentifierNotFound(name))
        }
    }

    fn eval_unary(&self, unary: Unary) -> Result<EcmaScriptValue, EvaluatorError> {
        match unary {
            Unary::Negation(expr) => {
                match self.eval_subexpr(*expr)? {
                    EcmaScriptValue::String(value) => {
                        // String negation is an error
                        Err(EvaluatorError::StringNegation(value))
                    },
                    EcmaScriptValue::Number(value) => {
                        // Negate value and return
                        Ok(EcmaScriptValue::Number(-value))
                    },
                    EcmaScriptValue::Boolean(value_is_true) => {
                        if value_is_true {
                            // Negation of 'true' evaluates to '-1.0'
                            Ok(EcmaScriptValue::Number(-1.0))
                        }
                        else {
                            // Negation of 'false' evaluates to '-0.0'
                            Ok(EcmaScriptValue::Number(-0.0))
                        }
                    },
                    EcmaScriptValue::Null => {
                        // Negation of 'null' evaluates to '-0.0'
                        Ok(EcmaScriptValue::Number(-0.0))
                    },
                }
            },
            Unary::Not(expr) => {
                match self.eval_subexpr(*expr)? {
                    EcmaScriptValue::String(_value) => {
                        // !String evaluates to 'false', because reasons
                        Ok(EcmaScriptValue::Boolean(false))
                    },
                    EcmaScriptValue::Number(value) => {
                        // !Number evaluates to 'true' for 0, 'false' for all other values
                        if (value - 0.0).abs() < f64::EPSILON {
                            Ok(EcmaScriptValue::Boolean(true))
                        }
                        else {
                            Ok(EcmaScriptValue::Boolean(false))
                        }
                    },
                    EcmaScriptValue::Boolean(value_is_true) => {
                        // Invert value and return
                        Ok(EcmaScriptValue::Boolean(!value_is_true))
                    },
                    EcmaScriptValue::Null => {
                        // !'null' evaluates to 'true'
                        Ok(EcmaScriptValue::Boolean(true))
                    },
                }
            },
        }
    }

    fn eval_binary(&self, op: Operator, left: Expression, right: Expression) -> Result<EcmaScriptValue, EvaluatorError> {
        let left_value = self.eval_subexpr(left)?;
        let right_value = self.eval_subexpr(right)?;

        match op {
            /* Arithmetic Operations */
            Operator::Arithmetic(math_op) => {
                Self::eval_math_op(math_op, left_value, right_value)
            },

            /* Logical Operations */
            Operator::Logical(logic_op) => {
                Ok(EcmaScriptValue::Boolean(Self::eval_logic_op(logic_op, left_value, right_value)))
            },
        }
    }

    
    /*  *  *  *  *  *  *  *\
     *   Helper Methods   *
    \*  *  *  *  *  *  *  */

    fn eval_literal(literal: Literal) -> EcmaScriptValue {
        match literal {
            Literal::String(value)  => EcmaScriptValue::String(value),
            Literal::Integer(value) => EcmaScriptValue::Number(value as f64),
            Literal::Float(value)   => EcmaScriptValue::Number(value),
            Literal::True           => EcmaScriptValue::Boolean(true),
            Literal::False          => EcmaScriptValue::Boolean(false),
            Literal::Null           => EcmaScriptValue::Null,
        }
    }

    fn eval_math_op(op: ArithmeticOperator, left: EcmaScriptValue, right: EcmaScriptValue) -> Result<EcmaScriptValue, EvaluatorError> {
        match op {
            ArithmeticOperator::Plus    => Ok(left + right),
            ArithmeticOperator::Minus   => left - right,
            ArithmeticOperator::Star    => left * right,
            ArithmeticOperator::Slash   => left / right,
        }
    }

    fn eval_logic_op(op: LogicalOperator, left: EcmaScriptValue, right: EcmaScriptValue) -> bool {
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
            Self::IdentifierNotFound(name) => {
                write!(f, "Identifier '{}' not found in Data Model'", name)
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

    use crate::{
        datamodel::SystemVariables,
        interpreter::{
            ArithmeticOperator,
            Expression,
            EcmaScriptValue,
            Literal,
            LogicalOperator,
            Operator,
            evaluator::{
                Evaluator,
            },
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
        let sys_vars = SystemVariables::default();
        let evaluator = Evaluator::new(expr.clone(), &sys_vars);

        eprintln!("Evaluating Expression '{}'...", expr.clone());

        // Attempt to evaluate the expression
        let result_as_iv = evaluator.evaluate()?;
        eprintln!("Expression '{}' == {:?}", expr, result_as_iv);

        assert_eq!(
            result_as_iv,
            EcmaScriptValue::Boolean(float_val == int_val as f64)
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
        let sys_vars = SystemVariables::default();
        let evaluator = Evaluator::new(expr.clone(), &sys_vars);

        eprintln!("Evaluating Expression '{}'...", expr.clone());

        // Attempt to evaluate the expression
        let result_as_iv = evaluator.evaluate()?;
        eprintln!("Expression '{}' == {:?}", expr, result_as_iv);

        assert_eq!(
            result_as_iv,
            EcmaScriptValue::Number(f64::INFINITY)
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
        let sys_vars = SystemVariables::default();
        let evaluator = Evaluator::new(expr.clone(), &sys_vars);

        eprintln!("Evaluating Expression '{}'...", expr.clone());

        // Attempt to evaluate the expression
        let result_as_iv = evaluator.evaluate()?;
        eprintln!("Expression '{}' == {:?}", expr, result_as_iv);

        assert_eq!(
            result_as_iv,
            EcmaScriptValue::String(float_val.to_string() + &int_val.to_string())
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
        let sys_vars = SystemVariables::default();
        let evaluator = Evaluator::new(expr.clone(), &sys_vars);

        eprintln!("Evaluating Expression '{}'...", expr.clone());

        // Attempt to evaluate the expression
        let result_as_iv = evaluator.evaluate()?;
        eprintln!("Expression '{}' == {:?}", expr, result_as_iv);

        assert_eq!(
            result_as_iv,
            EcmaScriptValue::Boolean(false)
        );

        Ok(())
    }
}
