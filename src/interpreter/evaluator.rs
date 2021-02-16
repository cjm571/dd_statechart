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
    expr: Box<Expression>,
}


#[derive(Debug, PartialEq)]
pub enum EvaluatorError {
    StringNegation(String),
    UnknownNegation(Box<Expression>),
    UnsupportedOperandTypes(ArithmeticOperator, Box<Expression>, Box<Expression>),

    CouldNotDowncast,
}

//FIXME: PUT THIS SOMEWHERE BETTER
//FIXME: All external calls to .as_any() are questionable
pub trait EvaluatedValue: fmt::Debug {
    fn as_any(&self) -> &dyn Any;
    fn equals(&self, other: &dyn EvaluatedValue) -> bool;
    fn order(&self, other: &dyn EvaluatedValue) -> Option<Ordering>;
}
impl PartialEq for dyn EvaluatedValue {
    fn eq(&self, other: &Self) -> bool {
        self.equals(other)
    }
}
impl PartialOrd for dyn EvaluatedValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.order(other)
    }
}

impl<T: 'static + PartialEq + PartialOrd + fmt::Debug> EvaluatedValue for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn equals(&self, other: &dyn EvaluatedValue) -> bool {
        other
            .as_any()
            .downcast_ref::<T>()
            .map_or(
                false,
                |x| self == x
            )
    }

    fn order(&self, other: &dyn EvaluatedValue) -> Option<Ordering> {
        other
            .as_any()
            .downcast_ref::<T>()
            .and_then(|x| self.partial_cmp(x))
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Evaluator {
    pub fn new(expr: Box<Expression>)  -> Self {
        Self { expr }
    }


    /*  *  *  *  *  *  *  *\
     *  Utility Methods   *
    \*  *  *  *  *  *  *  */

    pub fn evaluate(&mut self) -> Result<Box<dyn EvaluatedValue>, EvaluatorError> {
        //FIXME: Suss clone()
        Self::eval_expr(self.expr.clone())
    }

    
    /*  *  *  *  *  *  *  *\
     *   Helper Methods   *
    \*  *  *  *  *  *  *  */

    fn eval_expr(expr: Box<Expression>) -> Result<Box<dyn EvaluatedValue>, EvaluatorError> {
        //FIXME: DEBUG DELETE
        eprintln!("*** ENTER eval_expr ***");

        match *expr {
            Expression::Literal(literal)        => {
                //FIXME: DEBUG DELETE and REVERT
                // Ok(Self::eval_literal(literal))

                let value = Self::eval_literal(literal);
                if let Some(value) = value.as_any().downcast_ref::<&i32>() {
                    eprintln!("Successfully downcast to &i32!");
                    eprintln!("Returning '{}' TypeId: {:?}\n", value, value.as_any().type_id());
                }
                
                Ok(Box::new(value))
            },
            Expression::Unary(unary)            => Ok(Self::eval_unary(unary)?),
            Expression::Binary(left, op, right) => Ok(Self::eval_binary(op, left, right)?),
            Expression::Grouping(inner_expr)    => Self::eval_expr(inner_expr),
        }
    }

    fn eval_literal(literal: Literal) -> Box<dyn EvaluatedValue> {
        //FIXME: DEBUG DELETE
        eprintln!("*** ENTER eval_literal ***");

        match literal {
            Literal::Integer(value) => {
                //FIXME: DEBUG DELETE
                let value_as_any = &value as &dyn Any;
                eprintln!("Returning '{}' TypeId: {:?}\n", value, value_as_any.type_id());
                Box::new(value)
            },
            Literal::Float(value) => {
                //FIXME: DEBUG DELETE
                let value_as_any = &value as &dyn Any;
                eprintln!("Returning '{}' TypeId: {:?}\n", value, value_as_any.type_id());
                Box::new(value)
            },
            Literal::String(value) => {
                //FIXME: DEBUG DELETE
                let value_as_any = &value as &dyn Any;
                eprintln!("Returning '{}' TypeId: {:?}\n", value, value_as_any.type_id());
                Box::new(value)
            },
            Literal::True => {
                //FIXME: DEBUG DELETE
                let value = true;
                let value_as_any = &value as &dyn Any;
                eprintln!("Returning '{}' TypeId: {:?}\n", value, value_as_any.type_id());
                Box::new(true)
            },
            Literal::False => {
                //FIXME: DEBUG DELETE
                let value = false;
                let value_as_any = &value as &dyn Any;
                eprintln!("Returning '{}' TypeId: {:?}\n", value, value_as_any.type_id());
                Box::new(false)
            },
            Literal::Null => {
                //FIXME: DEBUG DELETE and REVERT
                // return None
                let value = -99999;
                let value_as_any = &value as &dyn Any;
                eprintln!("Returning '{}' TypeId: {:?}\n", value, value_as_any.type_id());
                Box::new(-99999)
            },
        }
    }

    fn eval_unary(unary: Unary) -> Result<Box<dyn EvaluatedValue>, EvaluatorError> {
        match unary {
            Unary::Negation(expr) => {
                let value = Self::eval_expr(expr.clone())?;
                
                // First check if the value is a String, as this is an error
                if let Some(as_string) = value.as_any().downcast_ref::<String>() {
                    return Err(EvaluatorError::StringNegation(as_string.clone()));
                }

                // Booleans must be handled differently, because the
                // Negation operator in ECMAscript changes their type to
                // an integer
                if let Some(as_bool) = value.as_any().downcast_ref::<bool>() {
                    // '-true' evaluates to -1, because reasons
                    if as_bool == &true {
                        return Ok(Box::new(-1));
                    }
                    // '-false' evaluates to -0, so just return 0
                    else {
                        return Ok(Box::new(0));
                    }
                }

                // The remaining variants can be handled by simple negation
                if let Some(as_int) = value.as_any().downcast_ref::<i32>() {
                    return Ok(Box::new(-as_int));
                }
                if let Some(as_float) = value.as_any().downcast_ref::<f64>() {
                    return Ok(Box::new(-as_float));
                }

                // Value could not be downcast into any of the expected types, return error
                Err(EvaluatorError::UnknownNegation(expr.clone()))

                //FIXME: HANDLE NULL
            },
            Unary::Not(expr) => {
                let value = Self::eval_expr(expr.clone())?;
                
                // First check if the value is bool - we can actually do something
                // sensible with those
                if let Some(as_bool) = value.as_any().downcast_ref::<bool>() {
                    Ok(Box::new(!as_bool))
                }
                // All non-bool values evaluate to 'false' when Notted
                else {
                    Ok(Box::new(false))
                }
                //FIXME: HANDLE NULL
            },
        }
    }

    fn eval_binary(op: Operator, left: Box<Expression>, right: Box<Expression>) -> Result<Box<dyn EvaluatedValue>, EvaluatorError> {
        //FIXME: DEBUG DELETE
        eprintln!("*** ENTER eval_binary ***");
        
        //FIXME: Bad clone()...
        let left_value = Self::eval_expr(left.clone())?;
        let right_value = Self::eval_expr(right.clone())?;

        match op {
            /* Arithmetic Operations */
            Operator::Arithmetic(math_op) => {
                // String Concatenation
                if let (Some(left_as_string), Some(right_as_string)) = (left_value.as_any().downcast_ref::<String>(), right_value.as_any().downcast_ref::<String>()) {
                    return Ok(Box::new(format!("{}{}", left_as_string, right_as_string)));
                }

                // Integer Addition
                if let (Some(left_as_int), Some(right_as_int)) = (left_value.as_any().downcast_ref::<i32>(), right_value.as_any().downcast_ref::<i32>()) {
                    //FIXME: DEBUG DELETE
                    eprintln!("Successfully downcast to i32/i32!");
                    eprintln!("Returning '{}'", left_as_int + right_as_int);
                    
                    return Ok(Box::new(left_as_int + right_as_int));
                }

                // FP Addition
                if let (Some(left_as_float), Some(right_as_float)) = (left_value.as_any().downcast_ref::<f64>(), right_value.as_any().downcast_ref::<f64>()) {
                    //FIXME: DEBUG DELETE
                    eprintln!("Successfully downcast to f64/f64!");
                    eprintln!("Returning '{}'", left_as_float + right_as_float);
                    
                    return Ok(Box::new(left_as_float + right_as_float));
                }

                // Mixed-Number Addition (casts Integers to Floats)
                if let (Some(left_as_int), Some(right_as_float)) = (left_value.as_any().downcast_ref::<i32>(), right_value.as_any().downcast_ref::<f64>()) {
                    //FIXME: DEBUG DELETE
                    eprintln!("Successfully downcast to i32/f64!");
                    eprintln!("Returning '{}'", *left_as_int as f64 + right_as_float);
                    
                    return Ok(Box::new(*left_as_int as f64 + right_as_float));
                }
                if let (Some(left_as_float), Some(right_as_int)) = (left_value.as_any().downcast_ref::<f64>(), right_value.as_any().downcast_ref::<i32>()) {
                    //FIXME: DEBUG DELETE
                    eprintln!("Successfully downcast to f64/i32!");
                    eprintln!("Returning '{}'", left_as_float + *right_as_int as f64);
                    
                    return Ok(Box::new(left_as_float + *right_as_int as f64));
                }

                // Unsupported operand types
                Err(EvaluatorError::UnsupportedOperandTypes(math_op, left, right))
            },

            /* Logical Operations */
            Operator::Logical(logic_op) => {
                Ok(Box::new(Self::eval_logic_op(logic_op, left_value, right_value)))
            },
        }
    }

    fn eval_logic_op<T: PartialEq + PartialOrd>(op: LogicalOperator, left: T, right: T) -> bool {
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
            Self::UnsupportedOperandTypes(op, left, right) => {
                write!(f, "Unsupported operation '{}' on types L: '{}' and R: '{}'", op, left, right)
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

    use std::{
        any::Any,
        error::Error,
    };

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

    #[test]
    fn arithmetic_evaluation() -> TestResult {
        // Create an Evaluator object loaded with a binary arithmetic expression
        let expr = Expression::Binary(
            Box::new(Expression::Literal(Literal::Float(1.5))),
            Operator::Arithmetic(ArithmeticOperator::Plus),
            Box::new(Expression::Literal(Literal::Integer(2))),
        );
        let mut evaluator = Evaluator::new(Box::new(expr.clone()));

        eprintln!("Evaluating Expression '{}'...", expr.clone());

        // Attempt to evaluate the expression
        let result = evaluator.evaluate()?;
        eprintln!("Intermediate Result: {:?} ({:?})", result, result.type_id());
        
        let float_as_any: &dyn Any = &5.0f64;
        let int_as_any: &dyn Any = &-5i32;

        eprintln!("COMPARISON");
        eprintln!("f64: {:?} {:?}", float_as_any, float_as_any.type_id());
        eprintln!("i32: {:?} {:?}", int_as_any, int_as_any.type_id());


        let casted_result = result.as_any().downcast_ref::<f64>().unwrap();

        eprintln!("Expression '{}' evaluated to {}", expr, casted_result);

        Ok(())
    }
    
    //FEAT: *FIX* 2.0 == 2 evaluates to false due to differing types
    //OPT: *TESTING* Develop a method to test large variety of value/type combos
    #[test]
    fn logical_evaluation() -> TestResult {
        let float_val = 2.5;
        let int_val = 2;

        // Create an Evaluator object loaded with a binary arithmetic expression
        let expr = Expression::Binary(
            Box::new(Expression::Literal(Literal::Float(float_val))),
            Operator::Logical(LogicalOperator::EqualTo),
            Box::new(Expression::Literal(Literal::Integer(int_val))),
        );
        let mut evaluator = Evaluator::new(Box::new(expr.clone()));

        eprintln!("Evaluating Expression '{}'...", expr.clone());

        // Attempt to evaluate the expression
        let result = evaluator.evaluate()?;
        eprintln!("Intermediate Result: {:?} ({:?})", result, result.type_id());
        
        let float_as_any: &dyn Any = &5.0f64;
        let int_as_any: &dyn Any = &-5i32;

        eprintln!("COMPARISON");
        eprintln!("f64: {:?} {:?}", float_as_any, float_as_any.type_id());
        eprintln!("i32: {:?} {:?}", int_as_any, int_as_any.type_id());


        let casted_result = result.as_any().downcast_ref::<bool>().unwrap();

        eprintln!("Expression '{}' evaluated to {}", expr, casted_result);

        assert_eq!(
            casted_result,
            &(float_val == int_val as f64)
        );

        Ok(())
    }
}