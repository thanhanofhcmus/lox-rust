use crate::expr::Expression;
use crate::token::Token;
use std::collections::HashMap;

const NUMBER_DELTA: f64 = 1e-10;

#[derive(Debug)]
pub enum Error {
    UnknownOperation(Token),
    InvalidOperationOnType(Token, Value),
    MismatchType(Value, Value),
    DivideByZero,
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Str(String),
    Number(f64),
    Bool(bool),
}

pub struct Interpreter {
    values: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            values: HashMap::new(),
        }
    }

    pub fn calculate(&self, expr: Expression) -> Result<Value, Error> {
        match expr {
            Expression::Nil => Ok(Value::Nil),
            Expression::Str(v) => Ok(Value::Str(v.to_string())),
            Expression::Bool(v) => Ok(Value::Bool(v)),
            Expression::Number(v) => Ok(Value::Number(v)),
            Expression::UnaryOp(expr, op) => self.calculate_unary_op(*expr, op),
            Expression::BinaryOp(lhs, op, rhs) => self.calculate_binary_op(*lhs, op, *rhs),
        }
    }

    fn calculate_unary_op(&self, expr: Expression, op: Token) -> Result<Value, Error> {
        let res = self.calculate(expr)?;
        match op {
            Token::Bang => match res {
                Value::Bool(v) => Ok(Value::Bool(!v)),
                _ => Err(Error::InvalidOperationOnType(op, res)),
            },
            Token::Minus => match res {
                Value::Number(v) => Ok(Value::Number(-v)),
                _ => Err(Error::InvalidOperationOnType(op, res)),
            },
            _ => Err(Error::UnknownOperation(op)),
        }
    }

    fn calculate_binary_op(
        &self,
        lhs: Expression,
        op: Token,
        rhs: Expression,
    ) -> Result<Value, Error> {
        let lhs = self.calculate(lhs)?;
        let rhs = self.calculate(rhs)?;

        match op {
            Token::Plus => add(&lhs, &rhs),
            Token::Minus => subtract(&lhs, &rhs),
            Token::Star => times(&lhs, &rhs),
            Token::Slash => divide(&lhs, &rhs),
            Token::And | Token::Or => and_or(&lhs, op, &rhs),
            Token::EqualEqual | Token::BangEqual => is_equal(&lhs, &rhs),
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                compare(&lhs, op, &rhs)
            }
            _ => Err(Error::UnknownOperation(op)),
        }
    }
}

fn is_equal(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    Ok(Value::Bool(match lhs {
        Value::Nil => false,
        Value::Bool(l) => match rhs {
            Value::Bool(r) => l == r,
            _ => false,
        },
        Value::Number(l) => match rhs {
            Value::Number(r) => f64::abs(*l - *r) < NUMBER_DELTA,
            _ => false,
        },
        Value::Str(l) => match rhs {
            Value::Str(r) => l == r,
            _ => false,
        },
    }))
}

fn compare(lhs: &Value, op: Token, rhs: &Value) -> Result<Value, Error> {
    let &Value::Number(l) = lhs else {
        return Err(Error::InvalidOperationOnType(op, lhs.clone()))
    };
    let &Value::Number(r) = rhs else {
        return Err(Error::InvalidOperationOnType(op, rhs.clone()))
    };

    match op {
        Token::Less => Ok(Value::Bool(l < r)),
        Token::LessEqual => Ok(Value::Bool(l <= r)),
        Token::Greater => Ok(Value::Bool(l > r)),
        Token::GreaterEqual => Ok(Value::Bool(l >= r)),
        _ => Err(Error::InvalidOperationOnType(op, lhs.clone())),
    }
}

fn and_or(lhs: &Value, op: Token, rhs: &Value) -> Result<Value, Error> {
    let &Value::Bool(l) = lhs else {
        return Err(Error::InvalidOperationOnType(op, lhs.clone()))
    };
    let &Value::Bool(r) = rhs else {
        return Err(Error::InvalidOperationOnType(op, rhs.clone()))
    };

    match op {
        Token::And => Ok(Value::Bool(l && r)),
        Token::Or => Ok(Value::Bool(l || r)),
        _ => Err(Error::UnknownOperation(op)),
    }
}

fn add(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    match lhs {
        Value::Number(l) => match rhs {
            Value::Number(r) => Ok(Value::Number(l + r)),
            _ => Err(Error::MismatchType(lhs.clone(), rhs.clone())),
        },
        Value::Str(l) => match rhs {
            Value::Str(r) => Ok(Value::Str(l.to_owned() + r)),
            _ => Err(Error::MismatchType(lhs.clone(), rhs.clone())),
        },
        _ => Err(Error::InvalidOperationOnType(Token::Plus, lhs.clone())),
    }
}

fn subtract(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    let Value::Number(l) = lhs else {
        return Err(Error::InvalidOperationOnType(Token::Minus, lhs.clone()))
    };
    let Value::Number(r) = rhs else {
        return Err(Error::InvalidOperationOnType(Token::Minus, rhs.clone()))
    };

    Ok(Value::Number(l - r))
}

fn times(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    let (Value::Number(l), Value::Number(r)) = (lhs, rhs) else {
     return Err(Error::InvalidOperationOnType(Token::Star, lhs.clone()))
    };

    Ok(Value::Number(l * r))
}

fn divide(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    let Value::Number(l) = lhs else {
        return Err(Error::InvalidOperationOnType(Token::Slash, lhs.clone()))
    };
    let Value::Number(r) = rhs else {
        return Err(Error::InvalidOperationOnType(Token::Minus, rhs.clone()))
    };

    if *r == 0.0 {
        return Err(Error::DivideByZero);
    }

    Ok(Value::Number(l / r))
}
