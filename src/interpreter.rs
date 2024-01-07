use crate::ast::{Expression, Statement};
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

    pub fn interpret_stmt(&mut self, stmt: Statement) -> Result<Option<Value>, Error> {
        match stmt {
            Statement::Assign(name, expr) => {
                let value = self.interpret_expr(expr)?;
                self.values.insert(name, value);
                Ok(None)
            }
            Statement::Expr(expr) => self.interpret_expr(expr).map(Some),
            Statement::MultiStmts(mut stmts) => {
                if let Some(last) = stmts.pop() {
                    for s in stmts {
                        self.interpret_stmt(s)?;
                    }
                    self.interpret_stmt(last)
                } else {
                    Ok(None)
                }
            }
        }
    }

    pub fn interpret_expr(&self, expr: Expression) -> Result<Value, Error> {
        match expr {
            Expression::Identifier(name) => Ok(self.get_value(name.as_str())),
            Expression::Nil => Ok(Value::Nil),
            Expression::Str(v) => Ok(Value::Str(v.to_string())),
            Expression::Bool(v) => Ok(Value::Bool(v)),
            Expression::Number(v) => Ok(Value::Number(v)),
            Expression::UnaryOp(expr, op) => self.interpret_unary_op(*expr, op),
            Expression::BinaryOp(lhs, op, rhs) => self.interpret_binary_op(*lhs, op, *rhs),
        }
    }

    fn get_value(&self, name: &str) -> Value {
        self.values
            .get(name)
            .map(|v| v.to_owned())
            .unwrap_or(Value::Nil)
    }

    fn interpret_unary_op(&self, expr: Expression, op: Token) -> Result<Value, Error> {
        let res = self.interpret_expr(expr)?;
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

    fn interpret_binary_op(
        &self,
        lhs: Expression,
        op: Token,
        rhs: Expression,
    ) -> Result<Value, Error> {
        let lhs = self.interpret_expr(lhs)?;
        let rhs = self.interpret_expr(rhs)?;

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
    let from_ord = |o: std::cmp::Ordering| match op {
        Token::Less => Ok(Value::Bool(o.is_lt())),
        Token::LessEqual => Ok(Value::Bool(o.is_le())),
        Token::Greater => Ok(Value::Bool(o.is_gt())),
        Token::GreaterEqual => Ok(Value::Bool(o.is_ge())),
        _ => Err(Error::UnknownOperation(op)),
    };
    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
        return from_ord(l.total_cmp(r));
    }
    if let (Value::Str(l), Value::Str(r)) = (lhs, rhs) {
        return from_ord(l.cmp(r));
    }
    Err(Error::InvalidOperationOnType(op, lhs.clone()))
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
