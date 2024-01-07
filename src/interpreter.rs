use crate::expr::Expression;
use crate::token::Token;

const NUMBER_DELTA: f64 = 1e-10;

#[derive(Debug)]
pub enum Error {
    UnknownOperation(Token),
    InvalidOperationOnType(Token, CalcResult),
    MismatchType(CalcResult, CalcResult),
    DivideByZero,
}

#[derive(Debug, Clone)]
pub enum CalcResult {
    Number(f64),
    Bool(bool),
}

fn compare(lhs: &CalcResult, rhs: &CalcResult) -> Result<CalcResult, Error> {
    Ok(CalcResult::Bool(match lhs {
        CalcResult::Bool(l) => match rhs {
            CalcResult::Bool(r) => l == r,
            _ => false,
        },
        CalcResult::Number(l) => match rhs {
            CalcResult::Number(r) => f64::abs(*l - *r) < NUMBER_DELTA,
            _ => false,
        },
    }))
}

fn and_or(lhs: &CalcResult, op: Token, rhs: &CalcResult) -> Result<CalcResult, Error> {
    let &CalcResult::Bool(l) = lhs else {
        return Err(Error::InvalidOperationOnType(op, lhs.clone()))
    };
    let &CalcResult::Bool(r) = rhs else {
        return Err(Error::InvalidOperationOnType(op, rhs.clone()))
    };

    match op {
        Token::And => Ok(CalcResult::Bool(l && r)),
        Token::Or => Ok(CalcResult::Bool(l || r)),
        _ => Err(Error::UnknownOperation(op)),
    }
}

fn add(lhs: &CalcResult, rhs: &CalcResult) -> Result<CalcResult, Error> {
    match lhs {
        CalcResult::Bool(_) => Err(Error::InvalidOperationOnType(Token::Plus, lhs.clone())),
        &CalcResult::Number(l) => match rhs {
            &CalcResult::Number(r) => Ok(CalcResult::Number(l + r)),
            _ => Err(Error::MismatchType(lhs.clone(), rhs.clone())),
        },
    }
}

fn subtract(lhs: &CalcResult, rhs: &CalcResult) -> Result<CalcResult, Error> {
    let CalcResult::Number(l) = lhs else {
        return Err(Error::InvalidOperationOnType(Token::Minus, lhs.clone()))
    };
    let CalcResult::Number(r) = rhs else {
        return Err(Error::InvalidOperationOnType(Token::Minus, rhs.clone()))
    };

    Ok(CalcResult::Number(l - r))
}

fn times(lhs: &CalcResult, rhs: &CalcResult) -> Result<CalcResult, Error> {
    let (CalcResult::Number(l), CalcResult::Number(r)) = (lhs, rhs) else {
     return Err(Error::InvalidOperationOnType(Token::Star, lhs.clone()))
    };

    Ok(CalcResult::Number(l * r))
}

fn divide(lhs: &CalcResult, rhs: &CalcResult) -> Result<CalcResult, Error> {
    let CalcResult::Number(l) = lhs else {
        return Err(Error::InvalidOperationOnType(Token::Slash, lhs.clone()))
    };
    let CalcResult::Number(r) = rhs else {
        return Err(Error::InvalidOperationOnType(Token::Minus, rhs.clone()))
    };

    if *r == 0.0 {
        return Err(Error::DivideByZero);
    }

    Ok(CalcResult::Number(l / r))
}

pub fn calculate(expr: Expression) -> Result<CalcResult, Error> {
    match expr {
        Expression::Bool(v) => Ok(CalcResult::Bool(v)),
        Expression::Number(v) => Ok(CalcResult::Number(v)),
        Expression::UnaryOp(expr, op) => calculate_unary_op(*expr, op),
        Expression::BinaryOp(lhs, op, rhs) => calculate_binary_op(*lhs, op, *rhs),
    }
}

fn calculate_unary_op(expr: Expression, op: Token) -> Result<CalcResult, Error> {
    let res = calculate(expr)?;
    match op {
        Token::Bang => match res {
            CalcResult::Bool(v) => Ok(CalcResult::Bool(!v)),
            _ => Err(Error::InvalidOperationOnType(op, res)),
        },
        Token::Minus => match res {
            CalcResult::Number(v) => Ok(CalcResult::Number(-v)),
            _ => Err(Error::InvalidOperationOnType(op, res)),
        },
        _ => Err(Error::UnknownOperation(op)),
    }
}

fn calculate_binary_op(lhs: Expression, op: Token, rhs: Expression) -> Result<CalcResult, Error> {
    let lhs = calculate(lhs)?;
    let rhs = calculate(rhs)?;

    match op {
        Token::Plus => add(&lhs, &rhs),
        Token::Minus => subtract(&lhs, &rhs),
        Token::Star => times(&lhs, &rhs),
        Token::Slash => divide(&lhs, &rhs),
        Token::And | Token::Or => and_or(&lhs, op, &rhs),
        Token::EqualEqual | Token::BangEqual => compare(&lhs, &rhs),
        _ => Err(Error::UnknownOperation(op)),
    }
}
