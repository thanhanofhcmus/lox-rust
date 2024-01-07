use crate::expr::Expression;
use crate::token::Token;

pub fn calculate(expr: Expression) -> Option<f64> {
    match expr {
        Expression::Number(v) => Some(v),
        Expression::BinaryOp(lhs, op, rhs) => calculate_binary_op(*lhs, op, *rhs),
    }
}

fn calculate_binary_op(lhs: Expression, op: Token, rhs: Expression) -> Option<f64> {
    let (Some(lhs), Some(rhs)) = (calculate(lhs), calculate(rhs) ) else {
      return None
    };

    match op {
        Token::Plus => Some(lhs + rhs),
        Token::Minus => Some(lhs - rhs),
        Token::Star => Some(lhs * rhs),
        Token::Slash => Some(lhs / rhs),
        _ => None,
    }
}
