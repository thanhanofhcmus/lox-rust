use crate::token::Token;

#[derive(Debug)]
pub enum Expression {
    Number(f64),
    BinaryOp(Box<Expression>, Token, Box<Expression>),
}
