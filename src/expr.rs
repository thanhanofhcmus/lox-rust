use crate::token::Token;

#[derive(Debug)]
pub enum Expression {
    Bool(bool),
    Number(f64),
    BinaryOp(Box<Expression>, Token, Box<Expression>),
}
