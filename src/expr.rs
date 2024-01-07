use crate::token::Token;

#[derive(Debug)]
pub enum Expression {
    Nil,
    Bool(bool),
    Number(f64),
    UnaryOp(Box<Expression>, Token),
    BinaryOp(Box<Expression>, Token, Box<Expression>),
}
