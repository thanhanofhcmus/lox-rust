use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Expression {
    Nil,
    Bool(bool),
    Number(f64),
    Str(String),
    Array(Vec<Expression>),
    UnaryOp(Box<Expression>, Token),
    BinaryOp(Box<Expression>, Token, Box<Expression>),
    Identifier(String),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    If(Expression, StatementList, Option<StatementList>),
    While(Expression, StatementList),
    Assign(String, Expression),
    Block(StatementList),
    Global(StatementList),
}

pub type StatementList = Vec<Statement>;
