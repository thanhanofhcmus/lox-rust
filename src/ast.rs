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
    FunctionDeclaration(Vec<String> /*args*/, StatementList),
    FunctionCall(String /* name */, Vec<Expression> /* args */),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    If(Expression, StatementList, Option<StatementList>),
    While(Expression, StatementList),
    Declare(String, Expression),
    Reassign(String, Expression),
    Block(StatementList),
    Global(StatementList),

    Print(Expression),
}

pub type StatementList = Vec<Statement>;
