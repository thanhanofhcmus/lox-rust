use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Expression {
    Nil,
    Bool(bool),
    Number(f64),
    Str(String),
    Array(Vec<Expression>),
    UnaryOp(Box<Expression>, Token),
    BinaryOp(BinaryOpNode),
    Identifier(String),
    FnDecl(FnDeclNode),
    FnCall(FnCallNode),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    If(IfNode),
    While(WhileNode),
    Declare(String, Expression),
    Reassign(String, Expression),
    Block(StatementList),
    Global(StatementList),

    Print(Expression),
}

pub type StatementList = Vec<Statement>;

#[derive(Debug, Clone)]
pub struct IfNode {
    pub cond: Box<Expression>,
    pub if_stmts: StatementList,
    pub else_stmts: Option<StatementList>,
}

#[derive(Debug, Clone)]
pub struct FnDeclNode {
    pub arg_names: Vec<String>,
    pub body: StatementList,
}

#[derive(Debug, Clone)]
pub struct FnCallNode {
    pub name: String,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct BinaryOpNode {
    pub lhs: Box<Expression>,
    pub op: Token,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct WhileNode {
    pub cond: Expression,
    pub body: StatementList,
}
