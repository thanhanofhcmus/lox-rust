use crate::{id::Id, token::Token};

#[derive(Debug, Clone)]
pub enum Expression {
    Nil,
    Bool(bool),
    Number(f64),
    Str(String),
    ArrayList(Vec<Expression>),
    ArrayRepeat(Box<ArrayRepeatNode>),
    Ternary(TernaryExprNode),
    When(Vec<CaseNode>),
    UnaryOp(Box<Expression>, Token),
    BinaryOp(BinaryOpNode),
    Identifier(IdentifierNode),
    Index(IndexExprNode),
    FnDecl(FnDeclNode),
    FnCall(FnCallNode),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Import(ImportNode),
    Expr(Expression),
    Return(Expression),
    If(IfStmtNode),
    While(WhileNode),
    Declare(IdentifierNode, Expression),
    ReassignIden(IdentifierNode, Expression),
    ReassignIndex(ReAssignIndexNode),
    Block(StatementList),
    Global(StatementList),

    Print(Vec<Expression>),
}

pub type StatementList = Vec<Statement>;

#[derive(Debug, Clone)]
pub struct IfStmtNode {
    pub cond: Expression,
    pub if_stmts: StatementList,
    pub else_stmts: Option<StatementList>,
}

#[derive(Debug, Clone)]
pub struct IdentifierNode {
    pub name: String,
    pub prefixes: Vec<String>,
}

impl IdentifierNode {
    pub fn new_from_name(name: String) -> Self {
        Self {
            name,
            prefixes: vec![],
        }
    }

    pub fn new_from_vec(mut parts: Vec<String>) -> Self {
        let name = parts.pop().expect("must have at least one part for name");
        Self {
            name,
            prefixes: parts,
        }
    }

    pub fn is_simple(&self) -> bool {
        self.prefixes.is_empty()
    }

    // TODO: move to use span
    pub fn join_dot(&self) -> String {
        self.prefixes
            .iter()
            .cloned()
            .chain(std::iter::once(self.name.clone()))
            .collect::<Vec<_>>()
            .join(".")
    }

    pub fn id(&self) -> Id {
        Id::new(&self.name)
    }
}

#[derive(Debug, Clone)]
pub struct TernaryExprNode {
    pub cond: Box<Expression>,
    pub true_expr: Box<Expression>,
    pub false_expr: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ArrayRepeatNode {
    pub value: Expression,
    pub repeat: Expression,
}

#[derive(Debug, Clone)]
pub struct FnDeclNode {
    pub arg_names: Vec<String>,
    pub body: StatementList,
}

#[derive(Debug, Clone)]
pub struct FnCallNode {
    pub iden: IdentifierNode,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct BinaryOpNode {
    pub lhs: Box<Expression>,
    pub op: Token,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ImportNode {
    pub path: String,
    pub iden: IdentifierNode,
}

#[derive(Debug, Clone)]
pub struct WhileNode {
    pub cond: Expression,
    pub body: StatementList,
}

#[derive(Debug, Clone)]
pub struct CaseNode {
    pub cond: Expression,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct IndexExprNode {
    pub indexer: Box<Expression>,
    pub indexee: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ReAssignIndexNode {
    pub indexer: Expression,
    pub indexee: Expression,
    pub expr: Expression,
}
