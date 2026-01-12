use crate::{id::Id, span::Span, token::Token};

#[derive(Debug, Clone)]
pub enum Expression {
    Nil,
    Bool(bool),
    Number(f64),
    Str(Span),
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
    // TODO: maybe create a struct that store these both as parts
    pub name: Span,
    pub name_id: Id,
    pub prefixes: Vec<Span>,
    pub prefix_ids: Vec<Id>,
}

impl IdentifierNode {
    pub fn new_from_name(name: Span, input: &str) -> Self {
        Self {
            name,
            name_id: Id::new(name.str_from_source(input)),
            prefixes: vec![],
            prefix_ids: vec![],
        }
    }

    pub fn new_from_vec(mut parts: Vec<Span>, input: &str) -> Self {
        let name = parts.pop().expect("must have at least one part for name");
        let prefix_ids = parts
            .iter()
            .map(|s| Id::new(s.str_from_source(input)))
            .collect();
        Self {
            name,
            name_id: Id::new(name.str_from_source(input)),
            prefixes: parts,
            prefix_ids,
        }
    }

    pub fn is_simple(&self) -> bool {
        self.prefixes.is_empty()
    }

    pub fn create_name(&self, input: &str) -> String {
        self.prefixes
            .iter()
            .chain(std::iter::once(&self.name))
            .map(|span| span.string_from_source(input))
            .collect::<Vec<_>>()
            .join(".")
    }

    pub fn get_id(&self) -> Id {
        self.name_id
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
    pub arg_names: Vec<IdentifierNode>,
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
    pub path: Span,
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
