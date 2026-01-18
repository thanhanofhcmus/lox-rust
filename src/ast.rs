use crate::{id::Id, span::Span, token::Token};

#[derive(Debug, Clone)]
pub enum Statement {
    Import(ImportNode),
    Expr(Expression),
    Return(Expression),
    If(IfStmtNode),
    While(WhileNode),
    Declare(IdentifierNode, Expression),
    ReassignIden(IdentifierNode, Expression),
    Block(StatementList),
    Global(StatementList),
}

pub type StatementList = Vec<Statement>;

#[derive(Debug, Clone)]
pub struct ImportNode {
    pub path: Span,
    pub iden: IdentifierNode,
}

#[derive(Debug, Clone)]
pub struct WhileNode {
    pub cond: ClauseNode,
    pub body: StatementList,
}

#[derive(Debug, Clone)]
pub struct IfStmtNode {
    pub cond: ClauseNode,
    pub if_stmts: StatementList,
    pub else_stmts: Option<StatementList>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Ternary(TernaryExprNode),
    When(Vec<WhenArmNode>),
    Clause(ClauseNode),
}

#[derive(Debug, Clone)]
pub enum ClauseNode {
    UnaryOp(Box<ClauseNode>, Token),
    BinaryOp(BinaryOpNode),
    Primary(PrimaryNode),
    Chaining(ChainingNode),
}

#[derive(Debug, Clone)]
pub enum PrimaryNode {
    Nil,
    Bool(bool),
    Integer(i64),
    Floating(f64),
    Str(Span),
    ArrayLiteral(ArrayLiteralNode),
    MapLiteral(MapLiteralNode),
    FnDecl(FnDeclNode),
}

#[derive(Debug, Clone)]
pub struct TernaryExprNode {
    pub cond: ClauseNode,
    pub true_clause: ClauseNode,
    pub false_clause: ClauseNode,
}

#[derive(Debug, Clone)]
pub enum ArrayLiteralNode {
    List(Vec<ClauseNode>),
    Repeat(Box<ArrayRepeatNode>),
}

#[derive(Debug, Clone)]
pub struct MapLiteralNode {
    pub nodes: Vec<MapLiteralElementNode>,
}

#[derive(Debug, Clone)]
pub struct MapLiteralElementNode {
    pub key: PrimaryNode,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct ArrayRepeatNode {
    pub value: ClauseNode,
    pub repeat: ClauseNode,
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
pub enum ChainingBase {
    Identifier(IdentifierNode),
    Primary(PrimaryNode),
    Group(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum ChainingFollow {
    Identifier(IdentifierNode), // a.b
    FnCall(FnCallNode),         // a(b, c)
    Index(Box<Expression>),     // a[b + c], array or map indexing
}

#[derive(Debug, Clone)]
pub struct ChainingNode {
    pub base: ChainingBase,
    pub follows: Vec<ChainingFollow>,
}

#[derive(Debug, Clone)]
pub struct BinaryOpNode {
    pub lhs: Box<ClauseNode>,
    pub op: Token,
    pub rhs: Box<ClauseNode>,
}

#[derive(Debug, Clone)]
pub struct WhenArmNode {
    pub cond: ClauseNode,
    pub expr: ClauseNode,
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
