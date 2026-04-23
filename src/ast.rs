use crate::{identifier_registry::Identifier, span::Span, token::Token, types::TypeId};

#[derive(Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub struct AST<T> {
    pub imports: Vec<ImportNode>,
    pub global_stmts: StatementList<T>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeNode {
    BuiltIn(TypeId),
    Array(Box<TypeNode>),
    // Identifier(IdentifierNode),
    Map {
        key: Box<TypeNode>,
        value: Box<TypeNode>,
    },
}

#[derive(Debug, Clone)]
pub enum Statement<T> {
    Declare(DeclareStatementNode<T>),
    StructDecl(StructDeclNode),
    ReassignIden(ChainingReassignTargetNode<T>, Expression<T>),
    Expr(Expression<T>),
}

#[derive(Debug, Clone)]
pub struct StructFieldNode {
    pub iden: Identifier,
    pub explicit_type: Option<TypeNode>,
}

#[derive(Debug, Clone)]
pub struct StructDeclNode {
    pub iden: Identifier,
    pub fields: Vec<StructFieldNode>,
}

#[derive(Debug, Clone)]
pub struct DeclareStatementNode<T> {
    pub iden: Identifier,
    /// User-declared type annotation. `None` means no annotation was written.
    pub explicit_type: Option<TypeNode>,
    pub expr: Expression<T>,
}

pub type StatementList<T> = Vec<Statement<T>>;

#[derive(Debug, Clone, Copy)]
pub enum ChainStep<S> {
    /// Subscription step like `a[0]`. Payload is the index (expression or value
    /// depending on the phase).
    Subscription(S),
    /// Member step like `p.x`. Payload is the field identifier (Id + span).
    Member(Identifier),
}

#[derive(Debug, Clone)]
pub struct ChainingReassignTargetNode<T> {
    pub base: Identifier,
    /// Steps in source order (outermost-first). For `a[0][1] = x` → `[0, 1]`;
    /// for `p.x = y` → `[.x]`; for `b.center.x = z` → `[.center, .x]`.
    pub follows: Vec<ChainStep<ClauseNode<T>>>,
}

#[derive(Debug, Clone)]
pub struct BlockNode<T> {
    pub extra: T,
    pub stmts: StatementList<T>,
    pub last_expr: Option<Box<Expression<T>>>,
}

impl BlockNode<()> {
    pub fn new(stmts: StatementList<()>, last_expr: Option<Box<Expression<()>>>) -> Self {
        Self {
            extra: (),
            stmts,
            last_expr,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportNode {
    pub path: Span,
    pub iden: Identifier,
}

#[derive(Debug, Clone)]
pub struct Expression<T> {
    pub extra: T,
    pub case: ExprCase<T>,
}

impl Expression<()> {
    pub fn new(case: ExprCase<()>) -> Self {
        Self { extra: (), case }
    }
}

#[derive(Debug, Clone)]
pub enum ExprCase<T> {
    When(WhenNode<T>),
    Clause(ClauseNode<T>),
    Block(BlockNode<T>),
    IfChain(IfChainNode<T>),
    While(WhileNode<T>),
    For(ForNode<T>),
    Return(Option<Box<Expression<T>>>),
}

#[derive(Debug, Clone)]
pub struct IfChainNode<T> {
    pub if_node: ElseIfNode<T>,
    pub else_if_nodes: Vec<ElseIfNode<T>>,
    pub else_stmts: Option<BlockNode<T>>,
}

#[derive(Debug, Clone)]
pub struct ElseIfNode<T> {
    pub cond: ClauseNode<T>,
    pub stmts: BlockNode<T>,
}

#[derive(Debug, Clone)]
pub struct ForNode<T> {
    pub iden: Identifier,
    pub collection: ClauseNode<T>,
    pub body: BlockNode<T>,
}

#[derive(Debug, Clone)]
pub struct WhileNode<T> {
    pub cond: ClauseNode<T>,
    pub body: BlockNode<T>,
}

#[derive(Debug, Clone)]
pub struct WhenArmNode<T> {
    pub cond: ClauseNode<T>,
    pub expr: ClauseNode<T>,
}

#[derive(Debug, Clone)]
pub struct WhenNode<T> {
    pub arms: Vec<WhenArmNode<T>>,
}

#[derive(Debug, Clone)]
pub struct ClauseNode<T> {
    pub extra: T,
    pub case: ClauseCase<T>,
}

impl ClauseNode<()> {
    pub fn new(case: ClauseCase<()>) -> Self {
        Self { extra: (), case }
    }
}

#[derive(Debug, Clone)]
pub enum ClauseCase<T> {
    Unary(Box<ClauseNode<T>>, Token),
    Binary(BinaryOpNode<T>),
    RawValue(RawValueNode<T>),
    Group(Box<ClauseNode<T>>),
    Identifier(Identifier),
    Subscription(SubscriptionNode<T>),
    FnCall(FnCallNode<T>),
    MemberAccess(MemberAccessNode<T>),
}

#[derive(Debug, Clone)]
pub struct SubscriptionNode<T> {
    pub indexer: Box<ClauseNode<T>>,
    pub indexee: Box<ClauseNode<T>>,
}

#[derive(Debug, Clone)]
pub struct MemberAccessNode<T> {
    pub object: Box<ClauseNode<T>>,
    pub field: Identifier,
}

#[derive(Debug, Clone)]
pub struct FnCallNode<T> {
    pub caller: Box<ClauseNode<T>>,
    pub args: Vec<Expression<T>>,
}

#[derive(Debug, Clone)]
pub enum RawValueNode<T> {
    Scalar(ScalarNode),
    ArrayLiteral(ArrayLiteralNode<T>),
    MapLiteral(MapLiteralNode<T>),
    StructLiteral(StructLiteralNode<T>),
    FnDecl(FnDeclNode<T>),
}

#[derive(Debug, Clone)]
pub enum ScalarNode {
    Nil,
    Bool(bool),
    Integer(i64),
    Floating(f64),
    LazyStr {
        span: Span,
        is_raw: bool,
    },
    /// Store the actual string, for REPL intepreter
    LiteralStr(String),
}

#[derive(Debug, Clone)]
pub struct StructLiteralFieldNode<T> {
    pub iden: Identifier,
    pub value: ClauseNode<T>,
}

#[derive(Debug, Clone)]
pub struct StructLiteralNode<T> {
    pub iden: Identifier,
    pub fields: Vec<StructLiteralFieldNode<T>>,
}

#[derive(Debug, Clone)]
pub struct MapLiteralNode<T> {
    pub nodes: Vec<MapLiteralElementNode<T>>,
}

#[derive(Debug, Clone)]
pub struct MapLiteralElementNode<T> {
    pub key: ScalarNode,
    pub value: ClauseNode<T>,
}

#[derive(Debug, Clone)]
pub enum ArrayLiteralNode<T> {
    List(Vec<ClauseNode<T>>),
    Repeat(Box<ArrayRepeatNode<T>>),
    ForComprehension(Box<ArrayForComprehensionNode<T>>),
}

#[derive(Debug, Clone)]
pub struct ArrayRepeatNode<T> {
    pub value: ClauseNode<T>,
    pub repeat: ClauseNode<T>,
}

#[derive(Debug, Clone)]
pub struct ArrayForComprehensionNode<T> {
    pub iden: Identifier,
    pub collection: ClauseNode<T>,
    pub transformer: ClauseNode<T>,
    pub filter: Option<ClauseNode<T>>,
}

#[derive(Debug, Clone)]
pub struct FnParamNode {
    pub iden: Identifier,
    pub explicit_type: Option<TypeNode>,
}

#[derive(Debug, Clone)]
pub struct FnDeclNode<T> {
    pub params: Vec<FnParamNode>,
    pub body: BlockNode<T>,
    pub return_type: Option<TypeNode>,
}

#[derive(Debug, Clone)]
pub struct BinaryOpNode<T> {
    pub lhs: Box<ClauseNode<T>>,
    pub op: Token,
    pub rhs: Box<ClauseNode<T>>,
}
