use crate::{id::Id, span::Span, token::Token, types::Type};

#[derive(Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub struct AST<T> {
    pub imports: Vec<ImportNode>,
    pub global_stmts: StatementList<T>,
}

#[derive(Debug, Clone)]
pub enum Statement<T> {
    Declare(DeclareStatementNode<T>),
    ReassignIden(ChainingReassignTargetNode<T>, Expression<T>),
    Expr(Expression<T>),
}

#[derive(Debug, Clone)]
pub struct DeclareStatementNode<T> {
    pub iden: IdentifierNode,
    /// User-declared type annotation. `None` means no annotation was written.
    pub type_: Option<Type>,
    pub expr: Expression<T>,
}

pub type StatementList<T> = Vec<Statement<T>>;

#[derive(Debug, Clone)]
pub struct ChainingReassignTargetNode<T> {
    pub base: IdentifierNode,
    pub follows: Vec<ClauseNode<T>>,
}

#[derive(Debug, Clone)]
pub struct BlockNode<T> {
    pub type_: T,
    pub stmts: StatementList<T>,
    pub last_expr: Option<Box<Expression<T>>>,
}

impl BlockNode<()> {
    pub fn new(stmts: StatementList<()>, last_expr: Option<Box<Expression<()>>>) -> Self {
        Self {
            type_: (),
            stmts,
            last_expr,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportNode {
    pub path: Span,
    pub iden: IdentifierNode,
}

#[derive(Debug, Clone)]
pub struct Expression<T> {
    pub type_: T,
    pub case: ExprCase<T>,
}

impl Expression<()> {
    pub fn new(case: ExprCase<()>) -> Self {
        Self { type_: (), case }
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
    pub iden: IdentifierNode,
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
    pub type_: T,
    pub case: ClauseCase<T>,
}

impl ClauseNode<()> {
    pub fn new(case: ClauseCase<()>) -> Self {
        Self { type_: (), case }
    }
}

#[derive(Debug, Clone)]
pub enum ClauseCase<T> {
    Unary(Box<ClauseNode<T>>, Token),
    Binary(BinaryOpNode<T>),
    RawValue(RawValueNode<T>),
    Group(Box<ClauseNode<T>>),
    Identifier(IdentifierNode),
    Subscription(SubscriptionNode<T>),
    FnCall(FnCallNode<T>),
}

#[derive(Debug, Clone)]
pub struct SubscriptionNode<T> {
    pub indexer: Box<ClauseNode<T>>,
    pub indexee: Box<ClauseNode<T>>,
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
    StrLiteral(String),
}

#[derive(Debug, Clone)]
pub enum ArrayLiteralNode<T> {
    List(Vec<ClauseNode<T>>),
    Repeat(Box<ArrayRepeatNode<T>>),
    ForComprehension(Box<ArrayForComprehentionNode<T>>),
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
pub struct ArrayRepeatNode<T> {
    pub value: ClauseNode<T>,
    pub repeat: ClauseNode<T>,
}

#[derive(Debug, Clone)]
pub struct ArrayForComprehentionNode<T> {
    pub iden: IdentifierNode,
    pub collection: ClauseNode<T>,
    pub transformer: ClauseNode<T>,
    pub filter: Option<ClauseNode<T>>,
}

#[derive(Debug, Clone)]
pub struct FnParamNode {
    pub id: IdentifierNode,
    pub type_: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct FnDeclNode<T> {
    pub params: Vec<FnParamNode>,
    pub body: BlockNode<T>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct BinaryOpNode<T> {
    pub lhs: Box<ClauseNode<T>>,
    pub op: Token,
    pub rhs: Box<ClauseNode<T>>,
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

impl From<&IdentifierNode> for Id {
    fn from(val: &IdentifierNode) -> Self {
        val.get_id()
    }
}

// Type aliases for pipeline stages
pub type UntypedAST = AST<()>;
pub type TypedAST = AST<Type>;
pub type UntypedBlockNode = BlockNode<()>;
pub type TypedBlockNode = BlockNode<Type>;
pub type UntypedExpr = Expression<()>;
pub type TypedExpr = Expression<Type>;
pub type UntypedClause = ClauseNode<()>;
pub type TypedClause = ClauseNode<Type>;
