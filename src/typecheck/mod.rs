use std::collections::HashMap;

use thiserror::Error;

use crate::{ast::*, id::Id, span::Span, token::Token, types::Type};

/// Scoped symbol table mapping `Id` → `Type` for typechecking.
/// The outermost scope is seeded with interpreter builtins (see `BUILTIN_NAMES`).
pub struct Environment {
    scopes: Vec<HashMap<Id, Type>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut builtins = HashMap::new();
        for &name in BUILTIN_NAMES {
            builtins.insert(Id::new(name), Type::Any);
        }
        // Scope 0 holds builtins; scope 1 is the user-global scope. Keeping them
        // separate lets user code shadow builtins without a redeclaration error.
        Self {
            scopes: vec![builtins, HashMap::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("scope underflow");
    }

    /// Declare `id` in the current (innermost) scope. Returns `Err` with the
    /// existing type if `id` is already bound *in the same scope*. Shadowing
    /// a binding from an outer scope is allowed and is not an error.
    pub fn declare(&mut self, id: Id, ty: Type) -> Result<(), Type> {
        let scope = self.scopes.last_mut().expect("at least one scope");
        use std::collections::hash_map::Entry;
        match scope.entry(id) {
            Entry::Occupied(o) => Err(o.get().clone()),
            Entry::Vacant(v) => {
                v.insert(ty);
                Ok(())
            }
        }
    }

    pub fn lookup(&self, id: Id) -> Option<&Type> {
        self.scopes.iter().rev().find_map(|s| s.get(&id))
    }
}

/// Keep in sync with `src/interpret/predule.rs::create()`. Registered as
/// `Type::Any` — we don't model their signatures yet.
const BUILTIN_NAMES: &[&str] = &[
    "print",
    "assert",
    "from_json",
    "to_json",
    "array_len",
    "array_push",
    "array_pop",
    "array_insert",
    "map_length",
    "map_keys",
    "map_values",
    "map_insert",
    "map_remove",
    "_dbg_print",
    "_dbg_state",
    "_dbg_gc_mark",
    "_dbg_gc_sweep",
    "_dbg_gc_mark_sweep",
    "_dbg_heap_stats",
];

pub struct TypeChecker<'cl, 'sl> {
    pub(super) environment: &'cl mut Environment,
    pub(super) input: &'sl str,
}

impl<'cl, 'sl> TypeChecker<'cl, 'sl> {
    pub fn new(environment: &'cl mut Environment, input: &'sl str) -> Self {
        Self { environment, input }
    }

    /// Run `f` with a fresh scope pushed on the environment. The scope is
    /// popped whether `f` succeeds or errors.
    fn with_scope<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.environment.push_scope();
        let result = f(self);
        self.environment.pop_scope();
        result
    }

    /// Consume an untyped AST and return a typed AST with all declarations type-checked.
    pub fn convert(&mut self, ast: UntypedAST) -> Result<TypedAST, Error> {
        let AST {
            imports,
            global_stmts: untyped_global_stmts,
        } = ast;

        let mut global_stmts = Vec::with_capacity(untyped_global_stmts.len());

        for ut_stmt in untyped_global_stmts {
            let stmt = self.convert_stmt(ut_stmt)?;
            global_stmts.push(stmt);
        }

        Ok(AST {
            imports,
            global_stmts,
        })
    }

    fn convert_stmt(&mut self, ut_stmt: Statement<()>) -> Result<Statement<Type>, Error> {
        match ut_stmt {
            Statement::Declare(DeclareStatementNode { iden, type_, expr }) => {
                let expr = self.convert_expr(expr)?;
                match &type_ {
                    None | Some(Type::Any) => {}
                    Some(type_annot) if *type_annot == expr.type_ => {}
                    Some(_) => {
                        return Err(Error::ExplitcitTypeDeclartionMismatchActualType(
                            iden.name,
                            iden.name_id,
                        ));
                    }
                }
                // Annotation wins when written (including `Any`); otherwise infer from expr.
                let var_type = type_.clone().unwrap_or_else(|| expr.type_.clone());
                self.environment
                    .declare(iden.name_id, var_type)
                    .map_err(|_| Error::DuplicateDeclaration(iden.name, iden.name_id))?;
                Ok(Statement::Declare(DeclareStatementNode {
                    expr,
                    iden,
                    type_,
                }))
            }
            Statement::ReassignIden(target, expr) => {
                let target = self.convert_reassign_target(target)?;
                let expr = self.convert_expr(expr)?;
                Ok(Statement::ReassignIden(target, expr))
            }
            Statement::Expr(expr) => {
                let expr = self.convert_expr(expr)?;
                Ok(Statement::Expr(expr))
            }
        }
    }

    fn convert_reassign_target(
        &mut self,
        target: ChainingReassignTargetNode<()>,
    ) -> Result<ChainingReassignTargetNode<Type>, Error> {
        if target.base.is_simple() && self.environment.lookup(target.base.name_id).is_none() {
            return Err(Error::UndefinedIdentifier(
                target.base.name,
                target.base.name_id,
            ));
        }
        let follows = target
            .follows
            .into_iter()
            .map(|c| self.convert_clause(c))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(ChainingReassignTargetNode {
            base: target.base,
            follows,
        })
    }

    fn convert_expr(&mut self, ut_expr: UntypedExpr) -> Result<TypedExpr, Error> {
        match ut_expr.case {
            ExprCase::Clause(clause) => {
                let clause = self.convert_clause(clause)?;
                Ok(Expression {
                    type_: clause.type_.clone(),
                    case: ExprCase::Clause(clause),
                })
            }
            _ => Ok(Expression {
                type_: Type::Any,
                case: self.convert_expr_case(ut_expr.case)?,
            }),
        }
    }

    fn convert_expr_case(&mut self, case: ExprCase<()>) -> Result<ExprCase<Type>, Error> {
        match case {
            ExprCase::Clause(clause) => Ok(ExprCase::Clause(self.convert_clause(clause)?)),
            ExprCase::Block(block) => Ok(ExprCase::Block(self.convert_block(block)?)),
            ExprCase::IfChain(if_chain) => Ok(ExprCase::IfChain(self.convert_if_chain(if_chain)?)),
            ExprCase::While(while_node) => Ok(ExprCase::While(self.convert_while(while_node)?)),
            ExprCase::For(for_node) => Ok(ExprCase::For(self.convert_for(for_node)?)),
            ExprCase::When(arms) => {
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        Ok(WhenArmNode {
                            cond: self.convert_clause(arm.cond)?,
                            expr: self.convert_clause(arm.expr)?,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                Ok(ExprCase::When(arms))
            }
            ExprCase::Return(expr) => {
                let expr = expr
                    .map(|e| self.convert_expr(*e).map(Box::new))
                    .transpose()?;
                Ok(ExprCase::Return(expr))
            }
        }
    }

    fn convert_block(&mut self, block: BlockNode<()>) -> Result<BlockNode<Type>, Error> {
        self.with_scope(|this| {
            let stmts = block
                .stmts
                .into_iter()
                .map(|s| this.convert_stmt(s))
                .collect::<Result<Vec<_>, _>>()?;
            let last_expr = block
                .last_expr
                .map(|e| this.convert_expr(*e).map(Box::new))
                .transpose()?;
            let type_ = last_expr
                .as_ref()
                .map(|e| e.type_.clone())
                .unwrap_or(Type::Unit);
            Ok(BlockNode {
                type_,
                stmts,
                last_expr,
            })
        })
    }

    fn convert_if_chain(&mut self, node: IfChainNode<()>) -> Result<IfChainNode<Type>, Error> {
        Ok(IfChainNode {
            if_node: self.convert_else_if(node.if_node)?,
            else_if_nodes: node
                .else_if_nodes
                .into_iter()
                .map(|n| self.convert_else_if(n))
                .collect::<Result<Vec<_>, _>>()?,
            else_stmts: node.else_stmts.map(|b| self.convert_block(b)).transpose()?,
        })
    }

    fn convert_else_if(&mut self, node: ElseIfNode<()>) -> Result<ElseIfNode<Type>, Error> {
        Ok(ElseIfNode {
            cond: self.convert_clause(node.cond)?,
            stmts: self.convert_block(node.stmts)?,
        })
    }

    fn convert_while(&mut self, node: WhileNode<()>) -> Result<WhileNode<Type>, Error> {
        Ok(WhileNode {
            cond: self.convert_clause(node.cond)?,
            body: self.convert_block(node.body)?,
        })
    }

    fn convert_for(&mut self, node: ForNode<()>) -> Result<ForNode<Type>, Error> {
        let collection = self.convert_clause(node.collection)?;
        // TODO: derive iden type from the collection's element type once array/map
        // element types are used during lookup. Permissive `Any` for now.
        let iden_name = node.iden.name;
        let iden_id = node.iden.name_id;
        self.with_scope(|this| {
            this.environment
                .declare(iden_id, Type::Any)
                .map_err(|_| Error::DuplicateDeclaration(iden_name, iden_id))?;
            let body = this.convert_block(node.body)?;
            Ok(ForNode {
                iden: node.iden,
                collection,
                body,
            })
        })
    }

    fn convert_clause(&mut self, ut_clause: UntypedClause) -> Result<TypedClause, Error> {
        let (type_, case) = match ut_clause.case {
            ClauseCase::RawValue(raw) => {
                let (type_, raw) = self.convert_raw_value(raw)?;
                (type_, ClauseCase::RawValue(raw))
            }
            ClauseCase::Identifier(iden) => {
                // Prefixed identifiers (e.g. `foo.bar`) are module/member access — not
                // yet modeled by the typechecker. Accept them permissively as `Any`.
                let type_ = if iden.is_simple() {
                    self.environment
                        .lookup(iden.name_id)
                        .cloned()
                        .ok_or(Error::UndefinedIdentifier(iden.name, iden.name_id))?
                } else {
                    Type::Any
                };
                (type_, ClauseCase::Identifier(iden))
            }
            ClauseCase::Group(inner) => {
                let inner = self.convert_clause(*inner)?;
                let type_ = inner.type_.clone();
                (type_, ClauseCase::Group(Box::new(inner)))
            }
            ClauseCase::Unary(operand, op) => {
                let operand = self.convert_clause(*operand)?;
                let type_ = self.type_unary_op(op, &operand.type_)?;
                (type_, ClauseCase::Unary(Box::new(operand), op))
            }
            ClauseCase::Binary(bin) => {
                let lhs = Box::new(self.convert_clause(*bin.lhs)?);
                let rhs = Box::new(self.convert_clause(*bin.rhs)?);
                let type_ = self.type_binary_op(bin.op, &lhs.type_, &rhs.type_)?;
                (
                    type_,
                    ClauseCase::Binary(BinaryOpNode {
                        lhs,
                        op: bin.op,
                        rhs,
                    }),
                )
            }
            ClauseCase::Subscription(sub) => {
                let sub = SubscriptionNode {
                    indexer: Box::new(self.convert_clause(*sub.indexer)?),
                    indexee: Box::new(self.convert_clause(*sub.indexee)?),
                };
                (Type::Any, ClauseCase::Subscription(sub))
            }
            ClauseCase::FnCall(call) => {
                let caller = Box::new(self.convert_clause(*call.caller)?);
                let args = call
                    .args
                    .into_iter()
                    .map(|e| self.convert_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;
                (Type::Any, ClauseCase::FnCall(FnCallNode { caller, args }))
            }
        };
        Ok(ClauseNode { type_, case })
    }

    fn convert_raw_value(
        &mut self,
        raw: RawValueNode<()>,
    ) -> Result<(Type, RawValueNode<Type>), Error> {
        match raw {
            RawValueNode::Scalar(s) => {
                let type_ = match &s {
                    ScalarNode::Nil => Type::Nil,
                    ScalarNode::Bool(_) => Type::Bool,
                    ScalarNode::Integer(_) | ScalarNode::Floating(_) => Type::Number,
                    ScalarNode::LazyStr { .. } | ScalarNode::StrLiteral(_) => Type::Str,
                };
                Ok((type_, RawValueNode::Scalar(s)))
            }
            RawValueNode::ArrayLiteral(arr) => {
                let (elem_ty, arr) = self.convert_array_literal(arr)?;
                Ok((
                    Type::Array(Box::new(elem_ty)),
                    RawValueNode::ArrayLiteral(arr),
                ))
            }
            RawValueNode::MapLiteral(map) => {
                let mut nodes = Vec::with_capacity(map.nodes.len());
                for elem in map.nodes {
                    if !scalar_is_string(&elem.key) {
                        return Err(Error::MapKeyMustBeString(elem.key));
                    }
                    let value = self.convert_clause(elem.value)?;
                    nodes.push(MapLiteralElementNode {
                        key: elem.key,
                        value,
                    });
                }
                let value_ty = unify_types(nodes.iter().map(|n| &n.value.type_));
                Ok((
                    Type::Map {
                        value: Box::new(value_ty),
                    },
                    RawValueNode::MapLiteral(MapLiteralNode { nodes }),
                ))
            }
            RawValueNode::FnDecl(fn_decl) => {
                // Params are declared in a fresh scope surrounding the body. The
                // body itself opens its own scope via `convert_block`, so param
                // shadowing by locals is handled naturally.
                self.with_scope(|this| {
                    for param in &fn_decl.params {
                        let ty = param.type_.clone().unwrap_or(Type::Any);
                        this.environment
                            .declare(param.id.name_id, ty)
                            .map_err(|_| {
                                Error::DuplicateDeclaration(param.id.name, param.id.name_id)
                            })?;
                    }
                    let body = this.convert_block(fn_decl.body)?;
                    // TODO: derive signature from params/return_type
                    Ok((
                        Type::Function {
                            params: vec![],
                            return_: Box::new(Type::Any),
                        },
                        RawValueNode::FnDecl(FnDeclNode {
                            params: fn_decl.params,
                            body,
                            return_type: fn_decl.return_type,
                        }),
                    ))
                })
            }
        }
    }

    /// Compute the result type of a binary operator applied to operand types.
    ///
    /// Opinionated choices (adjust if your language differs):
    /// - `+` does numeric add OR string concat (same-typed operands).
    /// - `-` `*` `/` `%` are numbers only.
    /// - `<` `<=` `>` `>=` are numbers only (no lexicographic string compare).
    /// - `==` `!=` require both sides to be the same type.
    /// - `and` `or` require both sides to be `Bool`.
    /// - `Any` on either side is permissive: the op is accepted and the
    ///   result is the "natural" output type (Number for arithmetic, Bool for
    ///   comparison/logical, Any for `+` when we can't disambiguate).
    fn type_binary_op(&self, op: Token, lhs: &Type, rhs: &Type) -> Result<Type, Error> {
        use Token::*;
        let any = matches!(lhs, Type::Any) || matches!(rhs, Type::Any);
        let mismatch = || Error::BinaryOpTypeMismatch(op, lhs.clone(), rhs.clone());

        match op {
            Plus => match (lhs, rhs) {
                (Type::Number, Type::Number) => Ok(Type::Number),
                (Type::Str, Type::Str) => Ok(Type::Str),
                _ if any => {
                    if matches!(lhs, Type::Str) || matches!(rhs, Type::Str) {
                        Ok(Type::Str)
                    } else if matches!(lhs, Type::Number) || matches!(rhs, Type::Number) {
                        Ok(Type::Number)
                    } else {
                        Ok(Type::Any)
                    }
                }
                _ => Err(mismatch()),
            },
            Minus | Star | Slash | Percentage => match (lhs, rhs) {
                (Type::Number, Type::Number) => Ok(Type::Number),
                _ if any => Ok(Type::Number),
                _ => Err(mismatch()),
            },
            EqualEqual | BangEqual => {
                if lhs == rhs || any {
                    Ok(Type::Bool)
                } else {
                    Err(mismatch())
                }
            }
            Less | LessEqual | Greater | GreaterEqual => match (lhs, rhs) {
                (Type::Number, Type::Number) | (Type::Str, Type::Str) => Ok(Type::Bool),
                _ if any => Ok(Type::Bool),
                _ => Err(mismatch()),
            },
            And | Or => match (lhs, rhs) {
                (Type::Bool, Type::Bool) => Ok(Type::Bool),
                _ if any => Ok(Type::Bool),
                _ => Err(mismatch()),
            },
            _ => unreachable!("non-binary-op token in BinaryOpNode: {:?}", op),
        }
    }

    /// Compute the result type of a unary operator applied to an operand type.
    ///
    /// Opinionated choices:
    /// - `-` (Minus) negates numbers.
    /// - `not` (Not) inverts Bools.
    /// - `Any` is permissive: accept and return the op's natural result type.
    fn type_unary_op(&self, op: Token, operand: &Type) -> Result<Type, Error> {
        use Token::*;
        let any = matches!(operand, Type::Any);
        let mismatch = || Error::UnaryOpTypeMismatch(op, operand.clone());

        match op {
            Minus => match operand {
                Type::Number => Ok(Type::Number),
                _ if any => Ok(Type::Number),
                _ => Err(mismatch()),
            },
            Not => match operand {
                Type::Bool => Ok(Type::Bool),
                _ if any => Ok(Type::Bool),
                _ => Err(mismatch()),
            },
            _ => unreachable!("non-unary-op token in Unary: {:?}", op),
        }
    }

    /// Typecheck an array literal and return the array's *element type* alongside
    /// the converted node. The caller wraps it in `Type::Array(...)`.
    fn convert_array_literal(
        &mut self,
        arr: ArrayLiteralNode<()>,
    ) -> Result<(Type, ArrayLiteralNode<Type>), Error> {
        match arr {
            ArrayLiteralNode::List(items) => {
                let items = items
                    .into_iter()
                    .map(|c| self.convert_clause(c))
                    .collect::<Result<Vec<_>, _>>()?;
                let elem_ty = unify_types(items.iter().map(|c| &c.type_));
                Ok((elem_ty, ArrayLiteralNode::List(items)))
            }
            ArrayLiteralNode::Repeat(rep) => {
                let value = self.convert_clause(rep.value)?;
                let repeat = self.convert_clause(rep.repeat)?;
                require_type(&Type::Number, &repeat.type_)?;
                let elem_ty = value.type_.clone();
                Ok((
                    elem_ty,
                    ArrayLiteralNode::Repeat(Box::new(ArrayRepeatNode { value, repeat })),
                ))
            }
            ArrayLiteralNode::ForComprehension(comp) => {
                let ArrayForComprehentionNode {
                    iden,
                    collection,
                    transformer,
                    filter,
                } = *comp;
                let collection = self.convert_clause(collection)?;
                // TODO: derive iden type from collection's element type.
                let iden_name = iden.name;
                let iden_id = iden.name_id;
                self.with_scope(|this| {
                    this.environment
                        .declare(iden_id, Type::Any)
                        .map_err(|_| Error::DuplicateDeclaration(iden_name, iden_id))?;
                    let transformer = this.convert_clause(transformer)?;
                    let filter = filter.map(|f| this.convert_clause(f)).transpose()?;
                    if let Some(f) = &filter {
                        require_type(&Type::Bool, &f.type_)?;
                    }
                    let elem_ty = transformer.type_.clone();
                    Ok((
                        elem_ty,
                        ArrayLiteralNode::ForComprehension(Box::new(ArrayForComprehentionNode {
                            iden,
                            collection,
                            transformer,
                            filter,
                        })),
                    ))
                })
            }
        }
    }
}

/// Join multiple types using gradual-typing-friendly unification:
/// - Skip `Any`s; take the first concrete type seen.
/// - If another concrete type disagrees, widen to `Any`.
/// - Empty iterator or all-`Any` → `Any`.
fn unify_types<'a>(types: impl IntoIterator<Item = &'a Type>) -> Type {
    let mut result: Option<&Type> = None;
    for t in types {
        if matches!(t, Type::Any) {
            continue;
        }
        match result {
            None => result = Some(t),
            Some(existing) if existing == t => {}
            Some(_) => return Type::Any,
        }
    }
    result.cloned().unwrap_or(Type::Any)
}

/// Assert `actual` is compatible with `expected`. `Any` on the actual side is
/// permissive (gradual typing).
fn require_type(expected: &Type, actual: &Type) -> Result<(), Error> {
    if matches!(actual, Type::Any) || actual == expected {
        Ok(())
    } else {
        Err(Error::ExpectedType(expected.clone(), actual.clone()))
    }
}

fn scalar_is_string(s: &ScalarNode) -> bool {
    matches!(s, ScalarNode::LazyStr { .. } | ScalarNode::StrLiteral(_))
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Type with name id {0:?} and position {1:?} have an explicit type mismatch")]
    // (Type span, Expected type, Actualt Type for now)
    // TODO: Update to be actual type and error message later if we we have a /resolved type id/things
    ExplitcitTypeDeclartionMismatchActualType(Span, Id),

    #[error("Binary operator {0:?} cannot apply to types {1:?} and {2:?}")]
    BinaryOpTypeMismatch(Token, Type, Type),

    #[error("Unary operator {0:?} cannot apply to type {1:?}")]
    UnaryOpTypeMismatch(Token, Type),

    #[error("Expected type {0:?}, got {1:?}")]
    ExpectedType(Type, Type),

    #[error("Map keys must be strings; got {0:?}")]
    MapKeyMustBeString(ScalarNode),

    #[error("Identifier {1:?} at {0:?} is not defined in scope")]
    UndefinedIdentifier(Span, Id),

    #[error("Identifier {1:?} at {0:?} is already declared in this scope")]
    DuplicateDeclaration(Span, Id),
}
