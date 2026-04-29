use std::collections::HashSet;

use super::{Environment, TypecheckError};

use crate::ast::*;
use crate::id::Id;
use crate::identifier_registry::Identifier;
use crate::types::{StructField, StructType, Type};
use crate::{token::Token, types::TypeId};

pub struct TypeChecker<'e> {
    environment: &'e mut Environment,

    /// Set by `convert_declare_stmt` to the lhs name before descending into
    /// the rhs, and `take`n by `convert_fn_decl` if the rhs is directly an
    /// fn literal — giving that body (and that body only) visibility of its
    /// own name for self-recursion. `take` keeps nested fn decls from
    /// inheriting it.
    current_declaration_id: Option<Identifier>,
}

impl<'cl> TypeChecker<'cl> {
    pub fn new(environment: &'cl mut Environment) -> Self {
        Self {
            environment,
            current_declaration_id: None,
        }
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
    pub fn convert(&mut self, ast: AST<()>) -> Result<AST<TypeId>, TypecheckError> {
        let AST {
            imports,
            global_stmts: untyped_global_stmts,
        } = ast;

        // TODO: work on imports

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

    fn convert_stmt(
        &mut self,
        ut_stmt: Statement<()>,
    ) -> Result<Statement<TypeId>, TypecheckError> {
        match ut_stmt {
            Statement::Declare(node) => {
                let result = self.convert_declare_stmt(node);
                Ok(Statement::Declare(result?))
            }
            Statement::StructDecl(node) => {
                let node = self.convert_struct_decl(node)?;
                Ok(Statement::StructDecl(node))
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

    fn convert_binding(
        &mut self,
        binding: &DeclareBindingNode,
        explicit_type_id: Option<TypeId>,
        actual_type_id: TypeId,
    ) -> Result<(), TypecheckError> {
        // 3 sources of truth
        // var %(a, b, c) : %(any, number, bool) = %("a string", 12, false);

        match binding {
            DeclareBindingNode::Identifier(iden) => {
                let type_id = match explicit_type_id {
                    None => promote_untyped_to_any(actual_type_id),
                    Some(explicit) => reconcile_single_type(*iden, explicit, actual_type_id)?,
                };

                self.environment
                    // Annotation wins when written (including `Any`); otherwise infer from expr.
                    .declare_variable_id(iden.id, type_id)
                    .map_err(|_| TypecheckError::DuplicateVariableDeclaration(*iden))?;
            }
            DeclareBindingNode::Tuple { members } => {
                let arity = members.len();
                let actual_members =
                    self.resolve_tuple_members_for_destructure(actual_type_id, arity)?;
                let explicit_members: Option<Vec<TypeId>> = explicit_type_id
                    .map(|e| self.resolve_tuple_members_for_destructure(e, arity))
                    .transpose()?;

                for (i, iden) in members.iter().enumerate() {
                    let actual = actual_members[i];
                    let type_id = match &explicit_members {
                        None => promote_untyped_to_any(actual),
                        Some(exp) => reconcile_single_type(*iden, exp[i], actual)?,
                    };
                    self.environment
                        .declare_variable_id(iden.id, type_id)
                        .map_err(|_| TypecheckError::DuplicateVariableDeclaration(*iden))?;
                }
            }
        }
        Ok(())
    }

    /// Resolve `type_id` into a per-member list suitable for tuple
    /// destructuring with the given `arity`. `Any` is permissive (returns a
    /// vec of `Any`s of the right length, letting the runtime catch the real
    /// shape). Any other non-tuple type is rejected.
    fn resolve_tuple_members_for_destructure(
        &self,
        type_id: TypeId,
        arity: usize,
    ) -> Result<Vec<TypeId>, TypecheckError> {
        if type_id == TypeId::ANY {
            return Ok(vec![TypeId::ANY; arity]);
        }
        match self.environment.lookup_type(type_id) {
            Some(Type::Tuple { members }) => {
                if members.len() != arity {
                    Err(TypecheckError::TupleDestructureArityMismatch {
                        expected: arity,
                        actual: members.len(),
                    })
                } else {
                    Ok(members.clone())
                }
            }
            _ => Err(TypecheckError::CannotDestructureAsTuple(type_id)),
        }
    }

    fn convert_declare_stmt(
        &mut self,
        DeclareStatementNode {
            binding,
            explicit_type: type_node,
            expr,
        }: DeclareStatementNode<()>,
    ) -> Result<DeclareStatementNode<TypeId>, TypecheckError> {
        let explicit_type_id = type_node
            .as_ref()
            .map(|n| self.extract_type_node_id(n))
            .transpose()?;

        // Expose the declared name to a directly-nested fn literal for
        // self-recursion. Tuple destructuring can't name a single callable,
        // so only the Identifier case is meaningful.
        if let DeclareBindingNode::Identifier(iden) = &binding {
            self.current_declaration_id = Some(*iden);
        }
        let expr = self.convert_expr(expr);
        self.current_declaration_id = None;
        let expr = expr?;

        self.convert_binding(&binding, explicit_type_id, expr.extra)?;
        Ok(DeclareStatementNode {
            binding,
            explicit_type: type_node,
            expr,
        })
    }

    fn extract_type_node_id(&mut self, node: &TypeNode) -> Result<TypeId, TypecheckError> {
        match node {
            TypeNode::BuiltIn(type_id) => Ok(*type_id),
            TypeNode::Named(type_iden) => self
                .environment
                .lookup_type_id_from_id(type_iden.id)
                .ok_or(TypecheckError::UndefinedTypeIdentifier(*type_iden)),
            TypeNode::Array(inner_type_node) => {
                let elem = self.extract_type_node_id(inner_type_node)?;
                Ok(self.environment.declare_type(&Type::Array { elem }))
            }
            TypeNode::Map { key, value } => {
                let key_type_id = self.extract_type_node_id(key)?;
                require_map_key_type(key_type_id)?;
                let value_type_id = self.extract_type_node_id(value)?;
                Ok(self.environment.declare_type(&Type::Map {
                    key: key_type_id,
                    value: value_type_id,
                }))
            }
            TypeNode::Tuple { members } => {
                let members = members
                    .iter()
                    .map(|n| self.extract_type_node_id(n))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(self.environment.declare_type(&Type::Tuple { members }))
            }
        }
    }

    fn convert_struct_decl(
        &mut self,
        node: StructDeclNode,
    ) -> Result<StructDeclNode, TypecheckError> {
        if let Some(prior) = self.environment.lookup_type_id_from_id(node.iden.id) {
            return Err(TypecheckError::DuplicateTypeDeclaration(node.iden, prior));
        }

        let fields = node
            .fields
            .iter()
            .map(|field| {
                let type_id = field
                    .explicit_type
                    .as_ref()
                    .map(|n| self.extract_type_node_id(n))
                    .transpose()?
                    .unwrap_or(TypeId::ANY);
                Ok(StructField {
                    id: field.iden.id,
                    type_: type_id,
                })
            })
            .collect::<Result<Vec<_>, TypecheckError>>()?;

        let type_id = self.environment.declare_type(&Type::Struct(StructType {
            id: node.iden.id,
            fields,
        }));
        self.environment
            .associate_id_with_type(node.iden.id, type_id);
        Ok(node)
    }

    fn convert_reassign_target(
        &mut self,
        target: ChainingReassignTargetNode<()>,
    ) -> Result<ChainingReassignTargetNode<TypeId>, TypecheckError> {
        // `_` is a discard target — accept `_ = rhs;` (no chain) at typecheck;
        // chained writes through `_` (`_.x = …`, `_[0] = …`) still fall
        // through to the normal undefined-variable path below.
        if target.base.id == Id::UNDERSCORE && target.follows.is_empty() {
            return Ok(ChainingReassignTargetNode {
                base: target.base,
                follows: Vec::new(),
            });
        }

        let base_type_id = self
            .environment
            .lookup_id(target.base.id)
            .ok_or(TypecheckError::UndefinedVariableIdentifier(target.base))?;

        // Walk the chain with an explicit running type, validating each step
        // against the current type and computing the next running type.
        let mut running_type_id = base_type_id;
        let mut follows: Vec<ChainStep<ClauseNode<TypeId>>> =
            Vec::with_capacity(target.follows.len());

        for step in target.follows {
            let next_type_id = match &step {
                ChainStep::Subscription(clause) => {
                    let typed_clause = self.convert_clause(clause.clone())?;
                    let indexee_type_id = typed_clause.extra;
                    let next =
                        self.resolve_subscription_result_type(running_type_id, indexee_type_id)?;
                    follows.push(ChainStep::Subscription(typed_clause));
                    next
                }
                ChainStep::Member(member) => {
                    let next = self.resolve_member_result_type(running_type_id, *member)?;
                    follows.push(ChainStep::Member(*member));
                    next
                }
            };
            running_type_id = next_type_id;
        }

        Ok(ChainingReassignTargetNode {
            base: target.base,
            follows,
        })
    }

    /// Resolve the element/value type produced by a subscription against an indexer type.
    fn resolve_subscription_result_type(
        &self,
        indexer_type_id: TypeId,
        indexee_type_id: TypeId,
    ) -> Result<TypeId, TypecheckError> {
        let indexer_type = self
            .environment
            .lookup_type(indexer_type_id)
            .ok_or(TypecheckError::TypeIsNotDeclaredInternal(indexer_type_id))?;
        match indexer_type {
            Type::Array { elem } if matches!(indexee_type_id, TypeId::ANY | TypeId::NUMBER) => {
                Ok(*elem)
            }
            Type::Map { key, value } if (*key == indexee_type_id) || (*key == TypeId::ANY) => {
                require_map_key_type(indexee_type_id)?;
                Ok(*value)
            }
            Type::Any => Ok(TypeId::ANY),
            _ => Err(TypecheckError::IndexOpTypeMismatch(
                indexer_type_id,
                indexee_type_id,
            )),
        }
    }

    /// Resolve the declared type of a struct field and tuple index
    fn resolve_member_result_type(
        &self,
        object_type_id: TypeId,
        member: MemberNode,
    ) -> Result<TypeId, TypecheckError> {
        if object_type_id == TypeId::ANY {
            return Ok(TypeId::ANY);
        }
        let object_type = self
            .environment
            .lookup_type(object_type_id)
            .ok_or(TypecheckError::TypeIsNotDeclaredInternal(object_type_id))?;
        match member {
            MemberNode::StructField(field) => match object_type {
                Type::Struct(struct_type) => struct_type
                    .fields
                    .iter()
                    .find(|f| f.id == field.id)
                    .map(|f| f.type_)
                    .ok_or(TypecheckError::StructFieldNotExist(object_type_id, field)),
                _ => Err(TypecheckError::TypeIsNotStruct(object_type_id)),
            },
            MemberNode::TupleIndex(idx) => match object_type {
                Type::Tuple {
                    members: tuple_members,
                } => tuple_members
                    .get(idx)
                    .copied()
                    .ok_or(TypecheckError::TupleIndexOutOfBound(
                        object_type_id,
                        tuple_members.len(),
                        idx,
                    )),
                _ => Err(TypecheckError::TypeIsNotStruct(object_type_id)),
            },
        }
    }

    fn convert_expr(
        &mut self,
        ut_expr: Expression<()>,
    ) -> Result<Expression<TypeId>, TypecheckError> {
        let (type_id, case) = match ut_expr.case {
            ExprCase::Clause(ut_clause) => {
                let clause = self.convert_clause(ut_clause)?;
                (clause.extra, ExprCase::Clause(clause))
            }
            ExprCase::Block(ut_block) => {
                let block = self.convert_block(ut_block)?;
                (block.extra, ExprCase::Block(block))
            }
            ExprCase::IfChain(ut_if_chain) => {
                let if_chain = self.convert_if_chain(ut_if_chain)?;
                let mut type_ids = vec![if_chain.if_node.stmts.extra];
                type_ids.extend(if_chain.else_if_nodes.iter().map(|v| v.stmts.extra));
                if let Some(else_node) = &if_chain.else_stmts {
                    type_ids.push(else_node.extra);
                }
                let type_id = unify_types(&type_ids);
                (type_id, ExprCase::IfChain(if_chain))
            }
            ExprCase::When(ut_when_node) => {
                let when_node = self.convert_when(ut_when_node)?;
                let type_id = unify_types(when_node.arms.iter().map(|c| &c.expr.extra));
                (type_id, ExprCase::When(when_node))
            }
            ExprCase::Return(ut_expr) => {
                let expr = ut_expr
                    .map(|e| self.convert_expr(*e).map(Box::new))
                    .transpose()?;
                let type_id = expr.as_ref().map(|e| e.extra).unwrap_or(TypeId::UNIT);
                (type_id, ExprCase::Return(expr))
            }
            ExprCase::While(ut_while_node) => (
                TypeId::UNIT,
                ExprCase::While(self.convert_while(ut_while_node)?),
            ),
            ExprCase::For(ut_for_node) => {
                (TypeId::UNIT, ExprCase::For(self.convert_for(ut_for_node)?))
            }
        };

        Ok(Expression {
            extra: type_id,
            case,
        })
    }

    fn convert_block(&mut self, block: BlockNode<()>) -> Result<BlockNode<TypeId>, TypecheckError> {
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
            let type_id = last_expr.as_ref().map(|e| e.extra).unwrap_or(TypeId::UNIT);
            Ok(BlockNode {
                extra: type_id,
                stmts,
                last_expr,
            })
        })
    }

    fn convert_if_chain(
        &mut self,
        node: IfChainNode<()>,
    ) -> Result<IfChainNode<TypeId>, TypecheckError> {
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

    fn convert_else_if(
        &mut self,
        node: ElseIfNode<()>,
    ) -> Result<ElseIfNode<TypeId>, TypecheckError> {
        let cond = self.convert_clause(node.cond)?;
        require_type(TypeId::BOOL, cond.extra)?;
        let stmts = self.convert_block(node.stmts)?;
        Ok(ElseIfNode { cond, stmts })
    }

    fn convert_while(&mut self, node: WhileNode<()>) -> Result<WhileNode<TypeId>, TypecheckError> {
        let cond = self.convert_clause(node.cond)?;
        require_type(TypeId::BOOL, cond.extra)?;
        let body = self.convert_block(node.body)?;
        Ok(WhileNode { cond, body })
    }

    fn convert_for(
        &mut self,
        node: ForStatementNode<()>,
    ) -> Result<ForStatementNode<TypeId>, TypecheckError> {
        let collection = self.convert_clause(node.collection)?;
        let elem_type_id = self.iterable_element_type(collection.extra);
        self.with_scope(|this| {
            this.convert_binding(&node.binding, None, elem_type_id)?;
            let body = this.convert_block(node.body)?;
            Ok(ForStatementNode {
                binding: node.binding,
                filter: None,
                collection,
                body,
            })
        })
    }

    fn iterable_element_type(&mut self, collection_type_id: TypeId) -> TypeId {
        match self.environment.lookup_type(collection_type_id) {
            Some(Type::Array { elem }) => *elem,
            Some(Type::Map { key, value }) => self.environment.declare_type(&Type::Tuple {
                members: vec![*key, *value],
            }),
            _ => TypeId::ANY,
        }
    }

    /// Shared scaffolding for `for`-comprehension typechecks. Converts the
    /// collection, binds the iterator under a fresh scope, validates the
    /// optional filter as bool, then defers to `body_fn` for the per-shape
    /// body conversion (single clause for arrays, key+value for maps).
    fn convert_for_comprehension_pre<R>(
        &mut self,
        binding: &DeclareBindingNode,
        collection: ClauseNode<()>,
        filter: Option<ClauseNode<()>>,
        body_fn: impl FnOnce(&mut Self) -> Result<R, TypecheckError>,
    ) -> Result<(ClauseNode<TypeId>, Option<ClauseNode<TypeId>>, R), TypecheckError> {
        let collection = self.convert_clause(collection)?;
        let elem_type_id = self.iterable_element_type(collection.extra);
        let (filter, body) = self.with_scope(|this| {
            this.convert_binding(binding, None, elem_type_id)?;
            let filter = filter.map(|f| this.convert_clause(f)).transpose()?;
            if let Some(f) = &filter {
                require_type(TypeId::BOOL, f.extra)?;
            }
            let body = body_fn(this)?;
            Ok((filter, body))
        })?;
        Ok((collection, filter, body))
    }

    fn convert_when(&mut self, node: WhenNode<()>) -> Result<WhenNode<TypeId>, TypecheckError> {
        let arms = node
            .arms
            .into_iter()
            .map(|arm| self.convert_when_arm(arm))
            .collect::<Result<Vec<_>, TypecheckError>>()?;
        Ok(WhenNode { arms })
    }

    fn convert_when_arm(
        &mut self,
        node: WhenArmNode<()>,
    ) -> Result<WhenArmNode<TypeId>, TypecheckError> {
        let cond = self.convert_clause(node.cond)?;
        require_type(TypeId::BOOL, cond.extra)?;
        let expr = self.convert_clause(node.expr)?;
        Ok(WhenArmNode { cond, expr })
    }

    fn convert_clause(
        &mut self,
        ut_clause: ClauseNode<()>,
    ) -> Result<ClauseNode<TypeId>, TypecheckError> {
        let (type_id, case) = match ut_clause.case {
            ClauseCase::RawValue(raw) => {
                let (type_id, raw) = self.convert_raw_value(raw)?;
                (type_id, ClauseCase::RawValue(raw))
            }
            ClauseCase::Identifier(iden) => {
                let type_ = self
                    .environment
                    .lookup_id(iden.id)
                    .ok_or(TypecheckError::UndefinedVariableIdentifier(iden))?;
                (type_, ClauseCase::Identifier(iden))
            }
            ClauseCase::Group(inner) => {
                let inner = self.convert_clause(*inner)?;
                (inner.extra, ClauseCase::Group(Box::new(inner)))
            }
            ClauseCase::Unary(operand, op) => {
                let operand = self.convert_clause(*operand)?;
                let type_id = self.type_unary_op(op, operand.extra)?;
                (type_id, ClauseCase::Unary(Box::new(operand), op))
            }
            ClauseCase::Binary(bin) => {
                let lhs = Box::new(self.convert_clause(*bin.lhs)?);
                let rhs = Box::new(self.convert_clause(*bin.rhs)?);
                let type_id = self.type_binary_op(bin.op, lhs.extra, rhs.extra)?;
                (
                    type_id,
                    ClauseCase::Binary(BinaryOpNode {
                        lhs,
                        op: bin.op,
                        rhs,
                    }),
                )
            }
            ClauseCase::Subscription(sub) => {
                let (type_id, case) = self.convert_subscription(sub)?;
                (type_id, ClauseCase::Subscription(case))
            }
            ClauseCase::MemberAccess(node) => {
                let (type_id, case) = self.convert_member_access(node)?;
                (type_id, ClauseCase::MemberAccess(case))
            }
            ClauseCase::FnCall(call) => {
                let (type_id, case) = self.convert_fn_call(call)?;
                (type_id, ClauseCase::FnCall(case))
            }
        };
        Ok(ClauseNode {
            extra: type_id,
            case,
        })
    }

    fn convert_raw_value(
        &mut self,
        raw: RawValueNode<()>,
    ) -> Result<(TypeId, RawValueNode<TypeId>), TypecheckError> {
        match raw {
            RawValueNode::Scalar(s) => Ok((get_scalar_node_type_id(&s), RawValueNode::Scalar(s))),
            RawValueNode::ArrayLiteral(arr) => {
                let (type_id, arr) = self.convert_array_literal(arr)?;
                Ok((type_id, RawValueNode::ArrayLiteral(arr)))
            }
            RawValueNode::MapLiteral(map) => {
                let (type_id, map) = self.convert_map_literal(map)?;
                Ok((type_id, RawValueNode::MapLiteral(map)))
            }
            RawValueNode::TupleLiteral(node) => {
                let (type_id, node) = self.convert_tuple_literal(node)?;
                Ok((type_id, RawValueNode::TupleLiteral(node)))
            }
            RawValueNode::StructLiteral(node) => {
                let (type_id, node) = self.convert_struct_literal(node)?;
                Ok((type_id, RawValueNode::StructLiteral(node)))
            }
            RawValueNode::FnDecl(fn_decl) => {
                let (type_id, fn_decl) = self.convert_fn_decl(fn_decl)?;
                Ok((type_id, RawValueNode::FnDecl(fn_decl)))
            }
        }
    }

    /// Typecheck an array literal and return the array's *element type* alongside the converted node
    fn convert_array_literal(
        &mut self,
        arr: ArrayLiteralNode<()>,
    ) -> Result<(TypeId, ArrayLiteralNode<TypeId>), TypecheckError> {
        match arr {
            ArrayLiteralNode::List(items) => {
                let items = items
                    .into_iter()
                    .map(|c| self.convert_clause(c))
                    .collect::<Result<Vec<_>, _>>()?;
                let type_id = if items.is_empty() {
                    TypeId::ARRAY_UNTYPED
                } else {
                    let elem = unify_types(items.iter().map(|c| &c.extra));
                    self.environment.declare_type(&Type::Array { elem })
                };
                Ok((type_id, ArrayLiteralNode::List(items)))
            }
            ArrayLiteralNode::Repeat(rep) => {
                let value = self.convert_clause(rep.value)?;
                let repeat = self.convert_clause(rep.repeat)?;
                require_type(TypeId::NUMBER, repeat.extra)?;
                let type_id = self
                    .environment
                    .declare_type(&Type::Array { elem: value.extra });
                Ok((
                    type_id,
                    ArrayLiteralNode::Repeat(Box::new(ArrayRepeatNode { value, repeat })),
                ))
            }
            ArrayLiteralNode::ForComprehension(comp) => {
                let ArrayForComprehensionNode {
                    binding,
                    collection,
                    body,
                    filter,
                } = *comp;
                let (collection, filter, body) =
                    self.convert_for_comprehension_pre(&binding, collection, filter, |this| {
                        this.convert_clause(body)
                    })?;
                let type_id = self
                    .environment
                    .declare_type(&Type::Array { elem: body.extra });
                Ok((
                    type_id,
                    ArrayLiteralNode::ForComprehension(Box::new(ArrayForComprehensionNode {
                        binding,
                        collection,
                        body,
                        filter,
                    })),
                ))
            }
        }
    }

    fn convert_map_literal(
        &mut self,
        map: MapLiteralNode<()>,
    ) -> Result<(TypeId, MapLiteralNode<TypeId>), TypecheckError> {
        match map {
            MapLiteralNode::List(elems) => {
                let mut nodes = Vec::with_capacity(elems.len());
                for elem in elems {
                    let value = self.convert_clause(elem.value)?;
                    nodes.push(MapLiteralKVElemNode {
                        key: elem.key,
                        value,
                    });
                }

                let type_id = if nodes.is_empty() {
                    TypeId::MAP_UNTYPED
                } else {
                    let key_types: Vec<_> = nodes
                        .iter()
                        .map(|n| get_scalar_node_type_id(&n.key))
                        .collect();
                    self.environment.declare_type(&Type::Map {
                        key: unify_types(&key_types),
                        value: unify_types(nodes.iter().map(|n| &n.value.extra)),
                    })
                };

                Ok((type_id, MapLiteralNode::List(nodes)))
            }
            MapLiteralNode::ForComprehension(comp) => {
                let MapForComprehensionNode {
                    binding,
                    collection,
                    body,
                    filter,
                } = *comp;
                let (collection, filter, body) =
                    self.convert_for_comprehension_pre(&binding, collection, filter, |this| {
                        let key = this.convert_clause(body.key)?;
                        require_map_key_type(key.extra)?;
                        let value = this.convert_clause(body.value)?;
                        Ok(MapForComprehensionBodyNode { key, value })
                    })?;
                let type_id = self.environment.declare_type(&Type::Map {
                    key: body.key.extra,
                    value: body.value.extra,
                });
                Ok((
                    type_id,
                    MapLiteralNode::ForComprehension(Box::new(MapForComprehensionNode {
                        binding,
                        collection,
                        body,
                        filter,
                    })),
                ))
            }
        }
    }

    fn convert_tuple_literal(
        &mut self,
        node: TupleLiteralNode<()>,
    ) -> Result<(TypeId, TupleLiteralNode<TypeId>), TypecheckError> {
        let members = node
            .members
            .into_iter()
            .map(|m| self.convert_clause(m))
            .collect::<Result<Vec<_>, _>>()?;
        let type_ = Type::Tuple {
            members: members.iter().map(|v| v.extra).collect(),
        };
        let type_id = self.environment.declare_type(&type_);
        Ok((type_id, TupleLiteralNode { members }))
    }

    fn convert_struct_literal(
        &mut self,
        node: StructLiteralNode<()>,
    ) -> Result<(TypeId, StructLiteralNode<TypeId>), TypecheckError> {
        let StructLiteralNode {
            iden,
            fields: ut_fields,
        } = node;

        let mut fields = Vec::with_capacity(ut_fields.len());
        let mut seen_field_ids = HashSet::with_capacity(ut_fields.len());
        for field in ut_fields {
            // Reject `Point { x = 1, x = 2 }` — each field name must appear at most once per literal
            if !seen_field_ids.insert(field.iden.id) {
                return Err(TypecheckError::DuplicateStructLiteralField(field.iden));
            }
            let value = self.convert_clause(field.value)?;
            fields.push(StructLiteralFieldNode {
                iden: field.iden,
                value,
            })
        }

        let struct_type_id = self
            .environment
            .lookup_type_id_from_id(iden.id)
            .ok_or(TypecheckError::UndefinedTypeIdentifier(iden))?;
        let type_ = self
            .environment
            .lookup_type(struct_type_id)
            .ok_or(TypecheckError::TypeIsNotDeclaredInternal(struct_type_id))?;

        match type_ {
            Type::Struct(struct_type) => {
                if fields.len() != struct_type.fields.len() {
                    return Err(TypecheckError::StructFieldCountMismatch(
                        struct_type_id,
                        struct_type.fields.len(),
                        fields.len(),
                    ));
                }
                for provided in &fields {
                    let declared = struct_type
                        .fields
                        .iter()
                        .find(|f| f.id == provided.iden.id)
                        .ok_or(TypecheckError::StructFieldNotExist(
                            struct_type_id,
                            provided.iden,
                        ))?;
                    let provided_type = provided.value.extra;
                    if provided_type != TypeId::ANY
                        && declared.type_ != TypeId::ANY
                        && provided_type != declared.type_
                    {
                        return Err(TypecheckError::StructFieldTypeMismatch(
                            struct_type_id,
                            declared.type_,
                            provided_type,
                        ));
                    }
                }
            }
            Type::Any => {} // ignore, runtime check
            _ => return Err(TypecheckError::TypeNotStructInLiteral(struct_type_id)),
        }

        Ok((struct_type_id, StructLiteralNode { iden, fields }))
    }

    fn convert_fn_decl(
        &mut self,
        node: FnDeclNode<()>,
    ) -> Result<(TypeId, FnDeclNode<TypeId>), TypecheckError> {
        // Params are declared in a fresh scope surrounding the body. The
        // body itself opens its own scope via `convert_block`, so param
        // shadowing by locals is handled naturally.
        self.with_scope(|this| {
            let params_type_ids = node
                .params
                .iter()
                .map(|p| {
                    Ok(p.explicit_type
                        .as_ref()
                        .map(|n| this.extract_type_node_id(n))
                        .transpose()?
                        .unwrap_or(TypeId::ANY))
                })
                .collect::<Result<Vec<_>, TypecheckError>>()?;

            let explicit_return_type = node
                .return_type
                .as_ref()
                .map(|n| this.extract_type_node_id(n))
                .transpose()?;

            for (param, type_id) in node.params.iter().zip(&params_type_ids) {
                this.environment
                    .declare_variable_id(param.iden.id, *type_id)
                    .map_err(|_| TypecheckError::DuplicateVariableDeclaration(param.iden))?;
            }

            // Self-binding for `var f = fn(...) { ... f(...) ... };`. The name
            // is pre-declared only inside this fn body's scope, so it cannot
            // be seen by sibling statements in the enclosing scope. `take`
            // prevents nested fn literals from inheriting this.
            if let Some(self_iden) = this.current_declaration_id.take() {
                let sig = this.environment.declare_type(&Type::Function {
                    params: params_type_ids.clone(),
                    variadic: None,
                    return_: explicit_return_type.unwrap_or(TypeId::ANY),
                });
                this.environment
                    .declare_variable_id(self_iden.id, sig)
                    .map_err(|_| TypecheckError::DuplicateVariableDeclaration(self_iden))?;
            }

            let body = this.convert_block(node.body)?;

            let return_type = match explicit_return_type {
                // inferred type got from the computed body
                None => body.extra,
                // explicit any type "swallow" the body type
                Some(TypeId::ANY) => TypeId::ANY,
                // same type
                Some(type_annot) if type_annot == body.extra => type_annot,
                // error case
                Some(type_annot) => {
                    return Err(TypecheckError::ExplicitFnReturnTypeMismatch(
                        type_annot, body.extra,
                    ));
                }
            };

            let type_id = this.environment.declare_type(&Type::Function {
                params: params_type_ids,
                // Normal function does not have a way to declare variadic function yet
                variadic: None,
                return_: return_type,
            });

            Ok((
                type_id,
                FnDeclNode {
                    params: node.params,
                    body,
                    return_type: node.return_type,
                },
            ))
        })
    }

    fn convert_member_access(
        &mut self,
        node: MemberAccessNode<()>,
    ) -> Result<(TypeId, MemberAccessNode<TypeId>), TypecheckError> {
        let object = self.convert_clause(*node.object)?;
        let member_type_id = self.resolve_member_result_type(object.extra, node.member)?;
        Ok((
            member_type_id,
            MemberAccessNode {
                object: Box::new(object),
                member: node.member,
            },
        ))
    }

    fn convert_subscription(
        &mut self,
        node: SubscriptionNode<()>,
    ) -> Result<(TypeId, SubscriptionNode<TypeId>), TypecheckError> {
        let indexer = self.convert_clause(*node.indexer)?;
        let indexee = self.convert_clause(*node.indexee)?;
        let result_type_id = self.resolve_subscription_result_type(indexer.extra, indexee.extra)?;
        Ok((
            result_type_id,
            SubscriptionNode {
                indexer: Box::new(indexer),
                indexee: Box::new(indexee),
            },
        ))
    }

    fn convert_fn_call(
        &mut self,
        node: FnCallNode<()>,
    ) -> Result<(TypeId, FnCallNode<TypeId>), TypecheckError> {
        let caller = self.convert_clause(*node.caller)?;

        let args = node
            .args
            .into_iter()
            .map(|e| self.convert_expr(e))
            .collect::<Result<Vec<_>, _>>()?;

        let caller_type_id = caller.extra;

        // Runtime deferred
        if caller_type_id == TypeId::ANY {
            return Ok((
                TypeId::ANY,
                FnCallNode {
                    caller: Box::new(caller),
                    args,
                },
            ));
        }

        let caller_type = self
            .environment
            .lookup_type(caller_type_id)
            .ok_or(TypecheckError::TypeIsNotDeclaredInternal(caller_type_id))?;
        let (param_type_ids, variadic_type_id, return_type_id) = match caller_type {
            Type::Function {
                params,
                return_,
                variadic,
            } => (params, *variadic, *return_),
            _ => return Err(TypecheckError::TypeIsNotCallable(caller_type_id)),
        };

        // TODO: variadic arg checking is broken — outer and inner both check
        // variadic_type_id.is_none(), making the inner check always true within this branch.
        // Needs separate non-variadic and variadic validation paths.

        if variadic_type_id.is_none() {
            if param_type_ids.len() != args.len() {
                return Err(TypecheckError::WrongNumberOfArgument(
                    caller_type_id,
                    param_type_ids.len(),
                    args.len(),
                ));
            }

            fn is_convertable(lhs: TypeId, rhs: TypeId) -> bool {
                match (lhs, rhs) {
                    (TypeId::ANY, _) | (_, TypeId::ANY) => true,
                    _ if lhs == rhs => true,
                    _ => false,
                }
            }

            let arg_type_id = args.iter().map(|e| e.extra);

            let wrong_type_args = param_type_ids
                .iter()
                .zip(arg_type_id)
                .enumerate()
                .filter(|(_, (l, r))| !is_convertable(**l, *r))
                .map(|(i, (l, r))| (i, *l, r))
                .collect::<Vec<(usize, TypeId, TypeId)>>();
            if !wrong_type_args.is_empty() {
                return Err(TypecheckError::WrongArgumentTypes(
                    caller_type_id,
                    wrong_type_args,
                ));
            }
        }

        Ok((
            // The type of this whole fn call is the return type
            return_type_id,
            FnCallNode {
                caller: Box::new(caller),
                args,
            },
        ))
    }

    fn type_binary_op(
        &self,
        op: Token,
        lhs: TypeId,
        rhs: TypeId,
    ) -> Result<TypeId, TypecheckError> {
        use Token::*;
        let any = lhs == TypeId::ANY || rhs == TypeId::ANY;
        let mismatch = || TypecheckError::BinaryOpTypeMismatch(op, lhs, rhs);

        match op {
            Plus => match (lhs, rhs) {
                (TypeId::NUMBER, TypeId::NUMBER) => Ok(TypeId::NUMBER),
                (TypeId::STR, TypeId::STR) => Ok(TypeId::STR),
                _ if any => {
                    if lhs == TypeId::STR || rhs == TypeId::STR {
                        Ok(TypeId::STR)
                    } else if lhs == TypeId::NUMBER || rhs == TypeId::NUMBER {
                        Ok(TypeId::NUMBER)
                    } else {
                        Ok(TypeId::ANY)
                    }
                }
                _ => Err(mismatch()),
            },
            Minus | Star | Slash | Percentage => match (lhs, rhs) {
                (TypeId::NUMBER, TypeId::NUMBER) => Ok(TypeId::NUMBER),
                _ if any => Ok(TypeId::NUMBER),
                _ => Err(mismatch()),
            },
            EqualEqual | BangEqual => {
                // TODO: enable for for strict type comparation
                // if lhs == rhs || any { Ok(TypeId::BOOL) } else { Err(mismatch()) }
                Ok(TypeId::BOOL)
            }
            Less | LessEqual | Greater | GreaterEqual => match (lhs, rhs) {
                (TypeId::NUMBER, TypeId::NUMBER) | (TypeId::STR, TypeId::STR) => Ok(TypeId::BOOL),
                _ if any => Ok(TypeId::BOOL),
                _ => Err(mismatch()),
            },
            And | Or => match (lhs, rhs) {
                (TypeId::BOOL, TypeId::BOOL) => Ok(TypeId::BOOL),
                _ if any => Ok(TypeId::BOOL),
                _ => Err(mismatch()),
            },
            _ => unreachable!("non-binary-op token in BinaryOpNode: {:?}", op),
        }
    }

    fn type_unary_op(&self, op: Token, operand: TypeId) -> Result<TypeId, TypecheckError> {
        use Token::*;
        let any = operand == TypeId::ANY;
        let mismatch = || TypecheckError::UnaryOpTypeMismatch(op, operand);

        match op {
            Minus => match operand {
                TypeId::NUMBER => Ok(TypeId::NUMBER),
                _ if any => Ok(TypeId::NUMBER),
                _ => Err(mismatch()),
            },
            Not => match operand {
                TypeId::BOOL => Ok(TypeId::BOOL),
                _ if any => Ok(TypeId::BOOL),
                _ => Err(mismatch()),
            },
            _ => unreachable!("non-unary-op token in Unary: {:?}", op),
        }
    }
}

fn get_scalar_node_type_id(node: &ScalarNode) -> TypeId {
    match &node {
        ScalarNode::Nil => TypeId::NIL,
        ScalarNode::Bool(_) => TypeId::BOOL,
        ScalarNode::Integer(_) | ScalarNode::Floating(_) => TypeId::NUMBER,
        ScalarNode::LazyStr { .. } | ScalarNode::LiteralStr(_) => TypeId::STR,
    }
}

fn require_map_key_type(type_id: TypeId) -> Result<(), TypecheckError> {
    match type_id {
        TypeId::ANY | TypeId::BOOL | TypeId::NUMBER | TypeId::STR | TypeId::NIL => Ok(()),
        _ => Err(TypecheckError::TypeCannotBeUsedAsMapKey(type_id)),
    }
}

/// Join multiple types using gradual-typing-friendly unification:
/// - Skip `Any`s; take the first concrete type seen.
/// - If another concrete type disagrees, widen to `Any`.
/// - Empty iterator or all-`Any` → `Any`
fn unify_types<'a>(type_ids: impl IntoIterator<Item = &'a TypeId>) -> TypeId {
    let mut result: Option<TypeId> = None;
    for &t in type_ids {
        if t == TypeId::ANY {
            continue;
        }
        match result {
            None => result = Some(t),
            Some(existing) if existing == t => {}
            Some(_) => return TypeId::ANY,
        }
    }
    result.unwrap_or(TypeId::ANY)
}

fn require_type(expected: TypeId, actual: TypeId) -> Result<(), TypecheckError> {
    if actual == TypeId::ANY || actual == expected {
        Ok(())
    } else {
        Err(TypecheckError::UnexpectedType(expected, actual))
    }
}

/// Turn an "untyped" array/map marker (produced by empty literals like `[]`
/// and `%{}`) into its inhabited `Any`-parameterised form. Concrete types
/// pass through unchanged.
fn promote_untyped_to_any(type_id: TypeId) -> TypeId {
    match type_id {
        TypeId::ARRAY_UNTYPED => TypeId::ARRAY_ANY,
        TypeId::MAP_UNTYPED => TypeId::MAP_ANY_ANY,
        v => v,
    }
}

/// Reconcile a single binding's `explicit` annotation with the `actual` rhs
/// type. `Any` on the annotation side is permissive; concrete annotations
/// must match (with the `ARRAY_UNTYPED` / `MAP_UNTYPED` relaxation for empty literals).
fn reconcile_single_type(
    iden: Identifier,
    explicit: TypeId,
    actual: TypeId,
) -> Result<TypeId, TypecheckError> {
    match (explicit, actual) {
        (TypeId::ANY, _) => Ok(TypeId::ANY),
        (e, a) if e == a => Ok(e),
        (e, TypeId::ARRAY_UNTYPED) if e.is_array() => Ok(e),
        (e, TypeId::MAP_UNTYPED) if e.is_map() => Ok(e),
        (e, a) => Err(TypecheckError::ExplicitTypeMismatch(iden, e, a)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{id::Id, identifier_registry::IdentifierRegistry, parse::lex};
    use pretty_assertions::assert_eq;

    fn typecheck_str(input: &str) -> Result<AST<TypeId>, TypecheckError> {
        let items = lex(input).expect("lex failed");
        let mut identifier_registry = IdentifierRegistry::new();
        let ast = crate::parse::parse(input, &items, &mut identifier_registry, false)
            .expect("parse failed");
        let mut env = Environment::new();
        TypeChecker::new(&mut env).convert(ast)
    }

    /// Type of the expression inside the first top-level Expr statement's Clause.
    fn first_clause_type(ast: &AST<TypeId>) -> TypeId {
        match &ast.global_stmts[0] {
            Statement::Expr(Expression {
                case: ExprCase::Clause(c),
                ..
            }) => c.extra,
            other => panic!("expected first stmt to be an Expr clause, got {other:?}"),
        }
    }

    // ---------- unify_types (pure helper) ----------

    #[test]
    fn unify_empty_is_any() {
        assert_eq!(unify_types(std::iter::empty::<&TypeId>()), TypeId::ANY);
    }

    #[test]
    fn unify_single_concrete() {
        assert_eq!(unify_types([&TypeId::NUMBER]), TypeId::NUMBER);
    }

    #[test]
    fn unify_all_same_concrete() {
        assert_eq!(
            unify_types([&TypeId::NUMBER, &TypeId::NUMBER, &TypeId::NUMBER]),
            TypeId::NUMBER
        );
    }

    #[test]
    fn unify_any_mixed_with_concrete_keeps_concrete() {
        assert_eq!(unify_types([&TypeId::ANY, &TypeId::NUMBER]), TypeId::NUMBER);
        assert_eq!(unify_types([&TypeId::NUMBER, &TypeId::ANY]), TypeId::NUMBER);
    }

    #[test]
    fn unify_disagreeing_concretes_widens_to_any() {
        assert_eq!(unify_types([&TypeId::NUMBER, &TypeId::STR]), TypeId::ANY);
    }

    #[test]
    fn unify_all_any_is_any() {
        assert_eq!(unify_types([&TypeId::ANY, &TypeId::ANY]), TypeId::ANY);
    }

    // ---------- require_type ----------

    #[test]
    fn require_type_exact_match_is_ok() {
        assert!(require_type(TypeId::NUMBER, TypeId::NUMBER).is_ok());
    }

    #[test]
    fn require_type_any_on_actual_is_permissive() {
        assert!(require_type(TypeId::NUMBER, TypeId::ANY).is_ok());
    }

    #[test]
    fn require_type_mismatch_errors() {
        let err = require_type(TypeId::NUMBER, TypeId::BOOL).unwrap_err();
        assert!(matches!(err, TypecheckError::UnexpectedType(_, _)));
    }

    // ---------- Environment ----------

    #[test]
    fn env_declare_and_lookup() {
        let mut env = Environment::new();
        let id = Id::new("x");
        env.declare_variable_id(id, TypeId::NUMBER).unwrap();
        assert_eq!(env.lookup_id(id), Some(TypeId::NUMBER));
    }

    #[test]
    fn env_lookup_missing_is_none() {
        let env = Environment::new();
        assert_eq!(env.lookup_id(Id::new("nope")), None);
    }

    #[test]
    fn env_redeclare_in_same_scope_errors() {
        let mut env = Environment::new();
        let id = Id::new("x");
        env.declare_variable_id(id, TypeId::NUMBER).unwrap();
        assert!(env.declare_variable_id(id, TypeId::STR).is_err());
    }

    #[test]
    fn env_shadow_across_scopes_ok() {
        let mut env = Environment::new();
        let id = Id::new("x");
        env.declare_variable_id(id, TypeId::NUMBER).unwrap();
        env.push_scope();
        assert!(env.declare_variable_id(id, TypeId::STR).is_ok());
        assert_eq!(env.lookup_id(id), Some(TypeId::STR));
        env.pop_scope();
        assert_eq!(env.lookup_id(id), Some(TypeId::NUMBER));
    }

    #[test]
    fn env_builtins_are_in_scope() {
        let env = Environment::new();
        assert!(env.lookup_id(Id::new("print")).is_some());
        assert!(env.lookup_id(Id::new("assert")).is_some());
    }

    #[test]
    fn env_user_global_can_shadow_builtin() {
        // User-global scope is scope 1, above builtins at scope 0.
        let mut env = Environment::new();
        assert!(
            env.declare_variable_id(Id::new("print"), TypeId::NUMBER)
                .is_ok()
        );
    }

    #[test]
    #[should_panic(expected = "scope underflow")]
    fn env_pop_underflow_panics() {
        let mut env = Environment::new();
        env.pop_scope();
        env.pop_scope();
        env.pop_scope(); // eventually underflows
    }

    // ---------- binary op typing (via type_binary_op directly) ----------
    // type_binary_op is a method on TypeChecker. Use end-to-end via typecheck_str.

    #[test]
    fn binary_number_plus_number_is_number() {
        let ast = typecheck_str("1 + 2;").unwrap();
        assert_eq!(first_clause_type(&ast), TypeId::NUMBER);
    }

    #[test]
    fn binary_str_plus_str_is_str() {
        let ast = typecheck_str("\"a\" + \"b\";").unwrap();
        assert_eq!(first_clause_type(&ast), TypeId::STR);
    }

    #[test]
    fn binary_number_plus_bool_errors() {
        let err = typecheck_str("1 + true;").unwrap_err();
        assert!(
            matches!(err, TypecheckError::BinaryOpTypeMismatch(Token::Plus, _, _)),
            "expected BinaryOpTypeMismatch(Plus, ...), got {err:?}"
        );
    }

    #[test]
    fn binary_comparison_returns_bool() {
        let ast = typecheck_str("1 < 2;").unwrap();
        assert_eq!(first_clause_type(&ast), TypeId::BOOL);
    }

    #[test]
    fn binary_equality_same_type_is_bool() {
        let ast = typecheck_str("1 == 2;").unwrap();
        assert_eq!(first_clause_type(&ast), TypeId::BOOL);
    }

    #[test]
    fn binary_and_on_bools_is_bool() {
        let ast = typecheck_str("true and false;").unwrap();
        assert_eq!(first_clause_type(&ast), TypeId::BOOL);
    }

    #[test]
    fn binary_and_on_non_bool_errors() {
        let err = typecheck_str("1 and 2;").unwrap_err();
        assert!(matches!(
            err,
            TypecheckError::BinaryOpTypeMismatch(Token::And, _, _)
        ));
    }

    // ---------- unary op typing ----------

    #[test]
    fn unary_minus_on_number_is_number() {
        let ast = typecheck_str("-5;").unwrap();
        assert_eq!(first_clause_type(&ast), TypeId::NUMBER);
    }

    #[test]
    fn unary_not_on_bool_is_bool() {
        let ast = typecheck_str("not true;").unwrap();
        assert_eq!(first_clause_type(&ast), TypeId::BOOL);
    }

    #[test]
    fn unary_minus_on_bool_errors() {
        let err = typecheck_str("-true;").unwrap_err();
        assert!(matches!(
            err,
            TypecheckError::UnaryOpTypeMismatch(Token::Minus, _)
        ));
    }

    #[test]
    fn unary_not_on_number_errors() {
        let err = typecheck_str("not 1;").unwrap_err();
        assert!(matches!(
            err,
            TypecheckError::UnaryOpTypeMismatch(Token::Not, _)
        ));
    }

    // ---------- array literals ----------

    // ---------- map literals ----------

    // ---------- identifier lookup ----------

    #[test]
    fn identifier_undefined_errors() {
        let err = typecheck_str("x;").unwrap_err();
        assert!(matches!(
            err,
            TypecheckError::UndefinedVariableIdentifier(_)
        ));
    }

    #[test]
    fn identifier_after_declare_has_declared_type() {
        let ast = typecheck_str("var x: number = 5; x;").unwrap();
        // The second stmt is `x;` — its clause's type should be Number.
        match &ast.global_stmts[1] {
            Statement::Expr(Expression {
                case: ExprCase::Clause(c),
                ..
            }) => assert_eq!(c.extra, TypeId::NUMBER),
            other => panic!("expected Expr clause, got {other:?}"),
        }
    }

    // ---------- declaration annotations ----------

    #[test]
    fn declare_with_matching_annotation_ok() {
        typecheck_str("var x: number = 5;").unwrap();
    }

    #[test]
    fn declare_with_mismatched_annotation_errors() {
        let err = typecheck_str("var x: number = \"hello\";").unwrap_err();
        assert!(matches!(err, TypecheckError::ExplicitTypeMismatch(..)));
    }

    #[test]
    fn declare_with_any_annotation_accepts_anything() {
        typecheck_str("var x: any = \"hello\";").unwrap();
        typecheck_str("var x: any = 5;").unwrap();
    }

    // ---------- reassign ----------

    #[test]
    fn reassign_undefined_errors() {
        let err = typecheck_str("x = 5;").unwrap_err();
        assert!(matches!(
            err,
            TypecheckError::UndefinedVariableIdentifier(_)
        ));
    }

    #[test]
    fn reassign_defined_ok() {
        typecheck_str("var x: number = 5; x = 10;").unwrap();
    }

    // ---------- duplicate declaration ----------

    #[test]
    fn duplicate_var_in_same_scope_errors() {
        let err = typecheck_str("var x = 1; var x = 2;").unwrap_err();
        assert!(matches!(
            err,
            TypecheckError::DuplicateVariableDeclaration(_)
        ));
    }

    #[test]
    fn fn_duplicate_param_errors() {
        let err = typecheck_str("var f = fn(x, x) x;").unwrap_err();
        assert!(matches!(
            err,
            TypecheckError::DuplicateVariableDeclaration(_)
        ));
    }

    // ---------- underscore wildcard ----------

    #[test]
    fn underscore_can_be_redeclared() {
        typecheck_str("var _ = 1; var _ = 2;").unwrap();
    }

    #[test]
    fn underscore_destructure_binds_other_members() {
        typecheck_str("var %(_, b) = %(1, 2); var n: number = b;").unwrap();
    }

    #[test]
    fn underscore_for_loop_typechecks() {
        typecheck_str("for _ in [1, 2, 3] { }").unwrap();
    }

    #[test]
    fn underscore_reassign_typechecks() {
        typecheck_str("_ = 1;").unwrap();
    }

    // ---------- fn self-binding (recursion) ----------

    #[test]
    fn fn_self_binding_direct_recursion_typechecks() {
        typecheck_str("var f = fn(n) { if n <= 0 { return 0; } return 1 + f(n - 1); }; f(3);")
            .unwrap();
    }

    #[test]
    fn fn_self_binding_does_not_leak_to_siblings() {
        // `f` is pre-bound only inside its own body — on the previous line `f`
        // should still be undefined.
        let err = typecheck_str("var x = f; var f = fn() 1;").unwrap_err();
        assert!(
            matches!(err, TypecheckError::UndefinedVariableIdentifier(_)),
            "expected UndefinedVariableIdentifier, got {err:?}"
        );
    }

    #[test]
    fn fn_mutual_recursion_is_rejected() {
        // Self-binding only covers the current fn; `b` is not visible when
        // converting `a`'s body.
        let err = typecheck_str("var a = fn() { return b(); }; var b = fn() { return a(); };")
            .unwrap_err();
        assert!(
            matches!(err, TypecheckError::UndefinedVariableIdentifier(_)),
            "expected UndefinedVariableIdentifier, got {err:?}"
        );
    }

    #[test]
    fn fn_inner_sees_outer_self_binding_via_closure() {
        // Lexical scoping: inner fn's body walks up the scope stack, so the
        // outer fn's self-binding is still visible. (This is the normal
        // closure behavior — not a leak to siblings.)
        typecheck_str(
            "var outer = fn() { var inner = fn() { return outer(); }; return inner(); };",
        )
        .unwrap();
    }

    // ---------- tuple destructuring typecheck ----------

    #[test]
    fn tuple_destructure_non_tuple_errors() {
        let err = typecheck_str("var %(a, b) = 42;").unwrap_err();
        assert!(
            matches!(err, TypecheckError::CannotDestructureAsTuple(_)),
            "expected CannotDestructureAsTuple, got {err:?}"
        );
    }

    #[test]
    fn tuple_destructure_wrong_arity_errors() {
        let err = typecheck_str("var %(a, b, c) = %(1, 2);").unwrap_err();
        assert!(
            matches!(
                err,
                TypecheckError::TupleDestructureArityMismatch {
                    expected: 3,
                    actual: 2,
                }
            ),
            "expected TupleDestructureArityMismatch{{3,2}}, got {err:?}"
        );
    }

    #[test]
    fn tuple_destructure_any_is_permissive() {
        // `any` rhs hides the shape from typecheck; we bind each name as Any
        // and defer to the runtime.
        typecheck_str("var x: any = 42; var %(a, b) = x;").unwrap();
    }

    #[test]
    fn tuple_destructure_binds_per_member_type() {
        // `a` gets number, `b` gets str — subsequent annotated uses must
        // accept them.
        typecheck_str("var %(a, b) = %(1, \"hi\"); var n: number = a; var s: str = b;").unwrap();
    }

    #[test]
    fn tuple_destructure_member_type_mismatch_via_annotation() {
        // `b` is bound as str from the rhs tuple; a subsequent `number`
        // annotation on b must fail. (Exercises that the member type really
        // propagated, since reassignment doesn't check types today.)
        let err = typecheck_str("var %(a, b) = %(1, \"hi\"); var n: number = b;").unwrap_err();
        assert!(
            matches!(err, TypecheckError::ExplicitTypeMismatch(_, _, _)),
            "expected ExplicitTypeMismatch, got {err:?}"
        );
    }

    #[test]
    fn tuple_destructure_explicit_annotation_matches() {
        typecheck_str("var %(a, b): %(number, str) = %(1, \"hi\");").unwrap();
    }

    #[test]
    fn tuple_destructure_explicit_annotation_wrong_shape_errors() {
        let err = typecheck_str("var %(a, b): number = %(1, \"hi\");").unwrap_err();
        assert!(
            matches!(err, TypecheckError::CannotDestructureAsTuple(_)),
            "expected CannotDestructureAsTuple, got {err:?}"
        );
    }

    #[test]
    fn tuple_destructure_explicit_annotation_wrong_arity_errors() {
        let err = typecheck_str("var %(a, b): %(number, str, bool) = %(1, \"hi\");").unwrap_err();
        assert!(
            matches!(
                err,
                TypecheckError::TupleDestructureArityMismatch {
                    expected: 2,
                    actual: 3,
                }
            ),
            "expected TupleDestructureArityMismatch{{2,3}}, got {err:?}"
        );
    }

    #[test]
    fn for_loop_tuple_binding_over_array_of_scalars_errors() {
        let err = typecheck_str("for %(a, b) in [1, 2, 3] { }").unwrap_err();
        assert!(
            matches!(err, TypecheckError::CannotDestructureAsTuple(_)),
            "expected CannotDestructureAsTuple, got {err:?}"
        );
    }

    // ---------- struct declarations & literals ----------

    #[test]
    fn struct_decl_registers_type() {
        typecheck_str("struct Point { x: number, y: number }").unwrap();
    }

    #[test]
    fn struct_literal_correct_fields_ok() {
        typecheck_str("struct Point { x: number, y: number } var p = Point { x = 1, y = 2 };")
            .unwrap();
    }

    #[test]
    fn struct_literal_undeclared_name_errors() {
        let err = typecheck_str("var p = Foo { x = 1 };").unwrap_err();
        assert!(
            matches!(err, TypecheckError::UndefinedTypeIdentifier(_)),
            "expected UndefinedTypeIdentifier, got {err:?}"
        );
    }

    #[test]
    fn struct_field_count_mismatch_errors() {
        let err = typecheck_str("struct Point { x: number, y: number } var p = Point { x = 1 };")
            .unwrap_err();
        assert!(
            matches!(err, TypecheckError::StructFieldCountMismatch(_, 2, 1)),
            "expected StructFieldCountMismatch(_, 2, 1), got {err:?}"
        );
    }

    #[test]
    fn struct_field_name_mismatch_errors() {
        let err = typecheck_str("struct Point { x: number } var p = Point { z = 1 };").unwrap_err();
        assert!(
            matches!(err, TypecheckError::StructFieldNotExist(_, _)),
            "expected StructFieldNameMismatch, got {err:?}"
        );
    }

    #[test]
    fn struct_field_type_mismatch_errors() {
        let err = typecheck_str("struct Point { x: number } var p = Point { x = \"hello\" };")
            .unwrap_err();
        assert!(
            matches!(err, TypecheckError::StructFieldTypeMismatch(_, _, _)),
            "expected StructFieldTypeMismatch, got {err:?}"
        );
    }

    #[test]
    fn struct_literal_duplicate_field_errors() {
        let err =
            typecheck_str("struct Point { x: number, y: number } var p = Point { x = 1, x = 2 };")
                .unwrap_err();
        assert!(
            matches!(err, TypecheckError::DuplicateStructLiteralField(_)),
            "expected DuplicateStructLiteralField, got {err:?}"
        );
    }

    #[test]
    fn struct_field_any_type_is_permissive() {
        // A field with `any` annotation accepts any value type.
        typecheck_str("struct Bag { item: any } var b = Bag { item = 42 };").unwrap();
        typecheck_str("struct Bag { item: any } var b = Bag { item = \"hello\" };").unwrap();
    }

    // ---------- member access ----------

    #[test]
    fn member_access_on_known_struct_typechecks() {
        typecheck_str("struct Point { x: number } var p = Point { x = 1 }; p.x;").unwrap();
    }

    #[test]
    fn member_access_unknown_field_errors() {
        let err =
            typecheck_str("struct Point { x: number } var p = Point { x = 1 }; p.z;").unwrap_err();
        assert!(
            matches!(err, TypecheckError::StructFieldNotExist(_, _)),
            "expected StructFieldNotExist, got {err:?}"
        );
    }

    #[test]
    fn member_access_on_non_struct_errors() {
        let err = typecheck_str("var n: number = 1; n.x;").unwrap_err();
        assert!(
            matches!(err, TypecheckError::TypeIsNotStruct(_)),
            "expected TypeIsNotStruct, got {err:?}"
        );
    }

    #[test]
    fn member_access_on_any_is_permissive() {
        // gradual-typing escape hatch: Any-typed object defers field check to runtime
        typecheck_str("var p: any = 1; p.x;").unwrap();
    }

    #[test]
    fn member_access_chained_typechecks() {
        // Note: user-defined struct names aren't valid in type annotations yet
        // (see parse_type_node), so we use `any` for the nested-struct field.
        // Dot access on Any is a typecheck-level passthrough.
        typecheck_str(
            "struct Inner { v: number } \
             struct Outer { inner: any } \
             var o = Outer { inner = Inner { v = 42 } }; \
             o.inner.v;",
        )
        .unwrap();
    }

    // ---------- array / map annotations ----------

    #[test]
    fn array_annotation_accepts_matching_literal() {
        typecheck_str("var a: [number] = [1, 2, 3];").unwrap();
    }

    #[test]
    fn array_annotation_accepts_empty_literal() {
        // empty literal is ARRAY_UNTYPED; annotation pins the element type
        typecheck_str("var a: [number] = [];").unwrap();
    }

    #[test]
    fn array_annotation_rejects_wrong_element_type() {
        let err = typecheck_str("var a: [number] = [\"hello\"];").unwrap_err();
        assert!(
            matches!(err, TypecheckError::ExplicitTypeMismatch(_, _, _)),
            "expected ExplicitTypeMismatch, got {err:?}"
        );
    }

    #[test]
    fn array_annotation_any_accepts_mixed() {
        typecheck_str("var a: [any] = [1, \"mixed\", true];").unwrap();
    }

    #[test]
    fn nested_array_annotation_accepts_matching_literal() {
        typecheck_str("var a: [[number]] = [[1, 2], [3, 4]];").unwrap();
    }

    #[test]
    fn nested_array_annotation_rejects_wrong_inner_type() {
        let err = typecheck_str("var a: [[number]] = [[\"s\"]];").unwrap_err();
        assert!(
            matches!(err, TypecheckError::ExplicitTypeMismatch(_, _, _)),
            "expected ExplicitTypeMismatch, got {err:?}"
        );
    }

    #[test]
    fn map_annotation_accepts_matching_literal() {
        typecheck_str("var m: %{str => number} = %{\"a\" => 1, \"b\" => 2};").unwrap();
    }

    #[test]
    fn map_annotation_accepts_empty_literal() {
        // empty literal is MAP_UNTYPED; annotation pins key/value types
        typecheck_str("var m: %{str => number} = %{};").unwrap();
    }

    #[test]
    fn map_annotation_rejects_wrong_value_type() {
        let err = typecheck_str("var m: %{str => number} = %{\"a\" => \"x\"};").unwrap_err();
        assert!(
            matches!(err, TypecheckError::ExplicitTypeMismatch(_, _, _)),
            "expected ExplicitTypeMismatch, got {err:?}"
        );
    }

    #[test]
    fn map_annotation_any_accepts_mixed_keys_and_values() {
        typecheck_str("var m: %{any => any} = %{\"a\" => 1, 42 => \"b\", true => nil};").unwrap();
    }

    // ---------- block types ----------

    #[test]
    fn block_with_trailing_expr_has_that_type() {
        let ast = typecheck_str("{ 5 }").unwrap();
        match &ast.global_stmts[0] {
            Statement::Expr(Expression {
                case: ExprCase::Block(block),
                ..
            }) => assert_eq!(block.extra, TypeId::NUMBER),
            other => panic!("expected Expr(Block), got {other:?}"),
        }
    }

    #[test]
    fn block_without_trailing_expr_is_unit() {
        let ast = typecheck_str("{ var x = 1; }").unwrap();
        match &ast.global_stmts[0] {
            Statement::Expr(Expression {
                case: ExprCase::Block(block),
                ..
            }) => assert_eq!(block.extra, TypeId::UNIT),
            other => panic!("expected Expr(Block), got {other:?}"),
        }
    }
}
