use super::{Environment, TypecheckError};

use crate::ast::*;
use crate::types::{StructField, StructType, SymbolId, Type};
use crate::{token::Token, types::TypeId};

pub struct TypeChecker<'cl, 'sl> {
    pub(super) environment: &'cl mut Environment,
    pub(super) input: &'sl str,

    // This is a horable horable cheat to get what we are trying to assing to
    // to avoid recursiving function does not resole id
    current_declaration_id: Option<IdentifierNode>,
}

impl<'cl, 'sl> TypeChecker<'cl, 'sl> {
    pub fn new(environment: &'cl mut Environment, input: &'sl str) -> Self {
        Self {
            environment,
            input,
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
                self.current_declaration_id.replace(node.iden);
                let result = self.convert_declare_stmt(node);
                self.current_declaration_id.take();
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

    fn convert_declare_stmt(
        &mut self,
        DeclareStatementNode {
            iden,
            explicit_type: type_node,
            expr,
        }: DeclareStatementNode<()>,
    ) -> Result<DeclareStatementNode<TypeId>, TypecheckError> {
        let expr = self.convert_expr(expr)?;
        match &type_node {
            None | Some(TypeNode::BuiltIn(TypeId::ANY)) => {}
            Some(TypeNode::BuiltIn(type_annot)) if *type_annot == expr.extra => {}
            Some(TypeNode::BuiltIn(type_annot)) => {
                return Err(TypecheckError::ExplicitTypeMismatch(
                    iden,
                    *type_annot,
                    expr.extra,
                ));
            }
        }
        // Annotation wins when written (including `Any`); otherwise infer from expr.
        let var_type = type_node.map(extract_type_node_id).unwrap_or(expr.extra);
        self.environment
            .declare_id(iden.id, var_type)
            .map_err(|_| TypecheckError::DuplicateDeclaration(iden))?;
        Ok(DeclareStatementNode {
            expr,
            iden,
            explicit_type: type_node,
        })
    }

    fn convert_struct_decl(
        &mut self,
        node: StructDeclNode,
    ) -> Result<StructDeclNode, TypecheckError> {
        let fields = node
            .fields
            .iter()
            .map(|field| {
                let type_id = field
                    .explicit_type
                    .map(extract_type_node_id)
                    .unwrap_or(TypeId::ANY);
                let symbol_id = self.environment.declare_type_symbol(field.iden, self.input);
                StructField {
                    symbol_id,
                    type_: type_id,
                }
            })
            .collect::<Vec<_>>();

        let symbol_id = self.environment.declare_type_symbol(node.iden, self.input);
        let type_id = self
            .environment
            .declare_type(&Type::Struct(StructType { symbol_id, fields }));
        self.environment
            .associate_id_with_type(node.iden.get_id(), type_id);
        self.environment
            .declare_id(node.iden.id, type_id)
            .map_err(|_| TypecheckError::DuplicateDeclaration(node.iden))?;
        Ok(node)
    }

    fn convert_reassign_target(
        &mut self,
        target: ChainingReassignTargetNode<()>,
    ) -> Result<ChainingReassignTargetNode<TypeId>, TypecheckError> {
        if self.environment.lookup_id(target.base.id).is_none() {
            return Err(TypecheckError::UndefinedIdentifier(target.base));
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

    fn convert_for(&mut self, node: ForNode<()>) -> Result<ForNode<TypeId>, TypecheckError> {
        let collection = self.convert_clause(node.collection)?;
        // TODO: derive iden type from the collection's element type once array/map
        // element types are used during lookup. Permissive `Any` for now.
        self.with_scope(|this| {
            this.environment
                .declare_id(node.iden.id, TypeId::ANY)
                .map_err(|_| TypecheckError::DuplicateDeclaration(node.iden))?;
            let body = this.convert_block(node.body)?;
            Ok(ForNode {
                iden: node.iden,
                collection,
                body,
            })
        })
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
                    .ok_or(TypecheckError::UndefinedIdentifier(iden))?;
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

    /// Typecheck an array literal and return the array's *element type* alongside
    /// the converted node. The caller wraps it in `Type::Array(...)`.
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
                let elem_type_id = unify_types(items.iter().map(|c| &c.extra));
                let type_id = self
                    .environment
                    .declare_type(&Type::Array { elem: elem_type_id });
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
                let ArrayForComprehentionNode {
                    iden,
                    collection,
                    transformer,
                    filter,
                } = *comp;
                let collection = self.convert_clause(collection)?;
                self.with_scope(|this| {
                    this.environment
                        .declare_id(iden.id, collection.extra)
                        .map_err(|_| TypecheckError::DuplicateDeclaration(iden))?;
                    let transformer = this.convert_clause(transformer)?;
                    let filter = filter.map(|f| this.convert_clause(f)).transpose()?;
                    if let Some(f) = &filter {
                        require_type(TypeId::BOOL, f.extra)?;
                    }
                    let type_id = this.environment.declare_type(&Type::Array {
                        elem: transformer.extra,
                    });
                    Ok((
                        type_id,
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

    fn convert_map_literal(
        &mut self,
        map: MapLiteralNode<()>,
    ) -> Result<(TypeId, MapLiteralNode<TypeId>), TypecheckError> {
        let mut nodes = Vec::with_capacity(map.nodes.len());
        for elem in map.nodes {
            let value = self.convert_clause(elem.value)?;
            nodes.push(MapLiteralElementNode {
                key: elem.key,
                value,
            });
        }

        let key_types: Vec<_> = nodes
            .iter()
            .map(|n| get_scalar_node_type_id(&n.key))
            .collect();

        let type_id = self.environment.declare_type(&Type::Map {
            key: unify_types(&key_types),
            value: unify_types(nodes.iter().map(|n| &n.value.extra)),
        });

        Ok((type_id, MapLiteralNode { nodes }))
    }

    fn convert_struct_literal(
        &mut self,
        node: StructLiteralNode<()>,
    ) -> Result<(TypeId, StructLiteralNode<TypeId>), TypecheckError> {
        let StructLiteralNode {
            iden,
            fields: ut_fields,
        } = node;

        let mut fields = vec![];
        for field in ut_fields {
            let value = self.convert_clause(field.value)?;
            fields.push(StructLiteralFieldNode {
                iden: field.iden,
                value,
            })
        }

        let struct_type_id = self
            .environment
            .lookup_type_id_from_id(iden.get_id())
            .ok_or(TypecheckError::UndefinedIdentifier(iden))?;
        let type_ = self
            .environment
            .lookup_type(struct_type_id)
            .ok_or(TypecheckError::TypeIsNotDeclared(struct_type_id))?;

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
                    let provided_symbol_id = SymbolId::from(provided.iden.get_id());
                    let declared = struct_type
                        .fields
                        .iter()
                        .find(|f| f.symbol_id == provided_symbol_id)
                        .ok_or(TypecheckError::StructFieldNameMismatch(
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
            _ => return Err(TypecheckError::TypeIdNotStructInLiteral(struct_type_id)),
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
                    p.explicit_type
                        .map(extract_type_node_id)
                        .unwrap_or(TypeId::ANY)
                })
                .collect::<Vec<_>>();

            if let Some(iden) = &this.current_declaration_id {
                // TODO: this unwrap panics on duplicate function declaration;
                // should return TypecheckError::DuplicateDeclaration like line below does
                this.environment
                    .declare_id(iden.id, TypeId::ANY_FUNCTION)
                    .unwrap();
            }

            for (param, type_id) in node.params.iter().zip(&params_type_ids) {
                this.environment
                    .declare_id(param.id.id, *type_id)
                    .map_err(|_| TypecheckError::DuplicateDeclaration(param.id))?;
            }
            let body = this.convert_block(node.body)?;

            let return_type = match &node.return_type {
                // inferred type got from the computed body
                None => body.extra,
                // explicit any type "swallow" the body type
                Some(TypeNode::BuiltIn(TypeId::ANY)) => TypeId::ANY,
                // same type
                Some(TypeNode::BuiltIn(type_annot)) if *type_annot == body.extra => *type_annot,
                // error case
                Some(node) => {
                    return Err(TypecheckError::ExplicitFnReturnTypeMismatch(
                        extract_type_node_id(*node), // expected
                        body.extra,                  // actual
                    ));
                }
            };

            let type_id = this.environment.declare_type(&Type::Function {
                params: params_type_ids,
                // Normal function does not have a way to declare variadict function yet
                variadict: None,
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

    fn convert_subscription(
        &mut self,
        node: SubscriptionNode<()>,
    ) -> Result<(TypeId, SubscriptionNode<TypeId>), TypecheckError> {
        let indexer = self.convert_clause(*node.indexer)?;
        let indexee = self.convert_clause(*node.indexee)?;
        let indexer_type_id = indexer.extra;
        let indexee_type_id = indexee.extra;

        let indexer_type = self
            .environment
            .lookup_type(indexer_type_id)
            .ok_or(TypecheckError::TypeIsNotDeclared(indexer_type_id))?;

        // Any is a special case, will need to resovle the actual type at runtime
        let result_type_id = match indexer_type {
            Type::Array { elem } if matches!(indexee_type_id, TypeId::ANY | TypeId::NUMBER) => {
                *elem
            }
            Type::Map { key, value }
                if (*key == indexee_type_id)
                    || (*key == TypeId::ANY
                        && matches!(
                            indexee_type_id,
                            TypeId::ANY | TypeId::BOOL | TypeId::NUMBER | TypeId::STR | TypeId::NIL
                        )) =>
            {
                *value
            }
            // defered to runtime
            Type::Any => TypeId::ANY,
            _ => {
                return Err(TypecheckError::IndexOpTypeMismatch(
                    indexer_type_id,
                    indexee_type_id,
                ));
            }
        };

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

        // Runtime defered
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
            .ok_or(TypecheckError::TypeIsNotDeclared(caller_type_id))?;
        let (param_type_ids, variadict_type_id, return_type_id) = match caller_type {
            Type::Function {
                params,
                return_,
                variadict: varidict,
            } => (params, *varidict, *return_),
            _ => return Err(TypecheckError::TypeIsNoCallable(caller_type_id)),
        };

        // TODO: variadic arg checking is broken — outer and inner both check
        // variadict_type_id.is_none(), making the inner check always true within this branch.
        // Needs separate non-variadic and variadic validation paths.

        if variadict_type_id.is_none() {
            if variadict_type_id.is_none() && param_type_ids.len() != args.len() {
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

    /// Compute the result type of a unary operator applied to an operand type.
    ///
    /// Opinionated choices:
    /// - `-` (Minus) negates numbers.
    /// - `not` (Not) inverts Bools.
    /// - `Any` is permissive: accept and return the op's natural result type.
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

/// Assert `actual` is compatible with `expected`. `Any` on the actual side is
/// permissive (gradual typing).
fn require_type(expected: TypeId, actual: TypeId) -> Result<(), TypecheckError> {
    if actual == TypeId::ANY || actual == expected {
        Ok(())
    } else {
        Err(TypecheckError::ExpectedType(expected, actual))
    }
}

fn extract_type_node_id(node: TypeNode) -> TypeId {
    match node {
        TypeNode::BuiltIn(type_id) => type_id,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{id::Id, parse::lex};
    use pretty_assertions::assert_eq;

    fn typecheck_str(input: &str) -> Result<AST<TypeId>, TypecheckError> {
        let items = lex(input).expect("lex failed");
        let ast = crate::parse::parse(input, &items, false).expect("parse failed");
        let mut env = Environment::new();
        TypeChecker::new(&mut env, input).convert(ast)
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
        assert!(matches!(err, TypecheckError::ExpectedType(_, _)));
    }

    // ---------- Environment ----------

    #[test]
    fn env_declare_and_lookup() {
        let mut env = Environment::new();
        let id = Id::new("x");
        env.declare_id(id, TypeId::NUMBER).unwrap();
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
        env.declare_id(id, TypeId::NUMBER).unwrap();
        assert!(env.declare_id(id, TypeId::STR).is_err());
    }

    #[test]
    fn env_shadow_across_scopes_ok() {
        let mut env = Environment::new();
        let id = Id::new("x");
        env.declare_id(id, TypeId::NUMBER).unwrap();
        env.push_scope();
        assert!(env.declare_id(id, TypeId::STR).is_ok());
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
        assert!(env.declare_id(Id::new("print"), TypeId::NUMBER).is_ok());
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
        assert!(matches!(err, TypecheckError::UndefinedIdentifier(_)));
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

    #[test]
    fn declare_without_annotation_infers() {
        let ast = typecheck_str("var x = 5; x;").unwrap();
        match &ast.global_stmts[1] {
            Statement::Expr(Expression {
                case: ExprCase::Clause(c),
                ..
            }) => assert_eq!(c.extra, TypeId::NUMBER),
            other => panic!("expected Expr clause, got {other:?}"),
        }
    }

    // ---------- reassign ----------

    #[test]
    fn reassign_undefined_errors() {
        let err = typecheck_str("x = 5;").unwrap_err();
        assert!(matches!(err, TypecheckError::UndefinedIdentifier(_)));
    }

    #[test]
    fn reassign_defined_ok() {
        typecheck_str("var x: number = 5; x = 10;").unwrap();
    }

    // ---------- duplicate declaration ----------

    #[test]
    fn duplicate_var_in_same_scope_errors() {
        let err = typecheck_str("var x = 1; var x = 2;").unwrap_err();
        assert!(matches!(err, TypecheckError::DuplicateDeclaration(_)));
    }

    #[test]
    fn fn_duplicate_param_errors() {
        let err = typecheck_str("var f = fn(x, x) x;").unwrap_err();
        assert!(matches!(err, TypecheckError::DuplicateDeclaration(_)));
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
            matches!(err, TypecheckError::UndefinedIdentifier(_)),
            "expected UndefinedIdentifier, got {err:?}"
        );
    }

    #[test]
    fn struct_field_count_mismatch_errors() {
        let err =
            typecheck_str("struct Point { x: number, y: number } var p = Point { x = 1 };")
                .unwrap_err();
        assert!(
            matches!(err, TypecheckError::StructFieldCountMismatch(_, 2, 1)),
            "expected StructFieldCountMismatch(_, 2, 1), got {err:?}"
        );
    }

    #[test]
    fn struct_field_name_mismatch_errors() {
        let err =
            typecheck_str("struct Point { x: number } var p = Point { z = 1 };").unwrap_err();
        assert!(
            matches!(err, TypecheckError::StructFieldNameMismatch(_, _)),
            "expected StructFieldNameMismatch, got {err:?}"
        );
    }

    #[test]
    fn struct_field_type_mismatch_errors() {
        let err =
            typecheck_str("struct Point { x: number } var p = Point { x = \"hello\" };")
                .unwrap_err();
        assert!(
            matches!(err, TypecheckError::StructFieldTypeMismatch(_, _, _)),
            "expected StructFieldTypeMismatch, got {err:?}"
        );
    }

    #[test]
    fn struct_field_any_type_is_permissive() {
        // A field with `any` annotation accepts any value type.
        typecheck_str("struct Bag { item: any } var b = Bag { item = 42 };").unwrap();
        typecheck_str("struct Bag { item: any } var b = Bag { item = \"hello\" };").unwrap();
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
