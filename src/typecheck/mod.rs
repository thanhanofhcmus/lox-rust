use thiserror::Error;

use crate::{
    ast::*,
    id::Id,
    span::Span,
    types::{ComputeType, Type},
};

pub struct Environment {}

impl Environment {
    pub fn new() -> Self {
        Self {}
    }
}

pub struct TypeChecker<'cl, 'sl> {
    pub(super) environment: &'cl mut Environment,
    pub(super) input: &'sl str,
}

impl<'cl, 'sl> TypeChecker<'cl, 'sl> {
    pub fn new(environment: &'cl mut Environment, input: &'sl str) -> Self {
        Self { environment, input }
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
                if type_ != Type::Infered && type_ != Type::Any && type_ != expr.type_ {
                    return Err(Error::ExplitcitTypeDeclartionMismatchActualType(
                        iden.name,
                        iden.name_id,
                    ));
                }
                let new_type = if type_ == Type::Any { Type::Any } else { expr.type_.clone() };
                Ok(Statement::Declare(DeclareStatementNode {
                    expr,
                    iden,
                    type_: new_type,
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

    fn convert_expr(&mut self, ut_expr: Expression<()>) -> Result<Expression<Type>, Error> {
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
        let stmts = block
            .stmts
            .into_iter()
            .map(|s| self.convert_stmt(s))
            .collect::<Result<Vec<_>, _>>()?;
        let last_expr = block
            .last_expr
            .map(|e| self.convert_expr(*e).map(Box::new))
            .transpose()?;
        Ok(BlockNode { stmts, last_expr })
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
        Ok(ForNode {
            iden: node.iden,
            collection: self.convert_clause(node.collection)?,
            body: self.convert_block(node.body)?,
        })
    }

    fn convert_clause(&mut self, ut_clause: ClauseNode<()>) -> Result<ClauseNode<Type>, Error> {
        let (type_, case) = match ut_clause.case {
            ClauseCase::RawValue(raw) => {
                let type_ = raw.compute_type();
                (type_, ClauseCase::RawValue(self.convert_raw_value(raw)?))
            }
            ClauseCase::Identifier(iden) => (Type::Any, ClauseCase::Identifier(iden)),
            ClauseCase::Group(inner) => {
                let inner = self.convert_clause(*inner)?;
                let type_ = inner.type_.clone();
                (type_, ClauseCase::Group(Box::new(inner)))
            }
            ClauseCase::Unary(operand, op) => {
                let operand = self.convert_clause(*operand)?;
                (Type::Any, ClauseCase::Unary(Box::new(operand), op))
            }
            ClauseCase::Binary(bin) => {
                let bin = BinaryOpNode {
                    lhs: Box::new(self.convert_clause(*bin.lhs)?),
                    op: bin.op,
                    rhs: Box::new(self.convert_clause(*bin.rhs)?),
                };
                (Type::Any, ClauseCase::Binary(bin))
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

    fn convert_raw_value(&mut self, raw: RawValueNode<()>) -> Result<RawValueNode<Type>, Error> {
        match raw {
            RawValueNode::Scalar(s) => Ok(RawValueNode::Scalar(s)),
            RawValueNode::ArrayLiteral(arr) => {
                Ok(RawValueNode::ArrayLiteral(self.convert_array_literal(arr)?))
            }
            RawValueNode::MapLiteral(map) => {
                let nodes = map
                    .nodes
                    .into_iter()
                    .map(|elem| {
                        Ok(MapLiteralElementNode {
                            key: elem.key,
                            value: self.convert_clause(elem.value)?,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                Ok(RawValueNode::MapLiteral(MapLiteralNode { nodes }))
            }
            RawValueNode::FnDecl(fn_decl) => Ok(RawValueNode::FnDecl(FnDeclNode {
                params: fn_decl.params,
                body: self.convert_block(fn_decl.body)?,
                return_type: fn_decl.return_type,
            })),
        }
    }

    fn convert_array_literal(
        &mut self,
        arr: ArrayLiteralNode<()>,
    ) -> Result<ArrayLiteralNode<Type>, Error> {
        match arr {
            ArrayLiteralNode::List(items) => {
                let items = items
                    .into_iter()
                    .map(|c| self.convert_clause(c))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ArrayLiteralNode::List(items))
            }
            ArrayLiteralNode::Repeat(rep) => {
                Ok(ArrayLiteralNode::Repeat(Box::new(ArrayRepeatNode {
                    value: self.convert_clause(rep.value)?,
                    repeat: self.convert_clause(rep.repeat)?,
                })))
            }
            ArrayLiteralNode::ForComprehension(comp) => Ok(ArrayLiteralNode::ForComprehension(
                Box::new(ArrayForComprehentionNode {
                    iden: comp.iden,
                    collection: self.convert_clause(comp.collection)?,
                    transformer: self.convert_clause(comp.transformer)?,
                    filter: comp.filter.map(|f| self.convert_clause(f)).transpose()?,
                }),
            )),
        }
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Type with name id {0:?} and position {1:?} have an explicit type mismatch")]
    // (Type span, Expected type, Actualt Type for now)
    // TOBO: Update to be actual type and error message later if we we have a /resolved type id/things
    ExplitcitTypeDeclartionMismatchActualType(Span, Id),
}
