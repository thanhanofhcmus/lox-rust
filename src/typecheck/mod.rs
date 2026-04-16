use thiserror::Error;

use crate::{ast::*, id::Id, span::Span, types::Type};

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

    /// consume the current ATS and return a new ATS will all declaration typed
    pub fn convert(&mut self, mut ast: AST) -> Result<AST, Error> {
        let AST {
            imports,
            global_stmts: untyped_global_stmts,
        } = ast;

        let mut global_stmts = Vec::with_capacity(untyped_global_stmts.len());

        for ut_stmt in untyped_global_stmts {
            global_stmts.push(ut_stmt);
        }

        Ok(AST {
            imports,
            global_stmts,
        })
    }

    fn convert_stmt(&mut self, ut_stmt: Statement) -> Result<Statement, Error> {
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
            Statement::ReassignIden(_, _) => Ok(ut_stmt),
            Statement::Expr(_) => Ok(ut_stmt),
        }
    }

    fn convert_expr(&mut self, mut ut_expr: Expression) -> Result<Expression, Error> {
        ut_expr.type_ = Type::Any;
        Ok(ut_expr)
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Type with name id {0:?} and position {1:?} have an explicit type mismatch")]
    // (Type span, Expected type, Actualt Type for now)
    // TOBO: Update to be actual type and error message later if we we have a /resolved type id/things
    ExplitcitTypeDeclartionMismatchActualType(Span, Id),
}
