use super::{Error, Instruction, VM};
use crate::ast::{Expression, Statement};

pub fn compile_stmt(vm: &mut VM, stmt: &Statement) -> Result<Vec<Instruction>, Error> {
    match stmt {
        Statement::Expr(expr) => compile_expr(vm, expr),
        _ => Err(Error::Compile),
    }
}

fn compile_expr(_vm: &mut VM, _expr: &Expression) -> Result<Vec<Instruction>, Error> {
    unimplemented!()
    // match expr {
    //     Expression::Number(_a) => Ok(vec![]),
    //     _ => Err(Error::Compile),
    // }
}
