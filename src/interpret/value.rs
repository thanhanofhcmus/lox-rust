use derive_more::Display;

use super::{error::Error, interpreter::Interpreter};

use crate::{ast::StatementList, id::Id};

pub type BuiltinFn = fn(&mut Interpreter, Vec<Value>) -> Result<Value, Error>;

#[derive(Debug, Clone)]
pub struct Function {
    pub arg_ids: Vec<Id>,
    pub body: StatementList,
}

#[derive(Display, Debug, Clone)]
pub enum Value {
    #[display(fmt = "nil")]
    Nil,

    // use ":?" to print the string in quotes
    #[display(fmt = "{:?}", _0)]
    Str(String),

    Number(f64),

    Bool(bool),

    #[display(fmt = "{:?}", _0)]
    Array(Vec<Value>),

    #[display(fmt = "function")]
    Function(Function),

    #[display(fmt = "builtin_function")]
    BuiltinFunction(BuiltinFn),
}
