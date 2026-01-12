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
    #[display(fmt = "{:?}", _0)] // Quotes for strings
    Str(String),

    #[display(fmt = "{}", _0)]
    Number(f64),

    #[display(fmt = "{}", _0)]
    Bool(bool),

    #[display(fmt = "{}", "format_array(_0)")] // Call helper
    Array(Vec<Value>),

    #[display(fmt = "function")]
    Function(Function),

    #[display(fmt = "builtin_function")]
    BuiltinFunction(BuiltinFn),
}

// Helper function to simulate Python's list printing
fn format_array(values: &[Value]) -> String {
    let internal = values
        .iter()
        .map(|v| v.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{}]", internal)
}
