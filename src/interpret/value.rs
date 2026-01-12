use derive_more::Display;

use crate::{ast::StatementList, id::Id};

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
    Function(Vec<Id>, StatementList),
}
