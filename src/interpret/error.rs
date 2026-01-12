use super::value::Value;
use thiserror::Error;

use crate::{ast::Expression, parse::ParseError, token::Token};

#[derive(Debug, Error)]
pub enum Error {
    #[error("Variable or function of name `{0}` has been declared before")]
    ReDeclareVariable(String),

    #[error("Variable or function of name `{0} has not been declared but get re-assigned")]
    NotFoundVariable(String),

    #[error("Variable of name `{0}` is readonly in this scope")]
    VariableReadOnly(String),

    #[error("Unknown operator `{0}`")]
    UnknownOperation(Token),

    #[error("Value `{1}` does not accept operator `{0}`")]
    InvalidOperationOnType(Token, Value),

    #[error("`{0}` and `{1} do not share the same type")]
    MismatchType(Value, Value),

    #[error("Condition evaluated to `{0}` which is not a boolean value")]
    ConditionNotBool(Value),

    #[error("Divide by 0")]
    DivideByZero,

    #[error("Value `{0}` is not a callable")]
    ValueNotCallable(Value),

    #[error("Callable `{0}` accept {1} number of arguments but receive {2}")]
    WrongNumberOfArgument(String, usize, usize),

    #[error("Value `{0}` is not of the type array of map, hence indexable")]
    ValueUnIndexable(Value),

    #[error("Value `{0}` is can not be used as key for array or map")]
    #[allow(dead_code)]
    ValueCannotBeUsedAsKey(Value),

    #[error("Value `{0}` is not of the type non-negative integer")]
    ValueMustBeUsize(Value),

    #[error("Expect expression of type `{0}`, have type `{1:?}`")]
    InvalidExpressionType(String, Expression),

    #[error("Array of name `{0}` has length `{1}` but receive index `{2}`")]
    ArrayOutOfBound(String, usize, usize),

    #[error("Module `{0}` cannot be found in path `{1}`")]
    ModuleNotFoundInPath(String, String),

    #[error("Reading module `{0}` in path `{1}` failed with error {2}")]
    ReadModuleFailed(String, String, std::io::Error),

    #[error("Parse module `{0}` in path `{1}` failed with error {2}")]
    ParseModuleFailed(String, String, ParseError),

    #[error("Interpret module `{0}` in pparse::ath `{1}` failed with error {2}")]
    InterpretModuleFailed(String, String, Box<Error>),
}
