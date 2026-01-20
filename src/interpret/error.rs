use super::value::Value;
use thiserror::Error;

use crate::{parse::ParseError, token::Token};

#[derive(Debug, Error)]
pub enum Error {
    #[error("Variable of name `{0}` has been declared before")]
    ReDeclareVariable(String),

    #[error("Variable of name `{0} has not been declared but get re-assigned")]
    NotFoundVariable(String),

    #[error("Variable of name `{0}` is readonly in this scope")]
    VariableReadOnly(String),

    #[error("Unknown operator `{0}`")]
    UnknownOperation(Token),

    #[error("Value `{1}` does not accept operator `{0}`")]
    InvalidOperationOnType(Token, Value),

    #[error("Type `{1}` and type `{2} canot perform operation `{0}` ")]
    MismatchType(Token, Value, Value),

    #[error("Condition evaluated to `{0}` which is not a boolean value")]
    ConditionNotBool(Value),

    #[error("Divide by 0")]
    DivideByZero,

    #[error("Value `{0}` is not a callable")]
    ValueNotCallable(Value),

    #[error("Callable `{0}` accepts {1} number of arguments but received {2}")]
    WrongNumberOfArgument(String, usize, usize),

    #[error("Value `{0}` is not of the type array or map, hance not indexable")]
    ValueUnIndexable(Value),

    #[error("Value `{0}` is can not be used as key for array or map")]
    #[allow(dead_code)]
    ValueCannotBeUsedAsKey(Value),

    #[error("Value `{0}` is not of the type non-negative integer")]
    ValueMustBeUsize(Value),

    #[error("Arrayhas length `{0}` but received index `{1}`")]
    ArrayOutOfBound(usize, usize),

    #[error("Module `{0}` cannot be found in path `{1}`")]
    ModuleNotFoundInPath(String, String),

    #[error("Reading module `{0}` in path `{1}` failed with error: {2}")]
    ReadModuleFailed(String, String, std::io::Error),

    #[error("Parsing module `{0}` in path `{1}` failed with error: {2}")]
    ParseModuleFailed(String, String, ParseError),

    #[error("Interpreting module `{0}` in path `{1}` failed with error: {2}")]
    InterpretModuleFailed(String, String, Box<Error>),

    #[error("Serialize value `{0}` failed with error: {1}")]
    SerializeFailed(Value, String),

    #[error("Deserialize value `{0}` failed with error: {1}")]
    DeserializeFailed(Value, String),
}
