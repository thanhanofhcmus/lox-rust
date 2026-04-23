use super::values::Value;
use thiserror::Error;

use crate::{
    id::Id,
    identifier_registry::{Identifier, IdentifierRegistry},
    interpret::heap::{GcHandle, GcKind, StrId},
    parse::ParseError,
    token::Token,
    typecheck,
};

// TODO: create a special error reporting function that print to the actual string/array/map value

#[derive(Debug, Error)]
pub enum InterpretError {
    #[error("Variable of name `{0:?}` has been declared before")]
    ReDeclareVariable(Identifier),

    #[error("Variable of name `{0:?}` has not been declared but get re-assigned")]
    NotFoundVariable(Identifier),

    #[error("Variable of name `{0:?}` is readonly in this scope")]
    VariableReadOnly(Identifier),

    #[error("Unknown operator `{0}`")]
    UnknownOperation(Token),

    #[error("Value `{1:?}` does not accept operator `{0}`")]
    InvalidOperationOnType(Token, Value),

    #[error("Type `{1:?}` and type `{2:?}` cannot perform operation `{0}`")]
    MismatchType(Token, Value, Value),

    #[error("Condition evaluated to `{0:?}` which is not a boolean value")]
    ConditionNotBool(Value),

    #[error("Divide by 0")]
    DivideByZero,

    #[error("Value `{0:?}` is not a callable")]
    ValueNotCallable(Value),

    #[error("Callable `{0:?}` accepts {1} number of arguments but received {2}")]
    WrongNumberOfArgument(Value, usize, usize),

    #[error("Callable `{0:?}` requires at least {1} argument(s) but received {2}")]
    WrongNumberOfArgumentAtLeast(Value, usize, usize),

    #[error("Callable `{0:?}` received argument `{1:?}` but expected type {2}")]
    WrongArgumentType(Value, Value, &'static str),

    // Fix this error to include structs and tuple
    #[error("Value `{0:?}` is not of the type array or map, hence not indexable")]
    ValueUnIndexable(Value),

    // Fix this error to include structs and tuple
    #[error("Value `{0:?}` is can not be used as key for array or map")]
    ValueCannotBeUsedAsKey(Value),

    #[error("Value `{0:?}` is not of the type non-negative integer")]
    ValueMustBeUsize(Value),

    // TODO: add GcHandle to error to know which array is out of bound
    #[error("Array has length `{0}` but received index `{1}`")]
    ArrayOutOfBound(usize, usize),

    #[error("Module `{0}` cannot be found in path `{1}`")]
    ModuleNotFoundInPath(String, String),

    #[error("Reading module `{0}` in path `{1}` failed with error: {2}")]
    ReadModuleFailed(String, String, std::io::Error),

    #[error("Parsing module `{0}` in path `{1}` failed with error: {2}")]
    ParseModuleFailed(String, String, ParseError),

    #[error("Type-checking module `{0}` in path `{1}` failed with error: {2}")]
    TypeCheckModuleFailed(String, String, typecheck::TypecheckError),

    #[error("Interpreting module `{0}` in path `{1}` failed with error: {2}")]
    InterpretModuleFailed(String, String, Box<InterpretError>),

    #[error("Value `{0:?}` cannot be use as for loop source")]
    ValueCannotBeUsedAsSourceInFor(Value),

    #[error("Value `{0:?}` is not serializable")]
    TypeIsNotSerializable(Value),

    #[error("Serialize value `{0:?}` failed with error: {1}")]
    SerializeFailed(Value, String),

    #[error("Deserialize value `{0:?}` failed with error: {1}")]
    DeserializeFailed(Value, String),

    #[error("Write value `{0:?}` failed with error: {1}")]
    WriteValueFailed(Value, std::io::Error),

    #[error("Value of type unit `()` should not be used")]
    UseUnitValue,

    #[error("GcObject `{0:?}` did not exist in the heap")]
    GcObjectNotFound(GcHandle),

    #[error("GcObject `{0:?}` has type `{}` but expected type `{}`", .1.type_name(), .2.type_name())]
    GcObjectWrongType(GcHandle, GcKind, GcKind),

    #[error("GcObject with type `{}` is not indexable", .0.type_name())]
    GcObjectUnIndexable(GcKind),

    #[error("GcObject with type `{}` is not a struct; cannot access member `{1:?}`", .0.type_name())]
    GcObjectNotStruct(GcKind, Identifier),

    #[error("GcObject with type `{}` is not a tuple; cannot access member `{1}`", .0.type_name())]
    GcObjectNotTuple(GcKind, usize),

    #[error("String with Id `{:?}` does not exist in the heap interner", .0)]
    StringNotFoundOnHeap(StrId),

    #[error("Scope depth limit ({0}) exceeded")]
    ScopeOverflow(usize),

    #[error("Scope underflow: attempted to pop the last scope")]
    ScopeUnderflow,

    #[error("Internal: module evaluation left the scope stack unbalanced (expected 1, got {0})")]
    ModuleScopeStackUnbalanced(usize),

    #[error("Assertion failed: {0}")]
    AssertionFailed(String),

    #[error("Struct value does not have field `{0:?}`")]
    StructFieldNotFound(Id, Identifier),

    #[error("Tuple have `{0}` members and cannot be indexed with member {1}")]
    TupleIndexOutOfBound(usize, usize),
}

impl InterpretError {
    pub fn generate_user_facing_error(
        &self,
        source_name: Option<&str>,
        input: &str,
        sb: &IdentifierRegistry,
    ) -> String {
        let description = self.resolve_description(sb);
        let source_name = source_name.unwrap_or("");

        // Interpret errors don't carry source spans, so show just the message
        // unless a future variant adds span support
        let _ = input;
        format!("Runtime Error: {description}\n  --> {source_name}\n")
    }

    // TODO: remove all debug print in here
    fn resolve_description(&self, sb: &IdentifierRegistry) -> String {
        match self {
            Self::ReDeclareVariable(node) => {
                format!(
                    "Variable '{}' has already been declared in this scope.",
                    sb.get_or_unknown(node.id),
                )
            }
            Self::NotFoundVariable(node) => {
                format!(
                    "Variable '{}' is not defined in the current scope.",
                    sb.get_or_unknown(node.id)
                )
            }
            Self::VariableReadOnly(node) => {
                format!(
                    "Cannot reassign '{}': variable is read-only in this scope.",
                    sb.get_or_unknown(node.id)
                )
            }
            Self::UnknownOperation(op) => {
                format!("The operator `{op:?}` is not recognized.")
            }
            Self::InvalidOperationOnType(op, val) => {
                format!("The operator `{op:?}` cannot be applied to value `{val:?}`.")
            }
            Self::MismatchType(op, left, right) => {
                format!(
                    "The operator `{op:?}` cannot be applied to `{left:?}` and `{right:?}`: type mismatch."
                )
            }
            Self::ConditionNotBool(val) => {
                format!("Expected a boolean condition, but got `{val:?}`.")
            }
            Self::DivideByZero => "Division by zero is not allowed.".to_string(),
            Self::ValueNotCallable(val) => {
                format!("`{val:?}` is not a function and cannot be called.")
            }
            Self::WrongNumberOfArgument(val, expected, actual) => {
                format!("`{val:?}` expects {expected} argument(s) but received {actual}.")
            }
            Self::WrongNumberOfArgumentAtLeast(val, min, actual) => {
                format!("`{val:?}` requires at least {min} argument(s) but received {actual}.")
            }
            Self::WrongArgumentType(callable, arg, expected_type) => {
                format!(
                    "`{callable:?}` received argument `{arg:?}` but expected type `{expected_type}`."
                )
            }
            Self::ValueUnIndexable(val) => {
                format!("`{val:?}` is not an array or map and cannot be indexed.")
            }
            Self::ValueCannotBeUsedAsKey(val) => {
                format!("`{val:?}` cannot be used as a key for an array or map.")
            }
            Self::ValueMustBeUsize(val) => {
                format!(
                    "`{val:?}` is not a non-negative integer; array indices must be non-negative integers."
                )
            }
            Self::ArrayOutOfBound(len, index) => {
                format!("Index {index} is out of bounds: the array has length {len}.")
            }
            Self::ModuleNotFoundInPath(module, path) => {
                format!("Module '{module}' could not be found at path '{path}'.")
            }
            Self::ReadModuleFailed(module, path, err) => {
                format!("Failed to read module '{module}' at '{path}': {err}.")
            }
            Self::ParseModuleFailed(module, path, err) => {
                format!("Failed to parse module '{module}' at '{path}': {err}.")
            }
            Self::TypeCheckModuleFailed(module, path, err) => {
                format!("Type error in module '{module}' at '{path}': {err}.")
            }
            Self::InterpretModuleFailed(module, path, err) => {
                format!("Runtime error in module '{module}' at '{path}': {err}.")
            }
            Self::ValueCannotBeUsedAsSourceInFor(val) => {
                format!("Value `{val:?}` cannot be use as for loop source")
            }
            Self::TypeIsNotSerializable(val) => {
                format!("`{val:?}` cannot be serialized.")
            }
            Self::SerializeFailed(val, err) => {
                format!("Serialization of `{val:?}` failed: {err}.")
            }
            Self::DeserializeFailed(val, err) => {
                format!("Deserialization of `{val:?}` failed: {err}.")
            }
            Self::WriteValueFailed(val, err) => {
                format!("Writing `{val:?}` failed: {err}.")
            }
            Self::UseUnitValue => {
                "A unit value `()` was used where a real value was expected.".to_string()
            }
            Self::GcObjectNotFound(handle) => {
                format!("Internal error: GC object `{handle:?}` no longer exists in the heap.")
            }
            Self::GcObjectWrongType(handle, actual, expected) => {
                format!(
                    "Internal error: GC object `{handle:?}` has type `{}` but expected `{}`.",
                    actual.type_name(),
                    expected.type_name()
                )
            }
            Self::GcObjectUnIndexable(kind) => {
                format!(
                    "Internal error: GC object of type `{}` is not indexable.",
                    kind.type_name()
                )
            }
            Self::GcObjectNotStruct(kind, iden) => {
                format!(
                    "GC object of type `{}` is not a struct; cannot access member '{}'.",
                    kind.type_name(),
                    sb.get_or_unknown(iden.id)
                )
            }
            Self::GcObjectNotTuple(kind, idx) => {
                format!(
                    "GC object of type `{}` is not a tuple; cannot access member at index '{}'.",
                    kind.type_name(),
                    idx
                )
            }
            Self::StringNotFoundOnHeap(id) => {
                // TODO: call to the interner to get the actual string back
                format!(
                    "Internal error: string with id `{id:?}` does not exist in the string interner."
                )
            }
            Self::ScopeOverflow(limit) => {
                format!("Stack overflow: call depth exceeded the limit of {limit}.")
            }
            Self::ScopeUnderflow => "Internal error: attempted to pop the root scope.".to_string(),
            Self::ModuleScopeStackUnbalanced(depth) => {
                format!(
                    "Internal error: module evaluation left {depth} scope(s) on the stack; \
                     expected exactly 1 (the module's global scope)."
                )
            }
            Self::AssertionFailed(msg) => {
                format!("Assertion failed: {msg}")
            }
            Self::StructFieldNotFound(id, iden) => {
                format!(
                    "Value {} does not have a field named '{}'.",
                    sb.get_or_unknown(*id),
                    sb.get_or_unknown(iden.id)
                )
            }
            Self::TupleIndexOutOfBound(len, index) => {
                format!("Index {index} is out of bounds: the Tuple has {len} members.")
            }
        }
    }
}
