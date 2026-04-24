use super::values::Value;
use thiserror::Error;

use crate::{
    ast::MemberNode,
    id::Id,
    identifier_registry::{Identifier, IdentifierRegistry},
    interpret::{
        Environment,
        heap::{GcHandle, GcKind, StrId},
        values::DisplayWriter,
    },
    parse::ParseError,
    token::Token,
    typecheck,
};

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

    #[error(
        "Value `{0:?}` cannot be indexed with `[…]`; only arrays and maps support subscription"
    )]
    ValueUnIndexable(Value),

    #[error("Value `{0:?}` cannot be used as a key for an array or map")]
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

    #[error("GcObject with type `{}` is not a struct or tuple; cannot access member `{1:?}`", .0.type_name())]
    GcObjectNotStructOrTuple(GcKind, MemberNode),

    #[error("Value `{0:?}` is not a struct or tuple; cannot access member `{1:?}`")]
    MemberAccessOnInvalidType(Value, MemberNode),

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

    #[error("Value `{0:?}` cannot be destructured as a tuple")]
    CannotDestructureAsTuple(Value),

    #[error("Tuple destructuring expected {expected} member(s) but value has {actual}")]
    TupleDestructureArityMismatch { expected: usize, actual: usize },
}

impl InterpretError {
    pub fn generate_user_facing_error(
        &self,
        source_name: Option<&str>,
        input: &str,
        env: &Environment,
        sb: &IdentifierRegistry,
    ) -> String {
        let description = self.resolve_description(env, sb);
        let source_name = source_name.unwrap_or("");

        // Interpret errors don't carry source spans, so show just the message
        // unless a future variant adds span support
        let _ = input;
        format!("Runtime Error: {description}\n  --> {source_name}\n")
    }

    fn resolve_description(&self, env: &Environment, sb: &IdentifierRegistry) -> String {
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
                format!(
                    "The operator `{op:?}` cannot be applied to value `{}`.",
                    format_value(*val, env, sb)
                )
            }
            Self::MismatchType(op, left, right) => {
                format!(
                    "The operator `{op:?}` cannot be applied to `{}` and `{}`: type mismatch.",
                    format_value(*left, env, sb),
                    format_value(*right, env, sb),
                )
            }
            Self::ConditionNotBool(val) => {
                format!(
                    "Expected a boolean condition, but got `{}`.",
                    format_value(*val, env, sb)
                )
            }
            Self::DivideByZero => "Division by zero is not allowed.".to_string(),
            Self::ValueNotCallable(val) => {
                format!(
                    "`{}` is not a function and cannot be called.",
                    format_value(*val, env, sb)
                )
            }
            Self::WrongNumberOfArgument(val, expected, actual) => {
                format!(
                    "`{}` expects {expected} argument(s) but received {actual}.",
                    format_value(*val, env, sb)
                )
            }
            Self::WrongNumberOfArgumentAtLeast(val, min, actual) => {
                format!(
                    "`{}` requires at least {min} argument(s) but received {actual}.",
                    format_value(*val, env, sb)
                )
            }
            Self::WrongArgumentType(callable, arg, expected_type) => {
                format!(
                    "`{}` received argument `{}` but expected type `{expected_type}`.",
                    format_value(*callable, env, sb),
                    format_value(*arg, env, sb),
                )
            }
            Self::ValueUnIndexable(val) => {
                format!(
                    "`{}` cannot be indexed with `[…]`; only arrays and maps support subscription.",
                    format_value(*val, env, sb)
                )
            }
            Self::ValueCannotBeUsedAsKey(val) => {
                format!(
                    "`{}` cannot be used as a key for an array or map.",
                    format_value(*val, env, sb)
                )
            }
            Self::ValueMustBeUsize(val) => {
                format!(
                    "`{}` is not a non-negative integer; array indices must be non-negative integers.",
                    format_value(*val, env, sb)
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
                format!(
                    "Runtime error in module '{module}' at '{path}': {}.",
                    err.resolve_description(env, sb)
                )
            }
            Self::ValueCannotBeUsedAsSourceInFor(val) => {
                format!(
                    "Value `{}` cannot be used as a for-loop source",
                    format_value(*val, env, sb)
                )
            }
            Self::TypeIsNotSerializable(val) => {
                format!("`{}` cannot be serialized.", format_value(*val, env, sb))
            }
            Self::SerializeFailed(val, err) => {
                format!(
                    "Serialization of `{}` failed: {err}.",
                    format_value(*val, env, sb)
                )
            }
            Self::DeserializeFailed(val, err) => {
                format!(
                    "Deserialization of `{}` failed: {err}.",
                    format_value(*val, env, sb)
                )
            }
            Self::WriteValueFailed(val, err) => {
                // Avoid calling `format_value` here — WriteValueFailed came from
                // the display path, so re-entering it would risk a loop if the
                // heap is in a partial state.
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
            Self::GcObjectNotStructOrTuple(kind, member) => {
                format!(
                    "GC object of type `{}` is not a struct or tuple; cannot access member {}.",
                    kind.type_name(),
                    format_member(member, sb),
                )
            }
            Self::MemberAccessOnInvalidType(val, member) => {
                format!(
                    "`{}` is not a struct or tuple; cannot access member {}.",
                    format_value(*val, env, sb),
                    format_member(member, sb),
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
            Self::CannotDestructureAsTuple(val) => {
                format!(
                    "`{}` cannot be destructured as a tuple.",
                    format_value(*val, env, sb)
                )
            }
            Self::TupleDestructureArityMismatch { expected, actual } => {
                format!("Tuple destructuring expected {expected} member(s) but value has {actual}.")
            }
        }
    }
}

fn format_member(member: &MemberNode, sb: &IdentifierRegistry) -> String {
    match member {
        MemberNode::StructField(iden) => format!("'.{}'", sb.get_or_unknown(iden.id)),
        MemberNode::TupleIndex(idx) => format!("'.{idx}'"),
    }
}

/// Render a `Value` as the string a user would see in the source — via the
/// same `DisplayWriter` path the interpreter uses for `print`. Falls back to
/// the derived `Debug` representation if display fails (e.g. a stale heap
/// handle during error teardown), so error formatting never silently panics.
fn format_value(val: Value, env: &Environment, sb: &IdentifierRegistry) -> String {
    let mut buf: Vec<u8> = Vec::new();
    match val.write_display(env, sb, &mut buf) {
        Ok(()) => String::from_utf8_lossy(&buf).into_owned(),
        Err(_) => format!("{val:?}"),
    }
}
