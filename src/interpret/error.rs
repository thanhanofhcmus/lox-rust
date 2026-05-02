use super::values::Value;

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
    types::TypeInterner,
};

#[derive(Debug)]
pub enum InterpretError {
    ReDeclareVariable(Identifier),
    NotFoundVariable(Identifier),
    VariableReadOnly(Identifier),
    UnknownOperation(Token),
    InvalidOperationOnType(Token, Value),
    MismatchType(Token, Value, Value),
    ConditionNotBool(Value),
    DivideByZero,
    ValueNotCallable(Value),
    WrongNumberOfArgument(Value, usize, usize),
    WrongNumberOfArgumentAtLeast(Value, usize, usize),
    WrongArgumentType(Value, Value, &'static str),
    ValueUnIndexable(Value),
    ValueCannotBeUsedAsKey(Value),
    ValueMustBeUsize(Value),
    // TODO: add GcHandle to error to know which array is out of bound
    ArrayOutOfBound(usize, usize),
    ModuleNotFoundInPath(String, String),
    ReadModuleFailed(String, String, std::io::Error),
    ParseModuleFailed(String, String, ParseError),
    TypeCheckModuleFailed(String, String, typecheck::TypecheckError),
    InterpretModuleFailed(String, String, Box<InterpretError>),
    ValueCannotBeUsedAsSourceInFor(Value),
    TypeIsNotSerializable(Value),
    SerializeFailed(Value, String),
    DeserializeFailed(Value, String),
    WriteValueFailed(Value, std::io::Error),
    UseUnitValue,
    GcObjectNotFound(GcHandle),
    GcObjectWrongType(GcHandle, GcKind, GcKind),
    GcObjectUnIndexable(GcKind),
    GcObjectNotStructOrTuple(GcKind, MemberNode),
    MemberAccessOnInvalidType(Value, MemberNode),
    StringNotFoundOnHeap(StrId),
    ScopeOverflow(usize),
    ScopeUnderflow,
    ModuleScopeStackUnbalanced(usize),
    AssertionFailed(String),
    StructFieldNotFound(Id, Identifier),
    TupleIndexOutOfBound(usize, usize),
    CannotDestructureAsTuple(Value),
    TupleDestructureArityMismatch { expected: usize, actual: usize },
}

impl InterpretError {
    pub fn generate_user_facing_error(
        &self,
        source_name: Option<&str>,
        input: &str,
        env: &Environment,
        ir: &IdentifierRegistry,
        interner: &TypeInterner,
    ) -> String {
        let description = self.resolve_description(env, ir, interner);
        let source_name = source_name.map(|s| format!("\n  --> {s}\n")).unwrap_or("".to_string());

        // Interpret errors don't carry source spans, so show just the message
        // unless a future variant adds span support
        let _ = input;
        format!("Runtime Error: {description}{source_name}")
    }

    fn resolve_description(&self, env: &Environment, ir: &IdentifierRegistry, interner: &TypeInterner) -> String {
        match self {
            Self::ReDeclareVariable(node) => {
                format!(
                    "Variable '{}' has already been declared in this scope.",
                    ir.get_or_unknown(node.id),
                )
            }
            Self::NotFoundVariable(node) => {
                format!(
                    "Variable '{}' is not defined in the current scope.",
                    ir.get_or_unknown(node.id)
                )
            }
            Self::VariableReadOnly(node) => {
                format!(
                    "Cannot reassign '{}': variable is read-only in this scope.",
                    ir.get_or_unknown(node.id)
                )
            }
            Self::UnknownOperation(op) => {
                format!("The operator `{op}` is not recognized.")
            }
            Self::InvalidOperationOnType(op, val) => {
                format!(
                    "The operator `{op}` cannot be applied to value `{}`.",
                    format_value(*val, env, ir)
                )
            }
            Self::MismatchType(op, left, right) => {
                format!(
                    "The operator `{op}` cannot be applied to `{}` and `{}`: type mismatch.",
                    format_value(*left, env, ir),
                    format_value(*right, env, ir),
                )
            }
            Self::ConditionNotBool(val) => {
                format!(
                    "Expected a boolean condition, but got `{}`.",
                    format_value(*val, env, ir)
                )
            }
            Self::DivideByZero => "Division by zero is not allowed.".to_string(),
            Self::ValueNotCallable(val) => {
                format!(
                    "`{}` is not a function and cannot be called.",
                    format_value(*val, env, ir)
                )
            }
            Self::WrongNumberOfArgument(val, expected, actual) => {
                format!(
                    "`{}` expects {expected} argument(s) but received {actual}.",
                    format_value(*val, env, ir)
                )
            }
            Self::WrongNumberOfArgumentAtLeast(val, min, actual) => {
                format!(
                    "`{}` requires at least {min} argument(s) but received {actual}.",
                    format_value(*val, env, ir)
                )
            }
            Self::WrongArgumentType(callable, arg, expected_type) => {
                format!(
                    "`{}` received argument `{}` but expected type `{expected_type}`.",
                    format_value(*callable, env, ir),
                    format_value(*arg, env, ir),
                )
            }
            Self::ValueUnIndexable(val) => {
                format!(
                    "`{}` cannot be indexed with `[…]`; only arrays and maps support subscription.",
                    format_value(*val, env, ir)
                )
            }
            Self::ValueCannotBeUsedAsKey(val) => {
                format!(
                    "`{}` cannot be used as a key for an array or map.",
                    format_value(*val, env, ir)
                )
            }
            Self::ValueMustBeUsize(val) => {
                format!(
                    "`{}` is not a non-negative integer; array indices must be non-negative integers.",
                    format_value(*val, env, ir)
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
                format!(
                    "Type error in module '{module}' at '{path}': {}.",
                    err.resolve_description(ir, interner),
                )
            }
            Self::InterpretModuleFailed(module, path, err) => {
                format!(
                    "Runtime error in module '{module}' at '{path}': {}.",
                    err.resolve_description(env, ir, interner)
                )
            }
            Self::ValueCannotBeUsedAsSourceInFor(val) => {
                format!(
                    "Value `{}` cannot be used as a for-loop source",
                    format_value(*val, env, ir)
                )
            }
            Self::TypeIsNotSerializable(val) => {
                format!("`{}` cannot be serialized.", format_value(*val, env, ir))
            }
            Self::SerializeFailed(val, err) => {
                format!("Serialization of `{}` failed: {err}.", format_value(*val, env, ir))
            }
            Self::DeserializeFailed(val, err) => {
                format!("Deserialization of `{}` failed: {err}.", format_value(*val, env, ir))
            }
            Self::WriteValueFailed(val, err) => {
                // Avoid calling `format_value` here — WriteValueFailed came from
                // the display path, so re-entering it would risk a loop if the
                // heap is in a partial state.
                format!("Writing `{val:?}` failed: {err}.")
            }
            Self::UseUnitValue => "A unit value `()` was used where a real value was expected.".to_string(),
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
                    format_member(member, ir),
                )
            }
            Self::MemberAccessOnInvalidType(val, member) => {
                format!(
                    "`{}` is not a struct or tuple; cannot access member {}.",
                    format_value(*val, env, ir),
                    format_member(member, ir),
                )
            }
            Self::StringNotFoundOnHeap(id) => {
                // TODO: call to the interner to get the actual string back
                format!("Internal error: string with id `{id:?}` does not exist in the string interner.")
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
                    ir.get_or_unknown(*id),
                    ir.get_or_unknown(iden.id)
                )
            }
            Self::TupleIndexOutOfBound(len, index) => {
                format!("Index {index} is out of bounds: the Tuple has {len} members.")
            }
            Self::CannotDestructureAsTuple(val) => {
                format!("`{}` cannot be destructured as a tuple.", format_value(*val, env, ir))
            }
            Self::TupleDestructureArityMismatch { expected, actual } => {
                format!("Tuple destructuring expected {expected} member(s) but value has {actual}.")
            }
        }
    }
}

fn format_member(member: &MemberNode, ir: &IdentifierRegistry) -> String {
    match member {
        MemberNode::StructField(iden) => format!("'.{}'", ir.get_or_unknown(iden.id)),
        MemberNode::TupleIndex(idx) => format!("'.{idx}'"),
    }
}

/// Render a `Value` as the string a user would see in the source — via the
/// same `DisplayWriter` path the interpreter uses for `print`. Falls back to
/// the derived `Debug` representation if display fails (e.g. a stale heap
/// handle during error teardown), so error formatting never silently panics.
fn format_value(val: Value, env: &Environment, ir: &IdentifierRegistry) -> String {
    let mut buf: Vec<u8> = Vec::new();
    match val.write_display(env, ir, &mut buf) {
        Ok(()) => String::from_utf8_lossy(&buf).into_owned(),
        Err(_) => format!("{val:?}"),
    }
}
