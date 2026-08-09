use std::collections::HashMap;

use super::helpers::{check_exact_args, check_min_args, get_array_arg};
use crate::{
    id::Id,
    interpret::{self, BorrowContext, GcObject, InterpretError, Number, Value},
    module::{ModuleIdentity, ModuleStringInterner},
    typecheck,
    types::{Type, TypeInterner},
};

// ---- std:array --------------------------------------------------------

pub(crate) fn array_typecheck_module(
    msi: &mut ModuleStringInterner,
    type_interner: &mut TypeInterner,
) -> (ModuleIdentity, typecheck::Module) {
    let identity = array_identity(msi);

    let mut symbol_scope = crate::types::TypeScope::new();

    let length_type = type_interner.intern_type(&Type::FUNCTION_ANY_TO_NUMBER);
    symbol_scope.associate(Id::new("length"), length_type.0);

    let push_type = type_interner.intern_type(&Type::FUNCTION_ANY_VARIADIC_ANY_TO_UNIT);
    symbol_scope.associate(Id::new("push"), push_type.0);

    let pop_type = type_interner.intern_type(&Type::FUNCTION_ANY_TO_ANY);
    symbol_scope.associate(Id::new("pop"), pop_type.0);

    let insert_type = type_interner.intern_type(&Type::FUNCTION_ANY_NUMBER_VARIADIC_ANY_TO_UNIT);
    symbol_scope.associate(Id::new("insert"), insert_type.0);

    let module = typecheck::Module {
        symbol_scope,
        struct_scope: crate::types::TypeScope::new(),
    };

    (identity, module)
}

pub(crate) fn array_interpret_module(msi: &mut ModuleStringInterner) -> (ModuleIdentity, interpret::Module) {
    let identity = array_identity(msi);

    let mut variables = HashMap::new();
    variables.insert(Id::new("length"), Value::BuiltinFunction(array_length_fn));
    variables.insert(Id::new("push"), Value::BuiltinFunction(array_push_fn));
    variables.insert(Id::new("pop"), Value::BuiltinFunction(array_pop_fn));
    variables.insert(Id::new("insert"), Value::BuiltinFunction(array_insert_fn));

    let module = interpret::Module::new(variables);

    (identity, module)
}

// ---- array function implementations ----------------------------------

fn array_identity(msi: &mut ModuleStringInterner) -> ModuleIdentity {
    ModuleIdentity {
        resolved_path: msi.intern("std:array"),
        is_std: true,
    }
}

fn array_length_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(array_length_fn, &args, 1)?;
    let handle = get_array_arg(array_length_fn, args[0])?;
    let len = ctx.environment.get_array(handle)?.len();
    Ok(Value::make_number(Number::Integer(len as i64)))
}

fn array_push_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_min_args(array_push_fn, &args, 1)?;
    let handle = get_array_arg(array_push_fn, args[0])?;
    for v in args.iter().skip(1) {
        ctx.environment.heap.shallow_copy_value(*v);
    }
    let Some(GcObject::Array(arr)) = ctx.environment.heap.get_object_mut(handle) else {
        return Err(InterpretError::GcObjectNotFound(handle));
    };
    for v in args.into_iter().skip(1) {
        arr.push(v);
    }
    Ok(Value::Unit)
}

fn array_pop_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(array_pop_fn, &args, 1)?;
    let handle = get_array_arg(array_pop_fn, args[0])?;
    let Some(GcObject::Array(arr)) = ctx.environment.heap.get_object_mut(handle) else {
        return Err(InterpretError::GcObjectNotFound(handle));
    };
    Ok(arr.pop().unwrap_or(Value::make_nil()))
}

fn array_insert_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_min_args(array_insert_fn, &args, 3)?;
    let handle = get_array_arg(array_insert_fn, args[0])?;
    let idx = args[1].to_index()?;
    let len = ctx.environment.get_array(handle)?.len();
    if idx > len {
        return Err(InterpretError::ArrayOutOfBound(len, idx));
    }
    for v in args.iter().skip(2) {
        ctx.environment.heap.shallow_copy_value(*v);
    }
    let Some(GcObject::Array(arr)) = ctx.environment.heap.get_object_mut(handle) else {
        return Err(InterpretError::GcObjectNotFound(handle));
    };
    for (i, v) in args.into_iter().skip(2).enumerate() {
        arr.insert(idx + i, v);
    }
    Ok(Value::Unit)
}
