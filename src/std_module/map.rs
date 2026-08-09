use std::collections::HashMap;

use super::helpers::{check_exact_args, get_map_arg};
use crate::{
    id::Id,
    interpret::{self, BorrowContext, GcObject, InterpretError, MapKey, Number, Value},
    module::{ModuleIdentity, ModuleStringInterner},
    typecheck,
    types::{Type, TypeInterner},
};

// ---- std:map -----------------------------------------------------------

pub(crate) fn map_typecheck_module(
    msi: &mut ModuleStringInterner,
    type_interner: &mut TypeInterner,
) -> (ModuleIdentity, typecheck::Module) {
    let identity = map_identity(msi);
    let mut symbol_scope = crate::types::TypeScope::new();

    let length_type = type_interner.intern_type(&Type::FUNCTION_ANY_TO_NUMBER);
    symbol_scope.associate(Id::new("length"), length_type.0);

    let keys_type = type_interner.intern_type(&Type::FUNCTION_ANY_TO_ANY);
    symbol_scope.associate(Id::new("keys"), keys_type.0);

    let values_type = type_interner.intern_type(&Type::FUNCTION_ANY_TO_ANY);
    symbol_scope.associate(Id::new("values"), values_type.0);

    let insert_type = type_interner.intern_type(&Type::FUNCTION_ANY_ANY_ANY_TO_NIL);
    symbol_scope.associate(Id::new("insert"), insert_type.0);

    let remove_type = type_interner.intern_type(&Type::FUNCTION_ANY_ANY_TO_ANY);
    symbol_scope.associate(Id::new("remove"), remove_type.0);

    let module = typecheck::Module {
        symbol_scope,
        struct_scope: crate::types::TypeScope::new(),
    };

    (identity, module)
}

pub(crate) fn map_interpret_module(msi: &mut ModuleStringInterner) -> (ModuleIdentity, interpret::Module) {
    let identity = map_identity(msi);

    let mut variables = HashMap::new();
    variables.insert(Id::new("length"), Value::BuiltinFunction(map_length_fn));
    variables.insert(Id::new("keys"), Value::BuiltinFunction(map_keys_fn));
    variables.insert(Id::new("values"), Value::BuiltinFunction(map_values_fn));
    variables.insert(Id::new("insert"), Value::BuiltinFunction(map_insert_fn));
    variables.insert(Id::new("remove"), Value::BuiltinFunction(map_remove_fn));

    let module = interpret::Module::new(variables);

    (identity, module)
}

// ---- map function implementations -------------------------------------

fn map_identity(msi: &mut ModuleStringInterner) -> ModuleIdentity {
    ModuleIdentity {
        resolved_path: msi.intern("std:map"),
        is_std: true,
    }
}

fn map_length_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(map_length_fn, &args, 1)?;
    let handle = get_map_arg(map_length_fn, args[0])?;
    let len = ctx.environment.get_map(handle)?.len();
    Ok(Value::make_number(Number::Integer(len as i64)))
}

fn map_keys_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(map_keys_fn, &args, 1)?;
    let handle = get_map_arg(map_keys_fn, args[0])?;
    let keys: Vec<Value> = {
        let map = ctx.environment.get_map(handle)?;
        map.keys()
            .map(|k| match k {
                MapKey::Scalar(s) => Value::Scalar(*s),
                MapKey::Str(id) => Value::Str(*id),
            })
            .collect()
    };
    for k in &keys {
        ctx.environment.heap.shallow_copy_value(*k);
    }
    Ok(ctx.environment.insert_array_variable(keys))
}

fn map_values_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(map_values_fn, &args, 1)?;
    let handle = get_map_arg(map_values_fn, args[0])?;
    let values: Vec<Value> = {
        let map = ctx.environment.get_map(handle)?;
        map.values().copied().collect()
    };
    for v in &values {
        ctx.environment.heap.shallow_copy_value(*v);
    }
    Ok(ctx.environment.insert_array_variable(values))
}

fn map_insert_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(map_insert_fn, &args, 3)?;
    let handle = get_map_arg(map_insert_fn, args[0])?;
    let key = MapKey::convert_from_value(args[1])?;
    let new_value = args[2];
    ctx.environment.heap.shallow_copy_value(new_value);
    let Some(GcObject::Map(map)) = ctx.environment.heap.get_object_mut(handle) else {
        return Err(InterpretError::GcObjectNotFound(handle));
    };
    Ok(map.insert(key, new_value).unwrap_or(Value::make_nil()))
}

fn map_remove_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(map_remove_fn, &args, 2)?;
    let handle = get_map_arg(map_remove_fn, args[0])?;
    let key = MapKey::convert_from_value(args[1])?;
    let Some(GcObject::Map(map)) = ctx.environment.heap.get_object_mut(handle) else {
        return Err(InterpretError::GcObjectNotFound(handle));
    };
    Ok(map.remove(&key).unwrap_or(Value::make_nil()))
}
