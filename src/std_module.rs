use std::collections::HashMap;

use crate::{
    id::Id,
    interpret::{
        self, BorrowContext, BuiltinFn, GcHandle, GcObject, InterpretError, MapKey, Number, SerialValue, Value,
    },
    module::{ModuleIdentity, ModuleStringInterner},
    typecheck,
    types::{Type, TypeId, TypeInterner},
};

/// Returns pre-built typecheck modules for all known std modules.
pub fn create_typecheck_modules(
    msi: &mut ModuleStringInterner,
    type_interner: &mut TypeInterner,
) -> Vec<(ModuleIdentity, typecheck::Module)> {
    let modules = vec![
        array_typecheck_module(msi, type_interner),
        map_typecheck_module(msi, type_interner),
        json_typecheck_module(msi, type_interner),
    ];
    modules
}

/// Returns pre-built interpret modules for all known std modules.
pub fn create_interpret_modules(msi: &mut ModuleStringInterner) -> Vec<(ModuleIdentity, interpret::Module)> {
    let modules = vec![
        array_interpret_module(msi),
        map_interpret_module(msi),
        json_interpret_module(msi),
    ];
    modules
}

// ---- std:array --------------------------------------------------------

fn array_identity(msi: &mut ModuleStringInterner) -> ModuleIdentity {
    ModuleIdentity {
        resolved_path: msi.intern("std:array"),
        is_std: true,
    }
}

fn array_typecheck_module(
    msi: &mut ModuleStringInterner,
    type_interner: &mut TypeInterner,
) -> (ModuleIdentity, typecheck::Module) {
    let identity = array_identity(msi);

    let mut symbol_scope = crate::types::TypeScope::new();

    // length: (any) -> number
    let length_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::ANY],
        variadic: None,
        return_: TypeId::NUMBER,
    });
    symbol_scope.associate(Id::new("length"), length_type.0);

    // push: (any, variadic any) -> unit
    let push_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::ANY],
        variadic: Some(TypeId::ANY),
        return_: TypeId::UNIT,
    });
    symbol_scope.associate(Id::new("push"), push_type.0);

    // pop: (any) -> any
    let pop_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::ANY],
        variadic: None,
        return_: TypeId::ANY,
    });
    symbol_scope.associate(Id::new("pop"), pop_type.0);

    // insert: (any, number, variadic any) -> unit
    let insert_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::ANY, TypeId::NUMBER],
        variadic: Some(TypeId::ANY),
        return_: TypeId::UNIT,
    });
    symbol_scope.associate(Id::new("insert"), insert_type.0);

    let module = typecheck::Module {
        symbol_scope,
        struct_scope: crate::types::TypeScope::new(),
    };

    (identity, module)
}

fn array_interpret_module(msi: &mut ModuleStringInterner) -> (ModuleIdentity, interpret::Module) {
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

// ---- argument validation helpers (duplicated from prelude) ------------

fn check_exact_args(func: BuiltinFn, args: &[Value], expected: usize) -> Result<(), InterpretError> {
    if args.len() != expected {
        return Err(InterpretError::WrongNumberOfArgument(
            Value::BuiltinFunction(func),
            expected,
            args.len(),
        ));
    }
    Ok(())
}

fn check_min_args(func: BuiltinFn, args: &[Value], min: usize) -> Result<(), InterpretError> {
    if args.len() < min {
        return Err(InterpretError::WrongNumberOfArgumentAtLeast(
            Value::BuiltinFunction(func),
            min,
            args.len(),
        ));
    }
    Ok(())
}

fn get_array_arg(func: BuiltinFn, arg: Value) -> Result<GcHandle, InterpretError> {
    match arg {
        Value::Array(handle) => Ok(handle),
        _ => Err(InterpretError::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "array",
        )),
    }
}

// ---- std:map -----------------------------------------------------------

fn map_identity(msi: &mut ModuleStringInterner) -> ModuleIdentity {
    ModuleIdentity {
        resolved_path: msi.intern("std:map"),
        is_std: true,
    }
}

fn map_typecheck_module(
    msi: &mut ModuleStringInterner,
    type_interner: &mut TypeInterner,
) -> (ModuleIdentity, typecheck::Module) {
    let identity = map_identity(msi);
    let mut symbol_scope = crate::types::TypeScope::new();

    // length: (any) -> number
    let length_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::ANY],
        variadic: None,
        return_: TypeId::NUMBER,
    });
    symbol_scope.associate(Id::new("length"), length_type.0);

    // keys: (any) -> any   (returns array)
    let keys_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::ANY],
        variadic: None,
        return_: TypeId::ANY,
    });
    symbol_scope.associate(Id::new("keys"), keys_type.0);

    // values: (any) -> any   (returns array)
    let values_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::ANY],
        variadic: None,
        return_: TypeId::ANY,
    });
    symbol_scope.associate(Id::new("values"), values_type.0);

    // insert: (any, any, any) -> nil
    let insert_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::ANY, TypeId::ANY, TypeId::ANY],
        variadic: None,
        return_: TypeId::NIL,
    });
    symbol_scope.associate(Id::new("insert"), insert_type.0);

    // remove: (any, any) -> any
    let remove_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::ANY, TypeId::ANY],
        variadic: None,
        return_: TypeId::ANY,
    });
    symbol_scope.associate(Id::new("remove"), remove_type.0);

    let module = typecheck::Module {
        symbol_scope,
        struct_scope: crate::types::TypeScope::new(),
    };

    (identity, module)
}

fn map_interpret_module(msi: &mut ModuleStringInterner) -> (ModuleIdentity, interpret::Module) {
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

fn get_map_arg(func: BuiltinFn, arg: Value) -> Result<GcHandle, InterpretError> {
    match arg {
        Value::Map(handle) => Ok(handle),
        _ => Err(InterpretError::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "map",
        )),
    }
}

// ---- std:json ----------------------------------------------------------

fn json_identity(msi: &mut ModuleStringInterner) -> ModuleIdentity {
    ModuleIdentity {
        resolved_path: msi.intern("std:json"),
        is_std: true,
    }
}

fn json_typecheck_module(
    msi: &mut ModuleStringInterner,
    type_interner: &mut TypeInterner,
) -> (ModuleIdentity, typecheck::Module) {
    let identity = json_identity(msi);
    let mut symbol_scope = crate::types::TypeScope::new();

    // parse: (str) -> any
    let parse_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::STR],
        variadic: None,
        return_: TypeId::ANY,
    });
    symbol_scope.associate(Id::new("parse"), parse_type.0);

    // stringify: (any, variadic bool) -> str
    let stringify_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::ANY],
        variadic: Some(TypeId::BOOL),
        return_: TypeId::STR,
    });
    symbol_scope.associate(Id::new("stringify"), stringify_type.0);

    let module = typecheck::Module {
        symbol_scope,
        struct_scope: crate::types::TypeScope::new(),
    };

    (identity, module)
}

fn json_interpret_module(msi: &mut ModuleStringInterner) -> (ModuleIdentity, interpret::Module) {
    let identity = json_identity(msi);

    let mut variables = HashMap::new();
    variables.insert(Id::new("parse"), Value::BuiltinFunction(json_parse_fn));
    variables.insert(Id::new("stringify"), Value::BuiltinFunction(json_stringify_fn));

    let module = interpret::Module::new(variables);

    (identity, module)
}

// ---- json function implementations ------------------------------------

fn json_parse_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(json_parse_fn, &args, 1)?;
    let value = args[0];
    let s = match value {
        Value::Str(str_id) => ctx.environment.get_string(str_id)?.to_string(),
        _ => {
            return Err(InterpretError::WrongArgumentType(
                Value::BuiltinFunction(json_parse_fn),
                value,
                "str",
            ));
        }
    };
    let serial_value =
        serde_json::from_str::<SerialValue>(&s).map_err(|e| InterpretError::DeserializeFailed(value, e.to_string()))?;
    let value = serial_value.hydrate(ctx.environment)?;
    Ok(value)
}

fn json_stringify_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_min_args(json_stringify_fn, &args, 1)?;
    let value = args[0];

    let serial_value = SerialValue::convert_from_value(value, ctx.environment, ctx.identifier_registry)?;

    let is_print_pretty = get_bool_arg(
        json_stringify_fn,
        args.get(1).copied().unwrap_or(Value::make_bool(false)),
    )?;
    let result = if is_print_pretty {
        serde_json::to_string_pretty(&serial_value)
    } else {
        serde_json::to_string(&serial_value)
    };
    match result {
        Ok(v) => Ok(ctx.environment.insert_string_variable(v)),
        Err(err) => Err(InterpretError::SerializeFailed(value, err.to_string())),
    }
}

fn get_bool_arg(func: BuiltinFn, arg: Value) -> Result<bool, InterpretError> {
    match arg.get_bool() {
        Some(b) => Ok(b),
        None => Err(InterpretError::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "bool",
        )),
    }
}
