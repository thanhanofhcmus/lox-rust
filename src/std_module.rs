use std::collections::HashMap;

use crate::{
    id::Id,
    interpret::{
        self, BorrowContext, GcHandle, GcObject, InterpretError, BuiltinFn, Number, Value,
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
    let mut modules = Vec::new();
    modules.push(array_typecheck_module(msi, type_interner));
    modules
}

/// Returns pre-built interpret modules for all known std modules.
pub fn create_interpret_modules(
    msi: &mut ModuleStringInterner,
) -> Vec<(ModuleIdentity, interpret::Module)> {
    let mut modules = Vec::new();
    modules.push(array_interpret_module(msi));
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
