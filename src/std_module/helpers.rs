use crate::interpret::{BuiltinFn, GcHandle, InterpretError, Value};

pub(crate) fn check_exact_args(func: BuiltinFn, args: &[Value], expected: usize) -> Result<(), InterpretError> {
    if args.len() != expected {
        return Err(InterpretError::WrongNumberOfArgument(
            Value::BuiltinFunction(func),
            expected,
            args.len(),
        ));
    }
    Ok(())
}

pub(crate) fn check_min_args(func: BuiltinFn, args: &[Value], min: usize) -> Result<(), InterpretError> {
    if args.len() < min {
        return Err(InterpretError::WrongNumberOfArgumentAtLeast(
            Value::BuiltinFunction(func),
            min,
            args.len(),
        ));
    }
    Ok(())
}

pub(crate) fn get_array_arg(func: BuiltinFn, arg: Value) -> Result<GcHandle, InterpretError> {
    match arg {
        Value::Array(handle) => Ok(handle),
        _ => Err(InterpretError::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "array",
        )),
    }
}

pub(crate) fn get_map_arg(func: BuiltinFn, arg: Value) -> Result<GcHandle, InterpretError> {
    match arg {
        Value::Map(handle) => Ok(handle),
        _ => Err(InterpretError::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "map",
        )),
    }
}

pub(crate) fn get_bool_arg(func: BuiltinFn, arg: Value) -> Result<bool, InterpretError> {
    match arg.get_bool() {
        Some(b) => Ok(b),
        None => Err(InterpretError::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "bool",
        )),
    }
}
