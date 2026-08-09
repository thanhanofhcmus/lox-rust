use super::helpers::{check_exact_args, check_min_args, get_array_arg};
use crate::{
    interpret::{BorrowContext, GcObject, InterpretError, Number, Value},
    types::Type,
};

// ---- std:array functions ----------------------------------------------

crate::std_fn! {
    "std:array", "length", Type::FUNCTION_ANY_TO_NUMBER,
    fn array_length_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(array_length_fn, &args, 1)?;
        let handle = get_array_arg(array_length_fn, args[0])?;
        let len = ctx.environment.get_array(handle)?.len();
        Ok(Value::make_number(Number::Integer(len as i64)))
    }
}

crate::std_fn! {
    "std:array", "push", Type::FUNCTION_ANY_VARIADIC_ANY_TO_UNIT,
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
}

crate::std_fn! {
    "std:array", "pop", Type::FUNCTION_ANY_TO_ANY,
    fn array_pop_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(array_pop_fn, &args, 1)?;
        let handle = get_array_arg(array_pop_fn, args[0])?;
        let Some(GcObject::Array(arr)) = ctx.environment.heap.get_object_mut(handle) else {
            return Err(InterpretError::GcObjectNotFound(handle));
        };
        Ok(arr.pop().unwrap_or(Value::make_nil()))
    }
}

crate::std_fn! {
    "std:array", "insert", Type::FUNCTION_ANY_NUMBER_VARIADIC_ANY_TO_UNIT,
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
}

crate::collect_std_fns! {
    "std:array",
    functions: [array_length_fn, array_push_fn, array_pop_fn, array_insert_fn],
    constants: []
}
