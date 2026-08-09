use super::helpers::{check_exact_args, get_map_arg};
use crate::{
    interpret::{BorrowContext, GcObject, InterpretError, MapKey, Number, Value},
    types::Type,
};

// ---- std:map functions ------------------------------------------------

crate::std_fn! {
    "std:map", "length", Type::FUNCTION_ANY_TO_NUMBER,
    fn map_length_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(map_length_fn, &args, 1)?;
        let handle = get_map_arg(map_length_fn, args[0])?;
        let len = ctx.environment.get_map(handle)?.len();
        Ok(Value::make_number(Number::Integer(len as i64)))
    }
}

crate::std_fn! {
    "std:map", "keys", Type::FUNCTION_ANY_TO_ANY,
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
}

crate::std_fn! {
    "std:map", "values", Type::FUNCTION_ANY_TO_ANY,
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
}

crate::std_fn! {
    "std:map", "insert", Type::FUNCTION_ANY_ANY_ANY_TO_NIL,
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
}

crate::std_fn! {
    "std:map", "remove", Type::FUNCTION_ANY_ANY_TO_ANY,
    fn map_remove_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(map_remove_fn, &args, 2)?;
        let handle = get_map_arg(map_remove_fn, args[0])?;
        let key = MapKey::convert_from_value(args[1])?;
        let Some(GcObject::Map(map)) = ctx.environment.heap.get_object_mut(handle) else {
            return Err(InterpretError::GcObjectNotFound(handle));
        };
        Ok(map.remove(&key).unwrap_or(Value::make_nil()))
    }
}

crate::collect_std_fns! {
    "std:map",
    functions: [map_length_fn, map_keys_fn, map_values_fn, map_insert_fn, map_remove_fn],
    constants: []
}
