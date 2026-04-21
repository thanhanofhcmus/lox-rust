use crate::interpret::{interpreter::BorrowContext, values::DisplayWriter};
use std::collections::HashMap;

use crate::{
    id::Id,
    interpret::{
        debug_string::DebugString,
        error::InterpretError,
        heap::{GcHandle, GcObject, StrId},
        values::{BuiltinFn, MapKey, Number, SerialValue, Value},
    },
};

pub fn create() -> HashMap<Id, Value> {
    let mut preludes = HashMap::new();

    preludes.insert(Id::new("print"), Value::BuiltinFunction(print_fn));
    preludes.insert(Id::new("assert"), Value::BuiltinFunction(assert_fn));
    preludes.insert(Id::new("from_json"), Value::BuiltinFunction(from_json_fn));
    preludes.insert(Id::new("to_json"), Value::BuiltinFunction(to_json_fn));

    preludes.insert(
        Id::new("array_length"),
        Value::BuiltinFunction(array_length_fn),
    );
    preludes.insert(Id::new("array_push"), Value::BuiltinFunction(array_push_fn));
    preludes.insert(Id::new("array_pop"), Value::BuiltinFunction(array_pop_fn));
    preludes.insert(
        Id::new("array_insert"),
        Value::BuiltinFunction(array_insert_fn),
    );

    preludes.insert(Id::new("map_length"), Value::BuiltinFunction(map_length_fn));
    preludes.insert(Id::new("map_keys"), Value::BuiltinFunction(map_keys_fn));
    preludes.insert(Id::new("map_values"), Value::BuiltinFunction(map_values_fn));
    preludes.insert(Id::new("map_insert"), Value::BuiltinFunction(map_insert_fn));
    preludes.insert(Id::new("map_remove"), Value::BuiltinFunction(map_remove_fn));

    preludes.insert(Id::new("_dbg_print"), Value::BuiltinFunction(dbg_print_fn));
    preludes.insert(Id::new("_dbg_state"), Value::BuiltinFunction(dbg_state_fn));
    preludes.insert(Id::new("_dbg_gc_mark"), Value::BuiltinFunction(dbg_gc_mark));
    preludes.insert(
        Id::new("_dbg_gc_sweep"),
        Value::BuiltinFunction(dbg_gc_sweep),
    );
    preludes.insert(
        Id::new("_dbg_gc_mark_sweep"),
        Value::BuiltinFunction(dbg_gc_mark_sweep),
    );
    preludes.insert(
        Id::new("_dbg_heap_stats"),
        Value::BuiltinFunction(dbg_heap_stats),
    );

    preludes
}

fn dbg_state_fn(ctx: &mut BorrowContext, _: Vec<Value>) -> Result<Value, InterpretError> {
    eprintln!("{}", ctx.environment.debug_state_string());
    Ok(Value::make_nil())
}

fn dbg_print_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    let mut print_writer = ctx.print_writer.borrow_mut();

    // TODO: handle write! error
    for value in args {
        write!(print_writer, "{:?}", value).unwrap();
        match value {
            Value::Str(str_id) => {
                let s = ctx.environment.get_string(str_id)?;
                write!(print_writer, ": {}", s.escape_debug()).unwrap();
            }
            Value::Function(handle) => match ctx.environment.heap.get_object(handle) {
                Some(obj) => write!(print_writer, ": {:?}", obj).unwrap(),
                None => write!(print_writer, ": No Object").unwrap(),
            },
            _ => {}
        }
    }
    writeln!(print_writer).unwrap();

    Ok(Value::make_nil())
}

fn dbg_heap_stats(ctx: &mut BorrowContext, _: Vec<Value>) -> Result<Value, InterpretError> {
    eprintln!("{}", ctx.environment.heap.get_stats().debug_string());
    Ok(Value::make_nil())
}

fn dbg_gc_mark(ctx: &mut BorrowContext, _: Vec<Value>) -> Result<Value, InterpretError> {
    ctx.environment
        .heap
        .mark(ctx.environment.collect_all_variables());
    Ok(Value::make_nil())
}

fn dbg_gc_sweep(ctx: &mut BorrowContext, _: Vec<Value>) -> Result<Value, InterpretError> {
    ctx.environment.heap.sweep();
    Ok(Value::make_nil())
}

fn dbg_gc_mark_sweep(ctx: &mut BorrowContext, _: Vec<Value>) -> Result<Value, InterpretError> {
    ctx.environment
        .heap
        .mark(ctx.environment.collect_all_variables());
    ctx.environment.heap.sweep();
    Ok(Value::make_nil())
}

fn print_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    let mut print_writer = ctx.print_writer.borrow_mut();

    // TODO: handle write! error
    for value in args {
        if let Value::Str(str_id) = value {
            let s = ctx.environment.get_string(str_id)?;
            write!(print_writer, "{}", s).unwrap();
        } else {
            value.write_display(ctx, &mut *print_writer)?;
        }
    }
    writeln!(print_writer).unwrap();

    Ok(Value::make_nil())
}

fn assert_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(assert_fn, &args, 2)?;

    let condition = args[0];
    let message_val = args[1];

    match condition.get_bool() {
        Some(true) => Ok(Value::make_nil()),
        Some(false) => {
            if ctx.strict_assert {
                let msg = stringify_assert_message(ctx, message_val)?;
                Err(InterpretError::AssertionFailed(msg))
            } else {
                print_fn(ctx, vec![message_val])
            }
        }
        None => {
            if ctx.strict_assert {
                let msg = stringify_assert_message(ctx, message_val)?;
                Err(InterpretError::AssertionFailed(format!(
                    "condition did not evaluate to a boolean: {msg}"
                )))
            } else {
                let warn_msg = ctx.environment.insert_string_variable(
                    "Assertion check value did not evaluate to a boolean".into(),
                );
                print_fn(ctx, vec![warn_msg])?;
                print_fn(ctx, vec![message_val])
            }
        }
    }
}

fn stringify_assert_message(ctx: &BorrowContext, value: Value) -> Result<String, InterpretError> {
    if let Value::Str(str_id) = value {
        return Ok(ctx.environment.get_string(str_id)?.to_string());
    }
    let mut buf: Vec<u8> = Vec::new();
    value.write_display(ctx, &mut buf)?;
    Ok(String::from_utf8_lossy(&buf).into_owned())
}

fn from_json_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(from_json_fn, &args, 1)?;
    let value = args[0];
    let str_id = get_str_arg(from_json_fn, value)?;
    let s = ctx.environment.get_string(str_id)?;
    let serial_value = serde_json::from_str::<SerialValue>(s)
        .map_err(|e| InterpretError::DeserializeFailed(value, e.to_string()))?;

    let value = serial_value.hydrate(ctx.environment)?;

    Ok(value)
}

fn to_json_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_min_args(to_json_fn, &args, 1)?;
    let value = args[0];

    let serial_value = SerialValue::convert_from_value(value, ctx.environment)?;

    // maybe also throw error here
    let is_print_pretty = args
        .get(1)
        .map(|v| v.get_bool())
        .unwrap_or_default()
        .unwrap_or(false);

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

// Array functions

fn array_length_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(array_length_fn, &args, 1)?;
    let handle = get_array_arg(array_length_fn, args[0])?;
    let len = ctx.environment.get_array(handle)?.len();
    Ok(Value::make_number(Number::Integer(len as i64)))
}

fn array_push_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_min_args(array_push_fn, &args, 1)?;
    let handle = get_array_arg(array_push_fn, args[0])?;
    // increment ref counts before taking the mutable heap reference
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
    // ownership of the popped value transfers to the caller; ref count stays at 1
    Ok(arr.pop().unwrap_or(Value::make_nil()))
}

fn array_insert_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_min_args(array_insert_fn, &args, 3)?;
    let handle = get_array_arg(array_insert_fn, args[0])?;
    let idx = args[1].to_index()?;
    // check bounds before the mutable borrow
    let len = ctx.environment.get_array(handle)?.len();
    if idx > len {
        return Err(InterpretError::ArrayOutOfBound(len, idx));
    }
    // increment ref counts before taking the mutable heap reference
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

// Map functions

fn map_length_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(map_length_fn, &args, 1)?;
    let handle = get_map_arg(map_length_fn, args[0])?;
    let len = ctx.environment.get_map(handle)?.len();
    Ok(Value::make_number(Number::Integer(len as i64)))
}

fn map_keys_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(map_keys_fn, &args, 1)?;
    let handle = get_map_arg(map_keys_fn, args[0])?;
    // collect into an owned Vec to release the immutable borrow before calling insert
    let keys: Vec<Value> = {
        let map = ctx.environment.get_map(handle)?;
        map.keys()
            .map(|k| match k {
                MapKey::Scalar(s) => Value::Scalar(*s),
                MapKey::Str(id) => Value::Str(*id),
            })
            .collect()
    };
    // shallow_copy_value is a no-op for Scalar/Str (no GcHandle), but call for correctness
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
    // increment ref count before taking the mutable heap reference
    ctx.environment.heap.shallow_copy_value(new_value);
    let Some(GcObject::Map(map)) = ctx.environment.heap.get_object_mut(handle) else {
        return Err(InterpretError::GcObjectNotFound(handle));
    };
    // ownership of the old value transfers to the caller; ref count stays at 1
    Ok(map.insert(key, new_value).unwrap_or(Value::make_nil()))
}

fn map_remove_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(map_remove_fn, &args, 2)?;
    let handle = get_map_arg(map_remove_fn, args[0])?;
    let key = MapKey::convert_from_value(args[1])?;
    let Some(GcObject::Map(map)) = ctx.environment.heap.get_object_mut(handle) else {
        return Err(InterpretError::GcObjectNotFound(handle));
    };
    // ownership of the removed value transfers to the caller; ref count stays at 1
    Ok(map.remove(&key).unwrap_or(Value::Unit))
}

// Argument validation helpers

fn check_exact_args(
    func: BuiltinFn,
    args: &[Value],
    expected: usize,
) -> Result<(), InterpretError> {
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

fn get_str_arg(func: BuiltinFn, arg: Value) -> Result<StrId, InterpretError> {
    match arg {
        Value::Str(str_id) => Ok(str_id),
        _ => Err(InterpretError::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "str",
        )),
    }
}

fn get_array_arg(func: BuiltinFn, arg: Value) -> Result<GcHandle, InterpretError> {
    match arg {
        Value::Array(handle) => Ok(handle),
        _ => Err(InterpretError::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "Array",
        )),
    }
}

fn get_map_arg(func: BuiltinFn, arg: Value) -> Result<GcHandle, InterpretError> {
    match arg {
        Value::Map(handle) => Ok(handle),
        _ => Err(InterpretError::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "Map",
        )),
    }
}
