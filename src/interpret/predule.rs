use crate::interpret::values::DisplayWriter;
use std::collections::HashMap;

use crate::{
    id::Id,
    interpret::{
        error::Error,
        heap::{GcHandle, GcObject},
        interpreter,
        values::{BuiltinFn, MapKey, Number, SerialValue, Value},
    },
};

pub fn create() -> HashMap<Id, Value> {
    let mut preludes = HashMap::new();

    preludes.insert(Id::new("print"), Value::BuiltinFunction(print_fn));
    preludes.insert(Id::new("assert"), Value::BuiltinFunction(assert_fn));
    preludes.insert(Id::new("from_json"), Value::BuiltinFunction(from_json_fn));
    preludes.insert(Id::new("to_json"), Value::BuiltinFunction(to_json_fn));

    preludes.insert(Id::new("array_len"), Value::BuiltinFunction(array_len_fn));
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

fn dbg_state_fn(itp: &mut interpreter::Interpreter, _: Vec<Value>) -> Result<Value, Error> {
    let env = &itp.environment;
    dbg!(&env);
    Ok(Value::make_nil())
}

fn dbg_print_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    let mut print_writer = itp.environment.get_print_writer();

    // TODO: handle write! error
    for value in args {
        write!(print_writer, "{:?}", value).unwrap();
        match value {
            Value::Str(str_id) => {
                let s = itp.environment.get_string(str_id)?;
                write!(print_writer, ": {}", s.escape_debug()).unwrap();
            }
            Value::Function(handle) => match itp.environment.heap.get_object(handle) {
                Some(obj) => write!(print_writer, ": {:?}", obj).unwrap(),
                None => write!(print_writer, ": No Object").unwrap(),
            },
            _ => {}
        }
    }
    writeln!(print_writer).unwrap();

    Ok(Value::make_nil())
}

fn dbg_heap_stats(itp: &mut interpreter::Interpreter, _: Vec<Value>) -> Result<Value, Error> {
    let stats = itp.environment.heap.get_stats();
    dbg!(stats);
    Ok(Value::make_nil())
}

fn dbg_gc_mark(itp: &mut interpreter::Interpreter, _: Vec<Value>) -> Result<Value, Error> {
    itp.environment
        .heap
        .mark(itp.environment.collect_all_variables());
    Ok(Value::make_nil())
}

fn dbg_gc_sweep(itp: &mut interpreter::Interpreter, _: Vec<Value>) -> Result<Value, Error> {
    itp.environment.heap.sweep();
    Ok(Value::make_nil())
}

fn dbg_gc_mark_sweep(itp: &mut interpreter::Interpreter, _: Vec<Value>) -> Result<Value, Error> {
    itp.environment
        .heap
        .mark(itp.environment.collect_all_variables());
    itp.environment.heap.sweep();
    Ok(Value::make_nil())
}

fn print_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    let mut print_writer = itp.environment.get_print_writer();

    // TODO: handle write! error
    for value in args {
        if let Value::Str(str_id) = value {
            let s = itp.environment.get_string(str_id)?;
            write!(print_writer, "{}", s).unwrap();
        } else {
            value.write_display(itp.environment, &mut *print_writer)?;
        }
    }
    writeln!(print_writer).unwrap();

    Ok(Value::make_nil())
}

fn assert_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    if args.len() != 2 {
        return Err(Error::WrongNumberOfArgument(
            Value::BuiltinFunction(assert_fn),
            2,
            args.len(),
        ));
    }

    let mut args_iter = args.into_iter();
    let condition = args_iter.next().unwrap();
    let message_val = args_iter.next().unwrap();

    match condition.get_bool() {
        Some(v) => {
            if v {
                Ok(Value::make_nil())
            } else {
                print_fn(itp, vec![message_val])
            }
        }
        _ => {
            let warn_msg = itp.environment.insert_string_variable(
                "Assertion check value did not evaluated to boolean".into(),
            );
            print_fn(itp, vec![warn_msg])?;
            print_fn(itp, vec![message_val])
        }
    }
}

fn from_json_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    let Some(value) = args.first() else {
        // TODO: maybe throw error here
        return Ok(Value::make_nil());
    };
    let value = *value;
    let Value::Str(handle) = value else {
        // TODO: maybe throw error here
        return Ok(Value::make_nil());
    };
    let s = itp.environment.get_string(handle)?;
    let serial_value = serde_json::from_str::<SerialValue>(s)
        .map_err(|e| Error::DeserializeFailed(value, e.to_string()))?;

    let value = serial_value.hydrate(itp.environment)?;

    Ok(value)
}

fn to_json_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    if args.is_empty() {
        return Ok(Value::make_nil());
    }
    let value = args[0];

    let serial_value = SerialValue::convert_from_value(value, itp.environment)?;

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
        Ok(v) => Ok(itp.environment.insert_string_variable(v)),
        Err(err) => Err(Error::SerializeFailed(value, err.to_string())),
    }
}

// Array functions

fn array_len_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    check_exact_args(array_len_fn, &args, 1)?;
    let handle = get_array_arg(array_len_fn, args[0])?;
    let len = itp.environment.get_array(handle)?.len();
    Ok(Value::make_number(Number::Integer(len as i64)))
}

fn array_push_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    check_min_args(array_push_fn, &args, 1)?;
    let handle = get_array_arg(array_push_fn, args[0])?;
    // increment ref counts before taking the mutable heap reference
    for v in args.iter().skip(1) {
        itp.environment.heap.shallow_copy_value(*v);
    }
    let Some(GcObject::Array(arr)) = itp.environment.heap.get_object_mut(handle) else {
        return Err(Error::GcObjectNotFound(handle));
    };
    for v in args.into_iter().skip(1) {
        arr.push(v);
    }
    Ok(Value::make_nil())
}

fn array_pop_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    check_exact_args(array_pop_fn, &args, 1)?;
    let handle = get_array_arg(array_pop_fn, args[0])?;
    let Some(GcObject::Array(arr)) = itp.environment.heap.get_object_mut(handle) else {
        return Err(Error::GcObjectNotFound(handle));
    };
    // ownership of the popped value transfers to the caller; ref count stays at 1
    Ok(arr.pop().unwrap_or(Value::Unit))
}

fn array_insert_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    check_min_args(array_insert_fn, &args, 3)?;
    let handle = get_array_arg(array_insert_fn, args[0])?;
    let idx = args[1].to_index()?;
    // check bounds before the mutable borrow
    let len = itp.environment.get_array(handle)?.len();
    if idx > len {
        return Err(Error::ArrayOutOfBound(len, idx));
    }
    // increment ref counts before taking the mutable heap reference
    for v in args.iter().skip(2) {
        itp.environment.heap.shallow_copy_value(*v);
    }
    let Some(GcObject::Array(arr)) = itp.environment.heap.get_object_mut(handle) else {
        return Err(Error::GcObjectNotFound(handle));
    };
    for (i, v) in args.into_iter().skip(2).enumerate() {
        arr.insert(idx + i, v);
    }
    Ok(Value::make_nil())
}

// Map functions

fn map_length_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    check_exact_args(map_length_fn, &args, 1)?;
    let handle = get_map_arg(map_length_fn, args[0])?;
    let len = itp.environment.get_map(handle)?.len();
    Ok(Value::make_number(Number::Integer(len as i64)))
}

fn map_keys_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    check_exact_args(map_keys_fn, &args, 1)?;
    let handle = get_map_arg(map_keys_fn, args[0])?;
    // collect into an owned Vec to release the immutable borrow before calling insert
    let keys: Vec<Value> = {
        let map = itp.environment.get_map(handle)?;
        map.keys()
            .map(|k| match k {
                MapKey::Scalar(s) => Value::Scalar(*s),
                MapKey::Str(id) => Value::Str(*id),
            })
            .collect()
    };
    // shallow_copy_value is a no-op for Scalar/Str (no GcHandle), but call for correctness
    for k in &keys {
        itp.environment.heap.shallow_copy_value(*k);
    }
    Ok(itp.environment.insert_array_variable(keys))
}

fn map_values_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    check_exact_args(map_values_fn, &args, 1)?;
    let handle = get_map_arg(map_values_fn, args[0])?;
    let values: Vec<Value> = {
        let map = itp.environment.get_map(handle)?;
        map.values().copied().collect()
    };
    for v in &values {
        itp.environment.heap.shallow_copy_value(*v);
    }
    Ok(itp.environment.insert_array_variable(values))
}

fn map_insert_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    check_exact_args(map_insert_fn, &args, 3)?;
    let handle = get_map_arg(map_insert_fn, args[0])?;
    let key = MapKey::convert_from_value(args[1])?;
    let new_value = args[2];
    // increment ref count before taking the mutable heap reference
    itp.environment.heap.shallow_copy_value(new_value);
    let Some(GcObject::Map(map)) = itp.environment.heap.get_object_mut(handle) else {
        return Err(Error::GcObjectNotFound(handle));
    };
    // ownership of the old value transfers to the caller; ref count stays at 1
    Ok(map.insert(key, new_value).unwrap_or(Value::Unit))
}

fn map_remove_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    check_exact_args(map_remove_fn, &args, 2)?;
    let handle = get_map_arg(map_remove_fn, args[0])?;
    let key = MapKey::convert_from_value(args[1])?;
    let Some(GcObject::Map(map)) = itp.environment.heap.get_object_mut(handle) else {
        return Err(Error::GcObjectNotFound(handle));
    };
    // ownership of the removed value transfers to the caller; ref count stays at 1
    Ok(map.remove(&key).unwrap_or(Value::Unit))
}

// Argument validation helpers

fn check_exact_args(func: BuiltinFn, args: &[Value], expected: usize) -> Result<(), Error> {
    if args.len() != expected {
        return Err(Error::WrongNumberOfArgument(
            Value::BuiltinFunction(func),
            expected,
            args.len(),
        ));
    }
    Ok(())
}

fn check_min_args(func: BuiltinFn, args: &[Value], min: usize) -> Result<(), Error> {
    if args.len() < min {
        return Err(Error::WrongNumberOfArgumentAtLeast(
            Value::BuiltinFunction(func),
            min,
            args.len(),
        ));
    }
    Ok(())
}

fn get_array_arg(func: BuiltinFn, arg: Value) -> Result<GcHandle, Error> {
    match arg {
        Value::Array(handle) => Ok(handle),
        _ => Err(Error::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "Array",
        )),
    }
}

fn get_map_arg(func: BuiltinFn, arg: Value) -> Result<GcHandle, Error> {
    match arg {
        Value::Map(handle) => Ok(handle),
        _ => Err(Error::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "Map",
        )),
    }
}
