use crate::interpret::values::DisplayWriter;
use std::collections::HashMap;

use crate::{
    id::Id,
    interpret::{
        error::Error,
        interpreter,
        values::{SerialValue, Value},
    },
};

pub fn create() -> HashMap<Id, Value> {
    let mut preludes = HashMap::new();

    preludes.insert(Id::new("print"), Value::BuiltinFunction(print_fn));
    preludes.insert(Id::new("assert"), Value::BuiltinFunction(assert_fn));
    preludes.insert(Id::new("from_json"), Value::BuiltinFunction(from_json_fn));
    preludes.insert(Id::new("to_json"), Value::BuiltinFunction(to_json_fn));

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
        if let Value::Str(handle) = value {
            let s = itp.environment.get_string(handle)?;
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
