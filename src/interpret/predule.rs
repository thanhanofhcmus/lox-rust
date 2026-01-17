use std::collections::HashMap;

use crate::{
    id::Id,
    interpret::{error::Error, interpreter, value::Value},
};

pub fn create() -> HashMap<Id, Value> {
    let mut preludes = HashMap::new();

    preludes.insert(Id::new("print"), Value::BuiltinFunction(print_fn));
    preludes.insert(Id::new("print_raw"), Value::BuiltinFunction(print_raw_fn));
    preludes.insert(Id::new("from_json"), Value::BuiltinFunction(from_json_fn));
    preludes.insert(Id::new("to_json"), Value::BuiltinFunction(to_json_fn));

    preludes
}

fn print_logic(
    itp: &mut interpreter::Interpreter,
    args: Vec<Value>,
    raw: bool,
) -> Result<Value, Error> {
    let mut print_writer = itp.environment.get_print_writer();

    // TODO: handle write! error
    for value in args {
        if raw {
            if let Value::Str(s) = &value {
                write!(print_writer, "{}", s).unwrap();
            } else {
                write!(print_writer, "{}", value).unwrap();
            }
        } else {
            write!(print_writer, "{}", value).unwrap();
        }
    }
    writeln!(print_writer).unwrap();

    Ok(Value::Nil)
}

fn print_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    print_logic(itp, args, false)
}

fn print_raw_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    print_logic(itp, args, true)
}

fn from_json_fn(_: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    let Some(value) = args.first() else {
        // TODO: maybe throw error here
        return Ok(Value::Nil);
    };
    let Value::Str(s) = value else {
        // TODO: maybe throw error here
        return Ok(Value::Nil);
    };
    serde_json::from_str::<Value>(s)
        .map_err(|e| Error::DeserializeFailed(value.clone(), e.to_string()))
}

fn to_json_fn(_: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    if args.is_empty() {
        return Ok(Value::Nil);
    }
    let value = &args[0];

    // maybe also throw error here
    let is_print_pretty = args
        .get(1)
        .map(|v| if let Value::Bool(b) = v { *b } else { false })
        .unwrap_or(false);

    let result = if is_print_pretty {
        serde_json::to_string_pretty(&value)
    } else {
        serde_json::to_string(value)
    };
    match result {
        Ok(v) => Ok(Value::Str(v)),
        Err(err) => Err(Error::SerializeFailed(value.clone(), err.to_string())),
    }
}
