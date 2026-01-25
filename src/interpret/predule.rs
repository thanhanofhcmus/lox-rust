use std::collections::HashMap;

use crate::{
    id::Id,
    interpret::{error::Error, helper_values::SerialValue, interpreter, value::Value},
};

pub fn create() -> HashMap<Id, Value> {
    let mut preludes = HashMap::new();

    preludes.insert(Id::new("print"), Value::BuiltinFunction(print_fn));
    preludes.insert(Id::new("assert"), Value::BuiltinFunction(assert_fn));
    preludes.insert(Id::new("from_json"), Value::BuiltinFunction(from_json_fn));
    preludes.insert(Id::new("to_json"), Value::BuiltinFunction(to_json_fn));

    preludes.insert(Id::new("_dbg_print"), Value::BuiltinFunction(dbg_print_fn));
    preludes.insert(Id::new("_dbg_state"), Value::BuiltinFunction(dbg_state_fn));

    preludes
}

fn dbg_state_fn(itp: &mut interpreter::Interpreter, _: Vec<Value>) -> Result<Value, Error> {
    let env = &itp.environment;
    dbg!(&env);
    Ok(Value::Nil)
}

fn dbg_print_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    let mut print_writer = itp.environment.get_print_writer();

    // TODO: handle write! error
    for value in args {
        write!(print_writer, "{:?}", value).unwrap();
    }
    writeln!(print_writer).unwrap();

    Ok(Value::Nil)
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

    Ok(Value::Nil)
}

fn assert_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    if args.len() != 2 {
        return Err(Error::WrongNumberOfArgument("assert".into(), 2, args.len()));
    }

    let mut args_iter = args.into_iter();
    let condition = args_iter.next().unwrap();
    let message_val = args_iter.next().unwrap();

    match condition {
        Value::Bool(true) => Ok(Value::Nil), // Assertion passed
        Value::Bool(false) => print_fn(itp, vec![message_val]),
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
        return Ok(Value::Nil);
    };
    let value = *value;
    let Value::Str(handle) = value else {
        // TODO: maybe throw error here
        return Ok(Value::Nil);
    };
    let s = itp.environment.get_string(handle)?;
    let serial_value = serde_json::from_str::<SerialValue>(s)
        .map_err(|e| Error::DeserializeFailed(value, e.to_string()))?;

    let value = serial_value.hydrate(itp.environment)?;

    Ok(value)
}

fn to_json_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    if args.is_empty() {
        return Ok(Value::Nil);
    }
    let value = args[0];

    let serial_value = SerialValue::convert_from_value(value.to_owned(), itp.environment)?;

    // maybe also throw error here
    let is_print_pretty = args
        .get(1)
        .map(|v| if let Value::Bool(b) = v { *b } else { false })
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
