use std::collections::HashMap;

use crate::{
    id::Id,
    interpret::{error::Error, interpreter, value::Value},
};

pub fn create() -> HashMap<Id, Value> {
    let mut preludes = HashMap::new();

    preludes.insert(Id::new("print"), Value::BuiltinFunction(print_fn));

    preludes
}

fn print_fn(itp: &mut interpreter::Interpreter, args: Vec<Value>) -> Result<Value, Error> {
    let mut print_writer = itp.environment.get_print_writer();

    // TODO: handle error in write!
    for value in args {
        write!(print_writer, "{}", value).unwrap();
    }
    writeln!(print_writer).unwrap();

    Ok(Value::Nil)
}
