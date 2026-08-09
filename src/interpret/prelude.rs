use crate::interpret::{interpreter::BorrowContext, values::DisplayWriter};
use std::collections::HashMap;

use crate::{
    id::Id,
    interpret::{
        debug_string::DebugString,
        error::InterpretError,
        values::{BuiltinFn, Value},
    },
};

pub fn create() -> HashMap<Id, Value> {
    let mut preludes = HashMap::new();

    preludes.insert(Id::new("print"), Value::BuiltinFunction(print_fn));
    preludes.insert(Id::new("assert"), Value::BuiltinFunction(assert_fn));

    preludes.insert(Id::new("_dbg_print"), Value::BuiltinFunction(dbg_print_fn));
    preludes.insert(Id::new("_dbg_state"), Value::BuiltinFunction(dbg_state_fn));
    preludes.insert(Id::new("_dbg_gc_mark"), Value::BuiltinFunction(dbg_gc_mark));
    preludes.insert(Id::new("_dbg_gc_sweep"), Value::BuiltinFunction(dbg_gc_sweep));
    preludes.insert(Id::new("_dbg_gc_mark_sweep"), Value::BuiltinFunction(dbg_gc_mark_sweep));
    preludes.insert(Id::new("_dbg_heap_stats"), Value::BuiltinFunction(dbg_heap_stats));

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
    // TODO: marks all variable in the modules, *all* modules, not just the one imported in this SELF
    ctx.environment.heap.mark(ctx.environment.collect_all_variables());
    Ok(Value::make_nil())
}

fn dbg_gc_sweep(ctx: &mut BorrowContext, _: Vec<Value>) -> Result<Value, InterpretError> {
    ctx.environment.heap.sweep();
    Ok(Value::make_nil())
}

fn dbg_gc_mark_sweep(ctx: &mut BorrowContext, _: Vec<Value>) -> Result<Value, InterpretError> {
    // TODO: marks all variable in the modules, *all* modules, not just the one imported in this SELF
    ctx.environment.heap.mark(ctx.environment.collect_all_variables());
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
            value.write_display(ctx.environment, ctx.identifier_registry, &mut *print_writer)?;
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
                let warn_msg = ctx
                    .environment
                    .insert_string_variable("Assertion check value did not evaluate to a boolean".into());
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
    value.write_display(ctx.environment, ctx.identifier_registry, &mut buf)?;
    Ok(String::from_utf8_lossy(&buf).into_owned())
}

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
