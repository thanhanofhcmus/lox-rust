use super::helpers::{check_exact_args, check_min_args, get_bool_arg};
use crate::{
    interpret::{BorrowContext, InterpretError, SerialValue, Value},
    types::Type,
};

// ---- std:json functions -----------------------------------------------

crate::std_fn! {
    "std:json", "parse", Type::FUNCTION_STR_TO_ANY,
    fn json_parse_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(json_parse_fn, &args, 1)?;
        let value = args[0];
        let s = match value {
            Value::Str(str_id) => ctx.environment.get_string(str_id)?.to_string(),
            _ => {
                return Err(InterpretError::WrongArgumentType(
                    Value::BuiltinFunction(json_parse_fn),
                    value,
                    "str",
                ));
            }
        };
        let serial_value =
            serde_json::from_str::<SerialValue>(&s).map_err(|e| InterpretError::DeserializeFailed(value, e.to_string()))?;
        let value = serial_value.hydrate(ctx.environment)?;
        Ok(value)
    }
}

// The trailing `bool` is really a single *optional* "pretty" flag. It is typed
// variadic only because optional parameters do not exist yet, so
// `json::stringify(v, true, false)` typechecks and the extras are ignored.
// Retype this as an optional parameter once the checker supports one.
crate::std_fn! {
    "std:json", "stringify", Type::FUNCTION_ANY_VARIADIC_BOOL_TO_STR,
    fn json_stringify_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_min_args(json_stringify_fn, &args, 1)?;
        let value = args[0];

        let serial_value = SerialValue::convert_from_value(value, ctx.environment, ctx.identifier_registry)?;

        let is_print_pretty = get_bool_arg(
            json_stringify_fn,
            args.get(1).copied().unwrap_or(Value::make_bool(false)),
        )?;
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
}

crate::collect_std_fns! {
    "std:json",
    functions: [json_parse_fn, json_stringify_fn],
    constants: []
}
