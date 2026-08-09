use std::collections::HashMap;

use super::helpers::{check_exact_args, check_min_args, get_bool_arg};
use crate::{
    id::Id,
    interpret::{self, BorrowContext, InterpretError, SerialValue, Value},
    module::{ModuleIdentity, ModuleStringInterner},
    typecheck,
    types::{Type, TypeInterner},
};

// ---- std:json ----------------------------------------------------------

pub(crate) fn json_typecheck_module(
    msi: &mut ModuleStringInterner,
    type_interner: &mut TypeInterner,
) -> (ModuleIdentity, typecheck::Module) {
    let identity = json_identity(msi);
    let mut symbol_scope = crate::types::TypeScope::new();

    let parse_type = type_interner.intern_type(&Type::FUNCTION_STR_TO_ANY);
    symbol_scope.associate(Id::new("parse"), parse_type.0);

    let stringify_type = type_interner.intern_type(&Type::FUNCTION_ANY_VARIADIC_BOOL_TO_STR);
    symbol_scope.associate(Id::new("stringify"), stringify_type.0);

    let module = typecheck::Module {
        symbol_scope,
        struct_scope: crate::types::TypeScope::new(),
    };

    (identity, module)
}

pub(crate) fn json_interpret_module(msi: &mut ModuleStringInterner) -> (ModuleIdentity, interpret::Module) {
    let identity = json_identity(msi);

    let mut variables = HashMap::new();
    variables.insert(Id::new("parse"), Value::BuiltinFunction(json_parse_fn));
    variables.insert(Id::new("stringify"), Value::BuiltinFunction(json_stringify_fn));

    let module = interpret::Module::new(variables);

    (identity, module)
}

// ---- json function implementations ------------------------------------

fn json_identity(msi: &mut ModuleStringInterner) -> ModuleIdentity {
    ModuleIdentity {
        resolved_path: msi.intern("std:json"),
        is_std: true,
    }
}

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
