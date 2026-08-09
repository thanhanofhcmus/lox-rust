use std::collections::HashMap;

use super::helpers::{check_exact_args, check_min_args};
use crate::{
    id::Id,
    interpret::{self, BorrowContext, BuiltinFn, InterpretError, Number, Value},
    module::{ModuleIdentity, ModuleStringInterner},
    typecheck,
    types::{Type, TypeId, TypeInterner},
};

// ---- std:string --------------------------------------------------------

pub(crate) fn string_typecheck_module(
    msi: &mut ModuleStringInterner,
    type_interner: &mut TypeInterner,
) -> (ModuleIdentity, typecheck::Module) {
    let identity = string_identity(msi);

    let mut symbol_scope = crate::types::TypeScope::new();

    // (str) -> number
    let str_to_number = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::STR],
        variadic: None,
        return_: TypeId::NUMBER,
    });
    symbol_scope.associate(Id::new("length"), str_to_number.0);

    // (str) -> str
    let str_to_str = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::STR],
        variadic: None,
        return_: TypeId::STR,
    });
    for name in &["trim", "trim_start", "trim_end", "to_lower", "to_upper"] {
        symbol_scope.associate(Id::new(name), str_to_str.0);
    }

    // (str) -> bool
    let str_to_bool = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::STR],
        variadic: None,
        return_: TypeId::BOOL,
    });
    for name in &["is_alpha", "is_number", "is_alphanumeric"] {
        symbol_scope.associate(Id::new(name), str_to_bool.0);
    }

    // (str, str) -> bool
    let str_str_to_bool = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::STR, TypeId::STR],
        variadic: None,
        return_: TypeId::BOOL,
    });
    for name in &["starts_with", "ends_with", "contains"] {
        symbol_scope.associate(Id::new(name), str_str_to_bool.0);
    }

    // (str, str) -> number
    let str_str_to_number = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::STR, TypeId::STR],
        variadic: None,
        return_: TypeId::NUMBER,
    });
    for name in &["index_of", "last_index_of"] {
        symbol_scope.associate(Id::new(name), str_str_to_number.0);
    }

    // replace: (str, str, str) -> str
    let replace_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::STR, TypeId::STR, TypeId::STR],
        variadic: None,
        return_: TypeId::STR,
    });
    symbol_scope.associate(Id::new("replace"), replace_type.0);

    // split: (str, str) -> any (returns array)
    let split_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::STR, TypeId::STR],
        variadic: None,
        return_: TypeId::ANY,
    });
    symbol_scope.associate(Id::new("split"), split_type.0);

    // join: (any, str) -> str
    let join_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::ANY, TypeId::STR],
        variadic: None,
        return_: TypeId::STR,
    });
    symbol_scope.associate(Id::new("join"), join_type.0);

    // repeat: (str, number) -> str
    let repeat_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::STR, TypeId::NUMBER],
        variadic: None,
        return_: TypeId::STR,
    });
    symbol_scope.associate(Id::new("repeat"), repeat_type.0);

    // substring: (str, number, variadic number?) -> str
    let substring_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::STR, TypeId::NUMBER],
        variadic: Some(TypeId::NUMBER),
        return_: TypeId::STR,
    });
    symbol_scope.associate(Id::new("substring"), substring_type.0);

    let module = typecheck::Module {
        symbol_scope,
        struct_scope: crate::types::TypeScope::new(),
    };

    (identity, module)
}

pub(crate) fn string_interpret_module(msi: &mut ModuleStringInterner) -> (ModuleIdentity, interpret::Module) {
    let identity = string_identity(msi);

    let mut variables = HashMap::new();
    variables.insert(Id::new("length"), Value::BuiltinFunction(string_length_fn));
    variables.insert(Id::new("starts_with"), Value::BuiltinFunction(string_starts_with_fn));
    variables.insert(Id::new("ends_with"), Value::BuiltinFunction(string_ends_with_fn));
    variables.insert(Id::new("contains"), Value::BuiltinFunction(string_contains_fn));
    variables.insert(Id::new("index_of"), Value::BuiltinFunction(string_index_of_fn));
    variables.insert(
        Id::new("last_index_of"),
        Value::BuiltinFunction(string_last_index_of_fn),
    );
    variables.insert(Id::new("replace"), Value::BuiltinFunction(string_replace_fn));
    variables.insert(Id::new("split"), Value::BuiltinFunction(string_split_fn));
    variables.insert(Id::new("join"), Value::BuiltinFunction(string_join_fn));
    variables.insert(Id::new("trim"), Value::BuiltinFunction(string_trim_fn));
    variables.insert(Id::new("trim_start"), Value::BuiltinFunction(string_trim_start_fn));
    variables.insert(Id::new("trim_end"), Value::BuiltinFunction(string_trim_end_fn));
    variables.insert(Id::new("to_lower"), Value::BuiltinFunction(string_to_lower_fn));
    variables.insert(Id::new("to_upper"), Value::BuiltinFunction(string_to_upper_fn));
    variables.insert(Id::new("repeat"), Value::BuiltinFunction(string_repeat_fn));
    variables.insert(Id::new("substring"), Value::BuiltinFunction(string_substring_fn));
    variables.insert(Id::new("is_alpha"), Value::BuiltinFunction(string_is_alpha_fn));
    variables.insert(Id::new("is_number"), Value::BuiltinFunction(string_is_number_fn));
    variables.insert(
        Id::new("is_alphanumeric"),
        Value::BuiltinFunction(string_is_alphanumeric_fn),
    );

    let module = interpret::Module::new(variables);

    (identity, module)
}

// ---- string identity --------------------------------------------------

fn string_identity(msi: &mut ModuleStringInterner) -> ModuleIdentity {
    ModuleIdentity {
        resolved_path: msi.intern("std:string"),
        is_std: true,
    }
}

// ---- string helpers ---------------------------------------------------

use crate::interpret::GcHandle;

fn get_array_arg(func: BuiltinFn, arg: Value) -> Result<GcHandle, InterpretError> {
    match arg {
        Value::Array(handle) => Ok(handle),
        _ => Err(InterpretError::WrongArgumentType(
            Value::BuiltinFunction(func),
            arg,
            "array",
        )),
    }
}

// ---- function implementations -----------------------------------------

fn string_length_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_length_fn, &args, 1)?;
    let s = ctx.get_str_value(string_length_fn, args[0])?;
    Ok(Value::make_number(Number::Integer(s.chars().count() as i64)))
}

fn string_starts_with_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_starts_with_fn, &args, 2)?;
    let s = ctx.get_str_value(string_starts_with_fn, args[0])?.to_owned();
    let prefix = ctx.get_str_value(string_starts_with_fn, args[1])?;
    Ok(Value::make_bool(s.starts_with(prefix)))
}

fn string_ends_with_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_ends_with_fn, &args, 2)?;
    let s = ctx.get_str_value(string_ends_with_fn, args[0])?.to_owned();
    let suffix = ctx.get_str_value(string_ends_with_fn, args[1])?;
    Ok(Value::make_bool(s.ends_with(suffix)))
}

fn string_contains_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_contains_fn, &args, 2)?;
    let s = ctx.get_str_value(string_contains_fn, args[0])?.to_owned();
    let needle = ctx.get_str_value(string_contains_fn, args[1])?;
    Ok(Value::make_bool(s.contains(needle)))
}

fn string_index_of_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_index_of_fn, &args, 2)?;
    let s = ctx.get_str_value(string_index_of_fn, args[0])?.to_owned();
    let needle = ctx.get_str_value(string_index_of_fn, args[1])?;
    // Return char index (not byte offset)
    let byte_idx = s.find(needle);
    let result = byte_idx.map(|bi| s[..bi].chars().count() as i64).unwrap_or(-1);
    Ok(Value::make_number(Number::Integer(result)))
}

fn string_last_index_of_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_last_index_of_fn, &args, 2)?;
    let s = ctx.get_str_value(string_last_index_of_fn, args[0])?.to_owned();
    let needle = ctx.get_str_value(string_last_index_of_fn, args[1])?;
    let byte_idx = s.rfind(needle);
    let result = byte_idx.map(|bi| s[..bi].chars().count() as i64).unwrap_or(-1);
    Ok(Value::make_number(Number::Integer(result)))
}

fn string_replace_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_replace_fn, &args, 3)?;
    let s = ctx.get_str_value(string_replace_fn, args[0])?.to_owned();
    let from = ctx.get_str_value(string_replace_fn, args[1])?;
    let to = ctx.get_str_value(string_replace_fn, args[2])?;
    Ok(ctx.environment.insert_string_variable(s.replace(from, to)))
}

fn string_split_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_split_fn, &args, 2)?;
    let s = ctx.get_str_value(string_split_fn, args[0])?.to_owned();
    let delim = ctx.get_str_value(string_split_fn, args[1])?.to_owned();
    // Collect owned strings first, then insert them into the heap
    let pieces: Vec<String> = s.split(&delim).map(|p| p.to_owned()).collect();
    let parts: Vec<Value> = pieces
        .into_iter()
        .map(|p| ctx.environment.insert_string_variable(p))
        .collect();
    for v in &parts {
        ctx.environment.heap.shallow_copy_value(*v);
    }
    Ok(ctx.environment.insert_array_variable(parts))
}

fn string_join_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_join_fn, &args, 2)?;
    let handle = get_array_arg(string_join_fn, args[0])?;
    let delim = ctx.get_str_value(string_join_fn, args[1])?;
    let strings: Vec<String> = {
        let arr = ctx.environment.get_array(handle)?;
        arr.iter()
            .map(|v| {
                let id = v.get_str_id().ok_or(InterpretError::WrongArgumentType(
                    Value::BuiltinFunction(string_join_fn),
                    *v,
                    "str",
                ))?;
                ctx.environment.get_string(id).map(|s| s.to_owned())
            })
            .collect::<Result<Vec<_>, _>>()?
    };
    Ok(ctx.environment.insert_string_variable(strings.join(delim)))
}

fn string_trim_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_trim_fn, &args, 1)?;
    let s = ctx.get_str_value(string_trim_fn, args[0])?;
    Ok(ctx.environment.insert_string_variable(s.trim().to_owned()))
}

fn string_trim_start_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_trim_start_fn, &args, 1)?;
    let s = ctx.get_str_value(string_trim_start_fn, args[0])?;
    Ok(ctx.environment.insert_string_variable(s.trim_start().to_owned()))
}

fn string_trim_end_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_trim_end_fn, &args, 1)?;
    let s = ctx.get_str_value(string_trim_end_fn, args[0])?;
    Ok(ctx.environment.insert_string_variable(s.trim_end().to_owned()))
}

fn string_to_lower_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_to_lower_fn, &args, 1)?;
    let s = ctx.get_str_value(string_to_lower_fn, args[0])?;
    Ok(ctx.environment.insert_string_variable(s.to_lowercase()))
}

fn string_to_upper_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_to_upper_fn, &args, 1)?;
    let s = ctx.get_str_value(string_to_upper_fn, args[0])?;
    Ok(ctx.environment.insert_string_variable(s.to_uppercase()))
}

fn string_repeat_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_repeat_fn, &args, 2)?;
    let s = ctx.get_str_value(string_repeat_fn, args[0])?.to_owned();
    let n = args[1].to_index()?;
    Ok(ctx.environment.insert_string_variable(s.repeat(n)))
}

fn string_substring_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_min_args(string_substring_fn, &args, 2)?;
    let s = ctx.get_str_value(string_substring_fn, args[0])?.to_owned();
    let start = args[1].to_index()?;
    let chars: Vec<char> = s.chars().collect();
    let end = if args.len() >= 3 {
        args[2].to_index()?.min(chars.len())
    } else {
        chars.len()
    };
    if start >= chars.len() {
        return Ok(ctx.environment.insert_string_variable(String::new()));
    }
    let result: String = chars[start..end].iter().collect();
    Ok(ctx.environment.insert_string_variable(result))
}

fn first_char_predicate(
    ctx: &BorrowContext,
    func: BuiltinFn,
    arg: Value,
    pred: fn(char) -> bool,
) -> Result<Value, InterpretError> {
    let s = ctx.get_str_value(func, arg)?;
    let result = s.chars().next().is_some_and(pred);
    Ok(Value::make_bool(result))
}

fn string_is_alpha_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_is_alpha_fn, &args, 1)?;
    first_char_predicate(ctx, string_is_alpha_fn, args[0], |c| c.is_alphabetic())
}

fn string_is_number_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_is_number_fn, &args, 1)?;
    first_char_predicate(ctx, string_is_number_fn, args[0], |c| c.is_ascii_digit())
}

fn string_is_alphanumeric_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(string_is_alphanumeric_fn, &args, 1)?;
    first_char_predicate(ctx, string_is_alphanumeric_fn, args[0], |c| c.is_alphanumeric())
}
