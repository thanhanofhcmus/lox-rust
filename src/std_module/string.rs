use super::helpers::{check_exact_args, check_min_args, get_array_arg};
use crate::{
    interpret::{BorrowContext, BuiltinFn, InterpretError, Number, Value},
    types::Type,
};

// ---- std:string helpers -----------------------------------------------

/// Upper bound on the size of a string a std:string function may build.
/// Keeps `repeat` from aborting the process on an allocation overflow.
const MAX_STRING_BYTES: usize = 1 << 26; // 64 MiB

/// True when `s` is non-empty and *every* char satisfies `pred`.
/// The empty string is never a member of these character classes.
fn all_chars_predicate(
    ctx: &BorrowContext,
    func: BuiltinFn,
    arg: Value,
    pred: fn(char) -> bool,
) -> Result<Value, InterpretError> {
    let s = ctx.get_str_value(func, arg)?;
    let result = !s.is_empty() && s.chars().all(pred);
    Ok(Value::make_bool(result))
}

// ---- std:string functions ---------------------------------------------

crate::std_fn! {
    "std:string", "length", Type::FUNCTION_STR_TO_NUMBER,
    fn string_length_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_length_fn, &args, 1)?;
        let s = ctx.get_str_value(string_length_fn, args[0])?;
        Ok(Value::make_number(Number::Integer(s.chars().count() as i64)))
    }
}

crate::std_fn! {
    "std:string", "starts_with", Type::FUNCTION_STR_STR_TO_BOOL,
    fn string_starts_with_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_starts_with_fn, &args, 2)?;
        let s = ctx.get_str_value(string_starts_with_fn, args[0])?.to_owned();
        let prefix = ctx.get_str_value(string_starts_with_fn, args[1])?;
        Ok(Value::make_bool(s.starts_with(prefix)))
    }
}

crate::std_fn! {
    "std:string", "ends_with", Type::FUNCTION_STR_STR_TO_BOOL,
    fn string_ends_with_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_ends_with_fn, &args, 2)?;
        let s = ctx.get_str_value(string_ends_with_fn, args[0])?.to_owned();
        let suffix = ctx.get_str_value(string_ends_with_fn, args[1])?;
        Ok(Value::make_bool(s.ends_with(suffix)))
    }
}

crate::std_fn! {
    "std:string", "contains", Type::FUNCTION_STR_STR_TO_BOOL,
    fn string_contains_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_contains_fn, &args, 2)?;
        let s = ctx.get_str_value(string_contains_fn, args[0])?.to_owned();
        let needle = ctx.get_str_value(string_contains_fn, args[1])?;
        Ok(Value::make_bool(s.contains(needle)))
    }
}

crate::std_fn! {
    "std:string", "index_of", Type::FUNCTION_STR_STR_TO_NUMBER,
    fn string_index_of_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_index_of_fn, &args, 2)?;
        let s = ctx.get_str_value(string_index_of_fn, args[0])?.to_owned();
        let needle = ctx.get_str_value(string_index_of_fn, args[1])?;
        let byte_idx = s.find(needle);
        let result = byte_idx.map(|bi| s[..bi].chars().count() as i64).unwrap_or(-1);
        Ok(Value::make_number(Number::Integer(result)))
    }
}

crate::std_fn! {
    "std:string", "last_index_of", Type::FUNCTION_STR_STR_TO_NUMBER,
    fn string_last_index_of_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_last_index_of_fn, &args, 2)?;
        let s = ctx.get_str_value(string_last_index_of_fn, args[0])?.to_owned();
        let needle = ctx.get_str_value(string_last_index_of_fn, args[1])?;
        let byte_idx = s.rfind(needle);
        let result = byte_idx.map(|bi| s[..bi].chars().count() as i64).unwrap_or(-1);
        Ok(Value::make_number(Number::Integer(result)))
    }
}

crate::std_fn! {
    "std:string", "replace", Type::FUNCTION_STR_STR_STR_TO_STR,
    fn string_replace_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_replace_fn, &args, 3)?;
        let s = ctx.get_str_value(string_replace_fn, args[0])?.to_owned();
        let from = ctx.get_str_value(string_replace_fn, args[1])?;
        let to = ctx.get_str_value(string_replace_fn, args[2])?;
        Ok(ctx.environment.insert_string_variable(s.replace(from, to)))
    }
}

crate::std_fn! {
    "std:string", "split", Type::FUNCTION_STR_STR_TO_ANY,
    fn string_split_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_split_fn, &args, 2)?;
        let s = ctx.get_str_value(string_split_fn, args[0])?.to_owned();
        let delim = ctx.get_str_value(string_split_fn, args[1])?.to_owned();
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
}

crate::std_fn! {
    "std:string", "join", Type::FUNCTION_ANY_STR_TO_STR,
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
}

crate::std_fn! {
    "std:string", "trim", Type::FUNCTION_STR_TO_STR,
    fn string_trim_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_trim_fn, &args, 1)?;
        let s = ctx.get_str_value(string_trim_fn, args[0])?;
        Ok(ctx.environment.insert_string_variable(s.trim().to_owned()))
    }
}

crate::std_fn! {
    "std:string", "trim_start", Type::FUNCTION_STR_TO_STR,
    fn string_trim_start_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_trim_start_fn, &args, 1)?;
        let s = ctx.get_str_value(string_trim_start_fn, args[0])?;
        Ok(ctx.environment.insert_string_variable(s.trim_start().to_owned()))
    }
}

crate::std_fn! {
    "std:string", "trim_end", Type::FUNCTION_STR_TO_STR,
    fn string_trim_end_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_trim_end_fn, &args, 1)?;
        let s = ctx.get_str_value(string_trim_end_fn, args[0])?;
        Ok(ctx.environment.insert_string_variable(s.trim_end().to_owned()))
    }
}

crate::std_fn! {
    "std:string", "to_lower", Type::FUNCTION_STR_TO_STR,
    fn string_to_lower_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_to_lower_fn, &args, 1)?;
        let s = ctx.get_str_value(string_to_lower_fn, args[0])?;
        Ok(ctx.environment.insert_string_variable(s.to_lowercase()))
    }
}

crate::std_fn! {
    "std:string", "to_upper", Type::FUNCTION_STR_TO_STR,
    fn string_to_upper_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_to_upper_fn, &args, 1)?;
        let s = ctx.get_str_value(string_to_upper_fn, args[0])?;
        Ok(ctx.environment.insert_string_variable(s.to_uppercase()))
    }
}

crate::std_fn! {
    "std:string", "repeat", Type::FUNCTION_STR_NUMBER_TO_STR,
    fn string_repeat_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_repeat_fn, &args, 2)?;
        let s = ctx.get_str_value(string_repeat_fn, args[0])?.to_owned();
        let n = args[1].to_index()?;
        // `str::repeat` aborts the process on capacity overflow, so reject
        // oversized results up front.
        let requested = s.len().saturating_mul(n);
        if requested > MAX_STRING_BYTES {
            return Err(InterpretError::StringLengthLimitExceeded {
                requested,
                limit: MAX_STRING_BYTES,
            });
        }
        Ok(ctx.environment.insert_string_variable(s.repeat(n)))
    }
}

crate::std_fn! {
    "std:string", "substring", Type::FUNCTION_STR_NUMBER_VARIADIC_NUMBER_TO_STR,
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
        // `start` and `end` are independently clamped against the length, so an
        // inverted or out-of-range pair must still produce an empty string
        // rather than an out-of-bounds slice.
        if start >= chars.len() || end <= start {
            return Ok(ctx.environment.insert_string_variable(String::new()));
        }
        let result: String = chars[start..end].iter().collect();
        Ok(ctx.environment.insert_string_variable(result))
    }
}

crate::std_fn! {
    "std:string", "is_alpha", Type::FUNCTION_STR_TO_BOOL,
    fn string_is_alpha_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_is_alpha_fn, &args, 1)?;
        all_chars_predicate(ctx, string_is_alpha_fn, args[0], |c| c.is_alphabetic())
    }
}

crate::std_fn! {
    "std:string", "is_number", Type::FUNCTION_STR_TO_BOOL,
    fn string_is_number_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_is_number_fn, &args, 1)?;
        all_chars_predicate(ctx, string_is_number_fn, args[0], |c| c.is_ascii_digit())
    }
}

crate::std_fn! {
    "std:string", "is_alphanumeric", Type::FUNCTION_STR_TO_BOOL,
    fn string_is_alphanumeric_fn(ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
        check_exact_args(string_is_alphanumeric_fn, &args, 1)?;
        all_chars_predicate(ctx, string_is_alphanumeric_fn, args[0], |c| c.is_alphanumeric())
    }
}

crate::collect_std_fns! {
    "std:string",
    functions: [
        string_length_fn,
        string_starts_with_fn,
        string_ends_with_fn,
        string_contains_fn,
        string_index_of_fn,
        string_last_index_of_fn,
        string_replace_fn,
        string_split_fn,
        string_join_fn,
        string_trim_fn,
        string_trim_start_fn,
        string_trim_end_fn,
        string_to_lower_fn,
        string_to_upper_fn,
        string_repeat_fn,
        string_substring_fn,
        string_is_alpha_fn,
        string_is_number_fn,
        string_is_alphanumeric_fn,
    ],
    constants: []
}
