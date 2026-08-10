use super::helpers::check_exact_args;
use crate::{
    interpret::{BorrowContext, BuiltinFn, InterpretError, Number, Value},
    types::Type,
};

// ---- std:math constants -----------------------------------------------

// pi, tau, e are registered as constants in collect_std_fns! below.

// ---- math helpers -----------------------------------------------------

fn get_number_arg(func: BuiltinFn, arg: Value) -> Result<Number, InterpretError> {
    arg.get_number().ok_or(InterpretError::WrongArgumentType(
        Value::BuiltinFunction(func),
        arg,
        "number",
    ))
}

macro_rules! impl_unary_math {
    ($name:ident, $lname:literal, $call:expr) => {
        crate::std_fn! {
            "std:math", $lname, Type::FUNCTION_NUMBER_TO_NUMBER,
            fn $name(_ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
                check_exact_args($name, &args, 1)?;
                let x = get_number_arg($name, args[0])?.to_f64();
                let result: f64 = $call(x);
                Ok(Value::make_number(Number::Floating(result)))
            }
        }
    };
}

macro_rules! impl_binary_math {
    ($name:ident, $lname:literal, $call:expr) => {
        crate::std_fn! {
            "std:math", $lname, Type::FUNCTION_NUMBER_NUMBER_TO_NUMBER,
            fn $name(_ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
                check_exact_args($name, &args, 2)?;
                let x = get_number_arg($name, args[0])?.to_f64();
                let y = get_number_arg($name, args[1])?.to_f64();
                let result: f64 = $call(x, y);
                Ok(Value::make_number(Number::Floating(result)))
            }
        }
    };
}

// `abs`, `floor`, `ceil`, `trunc`, `round`, `min` and `max` are exact on
// integers. Routing them through `f64` would lose precision above 2^53 and
// change the reported type of e.g. `math::abs(5)`, so integer inputs keep
// their integer representation. Everything else is inherently floating point.

macro_rules! impl_unary_math_int_preserving {
    ($name:ident, $lname:literal, $int_call:expr, $float_call:expr) => {
        crate::std_fn! {
            "std:math", $lname, Type::FUNCTION_NUMBER_TO_NUMBER,
            fn $name(_ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
                check_exact_args($name, &args, 1)?;
                let result = match get_number_arg($name, args[0])? {
                    Number::Integer(i) => $int_call(i),
                    Number::Floating(f) => Number::Floating($float_call(f)),
                };
                Ok(Value::make_number(result))
            }
        }
    };
}

macro_rules! impl_binary_math_int_preserving {
    ($name:ident, $lname:literal, $int_call:expr, $float_call:expr) => {
        crate::std_fn! {
            "std:math", $lname, Type::FUNCTION_NUMBER_NUMBER_TO_NUMBER,
            fn $name(_ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
                check_exact_args($name, &args, 2)?;
                let x = get_number_arg($name, args[0])?;
                let y = get_number_arg($name, args[1])?;
                let result = match (x, y) {
                    (Number::Integer(a), Number::Integer(b)) => Number::Integer($int_call(a, b)),
                    _ => Number::Floating($float_call(x.to_f64(), y.to_f64())),
                };
                Ok(Value::make_number(result))
            }
        }
    };
}

/// Rounding an integer is the identity.
fn int_identity(i: i64) -> Number {
    Number::Integer(i)
}

// ---- unary implementations --------------------------------------------

impl_unary_math!(math_sqrt_fn, "sqrt", |x: f64| x.sqrt());
impl_unary_math!(math_cbrt_fn, "cbrt", |x: f64| x.cbrt());
impl_unary_math!(math_exp_fn, "exp", |x: f64| x.exp());
impl_unary_math!(math_exp2_fn, "exp2", |x: f64| x.exp2());
impl_unary_math!(math_ln_fn, "ln", |x: f64| x.ln());
impl_unary_math!(math_log2_fn, "log2", |x: f64| x.log2());
impl_unary_math!(math_log10_fn, "log10", |x: f64| x.log10());
impl_unary_math!(math_sin_fn, "sin", |x: f64| x.sin());
impl_unary_math!(math_cos_fn, "cos", |x: f64| x.cos());
impl_unary_math!(math_tan_fn, "tan", |x: f64| x.tan());
impl_unary_math!(math_asin_fn, "asin", |x: f64| x.asin());
impl_unary_math!(math_acos_fn, "acos", |x: f64| x.acos());
impl_unary_math!(math_atan_fn, "atan", |x: f64| x.atan());
impl_unary_math!(math_sinh_fn, "sinh", |x: f64| x.sinh());
impl_unary_math!(math_cosh_fn, "cosh", |x: f64| x.cosh());
impl_unary_math!(math_tanh_fn, "tanh", |x: f64| x.tanh());
impl_unary_math!(math_degrees_fn, "degrees", |x: f64| x.to_degrees());
impl_unary_math!(math_radians_fn, "radians", |x: f64| x.to_radians());

// ---- integer-preserving unary implementations -------------------------

// `i64::MIN.abs()` overflows; fall back to floating point for that one value.
impl_unary_math_int_preserving!(
    math_abs_fn,
    "abs",
    |i: i64| match i.checked_abs() {
        Some(v) => Number::Integer(v),
        None => Number::Floating((i as f64).abs()),
    },
    |x: f64| x.abs()
);
impl_unary_math_int_preserving!(math_ceil_fn, "ceil", int_identity, |x: f64| x.ceil());
impl_unary_math_int_preserving!(math_floor_fn, "floor", int_identity, |x: f64| x.floor());
impl_unary_math_int_preserving!(math_trunc_fn, "trunc", int_identity, |x: f64| x.trunc());
// `f64::round` is ties-away-from-zero, matching the documented intent. The
// hand-rolled `(x + 0.5.copysign(x)).trunc()` was not equivalent: for
// `0.49999999999999994` the addition itself rounds up to `1.0`, yielding `1`.
impl_unary_math_int_preserving!(math_round_fn, "round", int_identity, |x: f64| x.round());

// ---- binary implementations -------------------------------------------

impl_binary_math!(math_pow_fn, "pow", |x: f64, y: f64| x.powf(y));
impl_binary_math!(math_atan2_fn, "atan2", |x: f64, y: f64| x.atan2(y));
impl_binary_math!(math_hypot_fn, "hypot", |x: f64, y: f64| x.hypot(y));

impl_binary_math_int_preserving!(math_min_fn, "min", |a: i64, b: i64| a.min(b), |x: f64, y: f64| x.min(y));
impl_binary_math_int_preserving!(math_max_fn, "max", |a: i64, b: i64| a.max(b), |x: f64, y: f64| x.max(y));

// ---- module registration ----------------------------------------------

crate::collect_std_fns! {
    "std:math",
    functions: [
        math_abs_fn,
        math_sqrt_fn,
        math_cbrt_fn,
        math_exp_fn,
        math_exp2_fn,
        math_ln_fn,
        math_log2_fn,
        math_log10_fn,
        math_sin_fn,
        math_cos_fn,
        math_tan_fn,
        math_asin_fn,
        math_acos_fn,
        math_atan_fn,
        math_sinh_fn,
        math_cosh_fn,
        math_tanh_fn,
        math_ceil_fn,
        math_floor_fn,
        math_round_fn,
        math_trunc_fn,
        math_degrees_fn,
        math_radians_fn,
        math_pow_fn,
        math_atan2_fn,
        math_hypot_fn,
        math_min_fn,
        math_max_fn,
    ],
    constants: [
        ("pi", Type::Number, Value::make_number(Number::Floating(std::f64::consts::PI))),
        ("tau", Type::Number, Value::make_number(Number::Floating(std::f64::consts::TAU))),
        ("e", Type::Number, Value::make_number(Number::Floating(std::f64::consts::E))),
    ]
}
