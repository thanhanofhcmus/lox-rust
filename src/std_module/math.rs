use std::collections::HashMap;

use super::helpers::check_exact_args;
use crate::{
    id::Id,
    interpret::{self, BorrowContext, BuiltinFn, InterpretError, Number, Value},
    module::{ModuleIdentity, ModuleStringInterner},
    typecheck,
    types::{Type, TypeId, TypeInterner},
};

// ---- std:math ----------------------------------------------------------

pub(crate) fn math_typecheck_module(
    msi: &mut ModuleStringInterner,
    type_interner: &mut TypeInterner,
) -> (ModuleIdentity, typecheck::Module) {
    let identity = math_identity(msi);

    let mut symbol_scope = crate::types::TypeScope::new();

    // Constants
    symbol_scope.associate(Id::new("pi"), TypeId::NUMBER);
    symbol_scope.associate(Id::new("tau"), TypeId::NUMBER);
    symbol_scope.associate(Id::new("e"), TypeId::NUMBER);

    // (number) -> number
    let unary_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::NUMBER],
        variadic: None,
        return_: TypeId::NUMBER,
    });

    // (number, number) -> number
    let binary_type = type_interner.intern_type(&Type::Function {
        params: vec![TypeId::NUMBER, TypeId::NUMBER],
        variadic: None,
        return_: TypeId::NUMBER,
    });

    // Unary functions
    let unary_names = [
        "abs", "sqrt", "cbrt", "exp", "exp2", "ln", "log2", "log10", "sin", "cos", "tan", "asin", "acos", "atan",
        "sinh", "cosh", "tanh", "ceil", "floor", "round", "trunc", "degrees", "radians",
    ];
    for name in &unary_names {
        symbol_scope.associate(Id::new(name), unary_type.0);
    }

    // Binary functions
    let binary_names = ["pow", "atan2", "hypot", "min", "max"];
    for name in &binary_names {
        symbol_scope.associate(Id::new(name), binary_type.0);
    }

    let module = typecheck::Module {
        symbol_scope,
        struct_scope: crate::types::TypeScope::new(),
    };

    (identity, module)
}

pub(crate) fn math_interpret_module(msi: &mut ModuleStringInterner) -> (ModuleIdentity, interpret::Module) {
    let identity = math_identity(msi);

    let mut variables = HashMap::new();

    // Constants
    variables.insert(
        Id::new("pi"),
        Value::make_number(Number::Floating(std::f64::consts::PI)),
    );
    variables.insert(
        Id::new("tau"),
        Value::make_number(Number::Floating(std::f64::consts::TAU)),
    );
    variables.insert(Id::new("e"), Value::make_number(Number::Floating(std::f64::consts::E)));

    // Unary functions
    variables.insert(Id::new("abs"), Value::BuiltinFunction(math_abs_fn));
    variables.insert(Id::new("sqrt"), Value::BuiltinFunction(math_sqrt_fn));
    variables.insert(Id::new("cbrt"), Value::BuiltinFunction(math_cbrt_fn));
    variables.insert(Id::new("exp"), Value::BuiltinFunction(math_exp_fn));
    variables.insert(Id::new("exp2"), Value::BuiltinFunction(math_exp2_fn));
    variables.insert(Id::new("ln"), Value::BuiltinFunction(math_ln_fn));
    variables.insert(Id::new("log2"), Value::BuiltinFunction(math_log2_fn));
    variables.insert(Id::new("log10"), Value::BuiltinFunction(math_log10_fn));
    variables.insert(Id::new("sin"), Value::BuiltinFunction(math_sin_fn));
    variables.insert(Id::new("cos"), Value::BuiltinFunction(math_cos_fn));
    variables.insert(Id::new("tan"), Value::BuiltinFunction(math_tan_fn));
    variables.insert(Id::new("asin"), Value::BuiltinFunction(math_asin_fn));
    variables.insert(Id::new("acos"), Value::BuiltinFunction(math_acos_fn));
    variables.insert(Id::new("atan"), Value::BuiltinFunction(math_atan_fn));
    variables.insert(Id::new("sinh"), Value::BuiltinFunction(math_sinh_fn));
    variables.insert(Id::new("cosh"), Value::BuiltinFunction(math_cosh_fn));
    variables.insert(Id::new("tanh"), Value::BuiltinFunction(math_tanh_fn));
    variables.insert(Id::new("ceil"), Value::BuiltinFunction(math_ceil_fn));
    variables.insert(Id::new("floor"), Value::BuiltinFunction(math_floor_fn));
    variables.insert(Id::new("round"), Value::BuiltinFunction(math_round_fn));
    variables.insert(Id::new("trunc"), Value::BuiltinFunction(math_trunc_fn));
    variables.insert(Id::new("degrees"), Value::BuiltinFunction(math_degrees_fn));
    variables.insert(Id::new("radians"), Value::BuiltinFunction(math_radians_fn));

    // Binary functions
    variables.insert(Id::new("pow"), Value::BuiltinFunction(math_pow_fn));
    variables.insert(Id::new("atan2"), Value::BuiltinFunction(math_atan2_fn));
    variables.insert(Id::new("hypot"), Value::BuiltinFunction(math_hypot_fn));
    variables.insert(Id::new("min"), Value::BuiltinFunction(math_min_fn));
    variables.insert(Id::new("max"), Value::BuiltinFunction(math_max_fn));

    let module = interpret::Module::new(variables);

    (identity, module)
}

// ---- math identity ----------------------------------------------------

fn math_identity(msi: &mut ModuleStringInterner) -> ModuleIdentity {
    ModuleIdentity {
        resolved_path: msi.intern("std:math"),
        is_std: true,
    }
}

// ---- math helpers -----------------------------------------------------

fn get_number_arg(func: BuiltinFn, arg: Value) -> Result<Number, InterpretError> {
    arg.get_number().ok_or(InterpretError::WrongArgumentType(
        Value::BuiltinFunction(func),
        arg,
        "number",
    ))
}

macro_rules! impl_unary_math {
    ($name:ident, $call:expr) => {
        fn $name(_ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
            check_exact_args($name, &args, 1)?;
            let x = get_number_arg($name, args[0])?.to_f64();
            let result: f64 = ({
                let f: fn(f64) -> f64 = $call;
                f
            })(x);
            Ok(Value::make_number(Number::Floating(result)))
        }
    };
}

macro_rules! impl_binary_math {
    ($name:ident, $call:expr) => {
        fn $name(_ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
            check_exact_args($name, &args, 2)?;
            let x = get_number_arg($name, args[0])?.to_f64();
            let y = get_number_arg($name, args[1])?.to_f64();
            let result: f64 = ({
                let f: fn(f64, f64) -> f64 = $call;
                f
            })(x, y);
            Ok(Value::make_number(Number::Floating(result)))
        }
    };
}

// ---- unary implementations --------------------------------------------

impl_unary_math!(math_abs_fn, |x| x.abs());
impl_unary_math!(math_sqrt_fn, |x| x.sqrt());
impl_unary_math!(math_cbrt_fn, |x| x.cbrt());
impl_unary_math!(math_exp_fn, |x| x.exp());
impl_unary_math!(math_exp2_fn, |x| x.exp2());
impl_unary_math!(math_ln_fn, |x| x.ln());
impl_unary_math!(math_log2_fn, |x| x.log2());
impl_unary_math!(math_log10_fn, |x| x.log10());
impl_unary_math!(math_sin_fn, |x| x.sin());
impl_unary_math!(math_cos_fn, |x| x.cos());
impl_unary_math!(math_tan_fn, |x| x.tan());
impl_unary_math!(math_asin_fn, |x| x.asin());
impl_unary_math!(math_acos_fn, |x| x.acos());
impl_unary_math!(math_atan_fn, |x| x.atan());
impl_unary_math!(math_sinh_fn, |x| x.sinh());
impl_unary_math!(math_cosh_fn, |x| x.cosh());
impl_unary_math!(math_tanh_fn, |x| x.tanh());
impl_unary_math!(math_ceil_fn, |x| x.ceil());
impl_unary_math!(math_floor_fn, |x| x.floor());
impl_unary_math!(math_trunc_fn, |x| x.trunc());
impl_unary_math!(math_degrees_fn, |x| x.to_degrees());
impl_unary_math!(math_radians_fn, |x| x.to_radians());

// round: custom — rounds to nearest integer, ties away from zero
fn math_round_fn(_ctx: &mut BorrowContext, args: Vec<Value>) -> Result<Value, InterpretError> {
    check_exact_args(math_round_fn, &args, 1)?;
    let x = get_number_arg(math_round_fn, args[0])?.to_f64();
    // f64::round uses ties-to-even (banker's rounding).
    // For a scripting language, ties-away-from-zero is more intuitive.
    let result = (x + 0.5_f64.copysign(x)).trunc();
    Ok(Value::make_number(Number::Floating(result)))
}

// ---- binary implementations -------------------------------------------

impl_binary_math!(math_pow_fn, |x, y| x.powf(y));
impl_binary_math!(math_atan2_fn, |x, y| x.atan2(y));
impl_binary_math!(math_hypot_fn, |x, y| x.hypot(y));
impl_binary_math!(math_min_fn, |x, y| x.min(y));
impl_binary_math!(math_max_fn, |x, y| x.max(y));
