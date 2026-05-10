//! End-to-end integration tests.
//!
//! Each test runs a fixture file through the built `lox-rust` binary with the
//! `--strict-assert` flag. The fixture is a self-checking lox program: a series
//! of `assert(...)` calls. Under strict-assert, a failing assertion raises
//! `InterpretError::AssertionFailed(msg)` and the process exits non-zero,
//! giving us a clean pass/fail signal.

mod common;

use common::{assert_err_contains, assert_ok, run_fixture};

// ---------- positive-path: every feature should run successfully ----------

macro_rules! fixture_tests {
    ($($name:ident: $file:literal),* $(,)?) => {
        $(
            #[test]
            fn $name() {
                assert_ok(&run_fixture($file));
            }
        )*
    };
}

fixture_tests! {
    scalars:              "01_scalars.lox",
    arithmetic:           "02_arithmetic.lox",
    boolean_logic:        "03_boolean_logic.lox",
    comparisons:          "04_comparisons.lox",
    variables_and_types:  "05_variables_and_types.lox",
    strings:              "06_strings.lox",
    blocks:               "07_blocks.lox",
    if_else:              "08_if_else.lox",
    while_loop:           "09_while_loop.lox",
    for_loop:             "10_for_loop.lox",
    when_expr:            "11_when_expr.lox",
    arrays:               "12_arrays.lox",
    maps:                 "13_maps.lox",
    functions:            "14_functions.lox",
    chaining:             "15_chaining.lox",
    json_roundtrip:       "16_json.lox",
    structs:              "17_structs.lox",
    tuples:               "18_tuples.lox",
    underscore:           "19_underscore.lox",
}

// ---------- negative-path: must return an error containing a specific message ----------

macro_rules! error_tests {
    ($($name:ident: $file:literal contains $needle:literal),* $(,)?) => {
        $(
            #[test]
            fn $name() {
                let res = run_fixture($file);
                assert_err_contains(&res, $needle);
            }
        )*
    };
}

error_tests! {
    error_div_by_zero:                      "errors/div_by_zero.lox"                     contains "Division by zero",
    error_array_out_of_bounds:              "errors/array_out_of_bounds.lox"             contains "out of bounds",
    error_assertion_failure_under_strict:   "errors/assertion_failure.lox"               contains "Assertion failed: deliberate",
    error_struct_unknown_field:             "errors/struct_unknown_field.lox"            contains "has no field",
    error_member_access_on_non_struct:      "errors/member_access_on_non_struct.lox"     contains "is not a struct",
    error_member_access_any_runtime:        "errors/member_access_any_runtime.lox"       contains "not a struct or tuple",
    error_fn_call_lvalue:                   "errors/fn_call_lvalue.lox"                  contains "must be a valid identifier",
    error_struct_field_write_readonly:      "errors/struct_field_write_readonly.lox"     contains "read-only",
    error_tuple_index_oob_static:           "errors/tuple_index_oob_static.lox"          contains "index at 5",
    error_tuple_index_oob_runtime:          "errors/tuple_index_oob_runtime.lox"         contains "out of bounds",
    error_tuple_as_map_key_annotation:      "errors/tuple_as_map_key_annotation.lox"     contains "Invalid map key type",
    error_tuple_as_map_key_runtime:         "errors/tuple_as_map_key_runtime.lox"        contains "cannot be used as a key",
    // Caught at typecheck under the destructure rules.
    error_tuple_destructure_non_tuple:      "errors/tuple_destructure_non_tuple.lox"     contains "cannot be destructured as a tuple",
    error_tuple_destructure_arity:          "errors/tuple_destructure_arity.lox"         contains "destructuring expected",
    // `any` rhs hides the shape/arity from typecheck; runtime must catch.
    error_tuple_destructure_non_tuple_runtime: "errors/tuple_destructure_non_tuple_runtime.lox" contains "cannot be destructured as a tuple",
    error_tuple_destructure_arity_runtime:  "errors/tuple_destructure_arity_runtime.lox" contains "destructuring expected",
}
