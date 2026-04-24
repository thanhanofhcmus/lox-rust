//! End-to-end integration tests.
//!
//! Each test runs a fixture file through the built `lox-rust` binary with the
//! `--strict-assert` flag. The fixture is a self-checking lox program: a series
//! of `assert(...)` calls. Under strict-assert, a failing assertion raises
//! `InterpretError::AssertionFailed(msg)` and the process exits non-zero,
//! giving us a clean pass/fail signal.

mod common;

use common::{assert_err_contains, assert_ok, run_fixture};

// ---------- positive-path: every feature should run to exit 0 ----------

#[test]
fn scalars() {
    assert_ok(&run_fixture("01_scalars.lox"));
}

#[test]
fn arithmetic() {
    assert_ok(&run_fixture("02_arithmetic.lox"));
}

#[test]
fn boolean_logic() {
    assert_ok(&run_fixture("03_boolean_logic.lox"));
}

#[test]
fn comparisons() {
    assert_ok(&run_fixture("04_comparisons.lox"));
}

#[test]
fn variables_and_types() {
    assert_ok(&run_fixture("05_variables_and_types.lox"));
}

#[test]
fn strings() {
    assert_ok(&run_fixture("06_strings.lox"));
}

#[test]
fn blocks() {
    assert_ok(&run_fixture("07_blocks.lox"));
}

#[test]
fn if_else() {
    assert_ok(&run_fixture("08_if_else.lox"));
}

#[test]
fn while_loop() {
    assert_ok(&run_fixture("09_while_loop.lox"));
}

#[test]
fn for_loop() {
    assert_ok(&run_fixture("10_for_loop.lox"));
}

#[test]
fn when_expr() {
    assert_ok(&run_fixture("11_when_expr.lox"));
}

#[test]
fn arrays() {
    assert_ok(&run_fixture("12_arrays.lox"));
}

#[test]
fn maps() {
    assert_ok(&run_fixture("13_maps.lox"));
}

#[test]
fn functions() {
    assert_ok(&run_fixture("14_functions.lox"));
}

#[test]
fn chaining() {
    assert_ok(&run_fixture("15_chaining.lox"));
}

#[test]
fn json_roundtrip() {
    assert_ok(&run_fixture("16_json.lox"));
}

#[test]
fn structs() {
    assert_ok(&run_fixture("17_structs.lox"));
}

#[test]
fn tuples() {
    assert_ok(&run_fixture("18_tuples.lox"));
}

// ---------- negative-path: must exit non-zero with a specific message ----------

#[test]
fn error_div_by_zero() {
    let res = run_fixture("errors/div_by_zero.lox");
    assert_err_contains(&res, "Division by zero");
}

#[test]
fn error_array_out_of_bounds() {
    let res = run_fixture("errors/array_out_of_bounds.lox");
    assert_err_contains(&res, "out of bounds");
}

#[test]
fn error_assertion_failure_under_strict() {
    let res = run_fixture("errors/assertion_failure.lox");
    assert_err_contains(&res, "Assertion failed: deliberate");
}

#[test]
fn error_struct_unknown_field() {
    let res = run_fixture("errors/struct_unknown_field.lox");
    assert_err_contains(&res, "has no field");
}

#[test]
fn error_member_access_on_non_struct() {
    let res = run_fixture("errors/member_access_on_non_struct.lox");
    assert_err_contains(&res, "is not a struct");
}

#[test]
fn error_member_access_any_runtime() {
    let res = run_fixture("errors/member_access_any_runtime.lox");
    assert_err_contains(&res, "not a struct or tuple");
}

#[test]
fn error_fn_call_lvalue() {
    let res = run_fixture("errors/fn_call_lvalue.lox");
    assert_err_contains(&res, "must be a valid identifier");
}

#[test]
fn error_struct_field_write_readonly() {
    let res = run_fixture("errors/struct_field_write_readonly.lox");
    assert_err_contains(&res, "read-only");
}

#[test]
fn error_tuple_index_oob_static() {
    let res = run_fixture("errors/tuple_index_oob_static.lox");
    assert_err_contains(&res, "index at 5");
}

#[test]
fn error_tuple_index_oob_runtime() {
    let res = run_fixture("errors/tuple_index_oob_runtime.lox");
    assert_err_contains(&res, "out of bounds");
}

#[test]
fn error_tuple_as_map_key_annotation() {
    let res = run_fixture("errors/tuple_as_map_key_annotation.lox");
    assert_err_contains(&res, "InvalidMapKeyType");
}

#[test]
fn error_tuple_as_map_key_runtime() {
    let res = run_fixture("errors/tuple_as_map_key_runtime.lox");
    assert_err_contains(&res, "cannot be used as a key");
}

#[test]
fn error_tuple_destructure_non_tuple() {
    // Caught at typecheck under the new destructure rules.
    let res = run_fixture("errors/tuple_destructure_non_tuple.lox");
    assert_err_contains(&res, "cannot be destructured as a tuple");
}

#[test]
fn error_tuple_destructure_arity() {
    // Caught at typecheck under the new destructure rules.
    let res = run_fixture("errors/tuple_destructure_arity.lox");
    assert_err_contains(&res, "destructuring expected");
}

#[test]
fn error_tuple_destructure_non_tuple_runtime() {
    // `any` rhs hides the shape from typecheck; runtime must catch.
    let res = run_fixture("errors/tuple_destructure_non_tuple_runtime.lox");
    assert_err_contains(&res, "cannot be destructured as a tuple");
}

#[test]
fn error_tuple_destructure_arity_runtime() {
    // `any` rhs hides the arity from typecheck; runtime must catch.
    let res = run_fixture("errors/tuple_destructure_arity_runtime.lox");
    assert_err_contains(&res, "destructuring expected");
}
