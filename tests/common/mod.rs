//! Shared helpers for end-to-end integration tests.
//!
//! Each test loads a self-checking lox program from `tests/fixtures/` and
//! runs it through the interpreter directly via the library. Under
//! `strict_assert`, a failing `assert(...)` becomes
//! `InterpretError::AssertionFailed`, which bubbles up as `Err(RunError)`
//! — giving us a clean pass/fail signal without spawning a subprocess.

use std::path::PathBuf;

use lox_rust::runner::{RunError, RunnerContext};

pub type FixtureResult = Result<(), RunError>;

/// Run a fixture file through the interpreter with `--strict-assert`.
/// `relative` is resolved against `tests/fixtures/`.
pub fn run_fixture(relative: &str) -> FixtureResult {
    // Suppress log output during tests (default level is Error, but
    // `is_test(true)` tells env_logger to avoid printing to stderr).
    let _ = env_logger::builder().is_test(true).try_init();

    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(relative);

    let source = std::fs::read_to_string(&fixture)
        .unwrap_or_else(|e| panic!("failed to read fixture {}: {e}", fixture.display()));

    let mut ctx = RunnerContext::new(true); // strict_assert = true
    ctx.run_stmt(&source, &fixture.to_string_lossy())
}

/// Assert the fixture ran successfully. On failure, dumps the error.
#[track_caller]
pub fn assert_ok(res: &FixtureResult) {
    if let Err(e) = res {
        panic!("fixture failed: {e}");
    }
}

/// Assert the fixture returned an error whose message contains `needle`.
#[track_caller]
pub fn assert_err_contains(res: &FixtureResult, needle: &str) {
    match res {
        Err(e) => {
            let msg = e.to_string();
            assert!(
                msg.contains(needle),
                "expected error to contain {needle:?}\n--- error ---\n{msg}",
            );
        }
        Ok(()) => {
            panic!("expected error but fixture succeeded");
        }
    }
}
