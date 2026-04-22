//! Shared helpers for end-to-end integration tests.
//!
//! Each test writes a self-checking lox program under `tests/fixtures/` and
//! runs it through the real CLI binary with `--strict-assert`. Under that
//! flag, a failing `assert(...)` becomes `InterpretError::AssertionFailed`,
//! which bubbles up and makes the process exit non-zero — giving us a clean
//! pass/fail signal without any in-process coupling to the interpreter.

use std::path::PathBuf;
use std::process::Command;

pub struct RunResult {
    pub status: i32,
    pub stdout: String,
    pub stderr: String,
}

/// Run a fixture file through `lox-rust -f <fixture> --strict-assert`.
/// `relative` is resolved against `tests/fixtures/`.
pub fn run_fixture(relative: &str) -> RunResult {
    let bin = env!("CARGO_BIN_EXE_lox-rust");
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(relative);

    assert!(fixture.exists(), "fixture not found: {}", fixture.display());

    let out = Command::new(bin)
        .arg("-f")
        .arg(&fixture)
        .arg("--strict-assert")
        // Force errors to reach stderr; env_logger defaults would otherwise
        // depend on the ambient RUST_LOG and make tests non-reproducible.
        .env("RUST_LOG", "error")
        .output()
        .expect("failed to spawn lox-rust binary");

    RunResult {
        status: out.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
    }
}

/// Assert the fixture exited with status 0. On failure, dump both streams so
/// the actual assertion error from the lox program is visible.
#[track_caller]
pub fn assert_ok(res: &RunResult) {
    assert_eq!(
        res.status, 0,
        "fixture exited with status {}\n--- stdout ---\n{}\n--- stderr ---\n{}",
        res.status, res.stdout, res.stderr
    );
}

/// Assert the fixture exited non-zero and some stream contains `needle`.
#[track_caller]
pub fn assert_err_contains(res: &RunResult, needle: &str) {
    assert_ne!(
        res.status, 0,
        "expected non-zero exit\n--- stdout ---\n{}\n--- stderr ---\n{}",
        res.stdout, res.stderr
    );
    let combined = format!("{}{}", res.stdout, res.stderr);
    assert!(
        combined.contains(needle),
        "expected output to contain {needle:?}\n--- stdout ---\n{}\n--- stderr ---\n{}",
        res.stdout,
        res.stderr
    );
}
