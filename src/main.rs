mod ast;
mod cli;
mod dag;
mod id;
mod identifier_registry;
mod interpret;
mod module;
mod parse;
mod runner;
mod span;
mod string_interner;
mod string_utils;
mod token;
mod type_index;
mod typecheck;
mod types;

use cli::{Config, Mode};
use runner::{DynResult, RunError, RunnerContext, run_file, run_prompt, run_repl};

fn main() -> DynResult {
    if let Err(e) = dotenvy::dotenv() {
        eprintln!("dotenvy load with error {}", e);
    }
    env_logger::init();

    let config = Config::parse();
    let mut ctx = RunnerContext::new(config.strict_assert);

    let run_result = match config.mode {
        Mode::Repl { initial_line } => run_repl(&mut ctx, initial_line),
        Mode::Prompt { line } => run_prompt(&mut ctx, &line),
        Mode::File { path } => run_file(&mut ctx, &path),
    };

    handle_exit(run_result)
}

fn handle_exit(result: DynResult) -> DynResult {
    match result {
        Ok(()) => Ok(()),
        Err(error) => {
            if error.is::<RunError>() {
                std::process::exit(1);
            }
            Err(error)
        }
    }
}
