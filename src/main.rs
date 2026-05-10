use lox_rust::cli::{Config, Mode};
use lox_rust::runner::{DynResult, RunError, RunnerContext, run_file, run_prompt, run_repl};

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
