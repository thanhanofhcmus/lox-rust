mod ast;
mod interpreter;
mod lex;
mod parse;
mod parse_error;
mod span;
mod token;

use log::{debug, error, info};

type DynResult = Result<(), Box<dyn std::error::Error>>;

fn main() -> DynResult {
    env_logger::init();

    let args = std::env::args().collect::<Vec<String>>();
    let input = args.get(1).expect("must have one argument");
    debug!("{:?}", input);

    match input.as_str() {
        "-i" => repl(),
        "-f" => read_from_file(args.get(2).expect("must provide file name")),
        _ => run_stmt(input, &mut interpreter::Environment::new(), true),
    }
}

fn repl() -> DynResult {
    info!("Running in REPL mode");

    let mut env = interpreter::Environment::new();

    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;

        if line == "quit" {
            return Ok(());
        }

        run_stmt(line.trim_end(), &mut env, true).unwrap_or_else(|e| error!("{}", e));
    }
}

fn read_from_file(file_path: &str) -> DynResult {
    info!("Read from file");
    let contents = std::fs::read_to_string(file_path)?;
    run_stmt(&contents, &mut interpreter::Environment::new(), false)
}

fn run_stmt(input: &str, env: &mut interpreter::Environment, print_result: bool) -> DynResult {
    let tokens = match lex::lex(input) {
        Ok(list) => list,
        Err(err) => {
            error!("Lex error: {}", err);
            return Err(Box::new(err));
        }
    };

    for token in &tokens {
        debug!(
            "{} - {:?}: {:?}",
            token.span,
            token.token,
            token.span.str_from_source(input),
        );
    }

    let expr = match parse::parse(input, &tokens) {
        Ok(list) => list,
        Err(err) => {
            error!("Parse error: {}", err);
            return Err(Box::new(err));
        }
    };

    let calc_result = interpreter::interpret_stmt(env, expr);

    match calc_result {
        Ok(value) => {
            if print_result {
                info!(
                    "{:?}",
                    value
                        .map(|v| format!("{}", v))
                        .unwrap_or("None".to_string())
                )
            }
        }
        Err(err) => {
            error!("Interpreter error: {}", err);
            return Err(Box::new(err));
        }
    }

    Ok(())
}
