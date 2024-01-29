mod ast;
mod interpreter;
mod lex;
mod parse;
mod parse_error;
mod span;
mod token;
mod vm;

use crate::ast::Statement;
use log::{debug, error, info, trace};

type DynResult = Result<(), Box<dyn std::error::Error>>;

fn main() -> DynResult {
    if let Err(e) = dotenvy::dotenv() {
        println!("dotenvy load with error {}", e);
    }
    env_logger::init();

    let args = std::env::args().collect::<Vec<String>>();
    let input = args.get(1).expect("must have one argument");
    debug!("{:?}", input);

    match input.as_str() {
        "-i" => repl(),
        "-f" => read_from_file(args.get(2).expect("must provide file name")),
        "-vm" => run_vm(),
        _ => run_stmt(input, &mut interpreter::Context::new(), true),
    }
}

fn repl() -> DynResult {
    info!("Running in REPL mode");

    let mut env = interpreter::Context::new();

    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;

        if line == "quit" {
            return Ok(());
        }

        run_stmt(line.trim_end(), &mut env, true).unwrap_or_else(|e| error!("{}", e));
    }
}

fn run_vm() -> DynResult {
    let mut vm = vm::VM::new();

    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;

        if line == "quit" {
            return Ok(());
        }

        let stmt = parse(&line)?;
        vm::compile_and_run(&mut vm, &stmt)?;
    }
}

fn read_from_file(file_path: &str) -> DynResult {
    info!("Read from file");
    let contents = std::fs::read_to_string(file_path)?;
    run_stmt(&contents, &mut interpreter::Context::new(), false)
}

fn parse(input: &str) -> Result<Statement, Box<dyn std::error::Error>> {
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
            trace!("{:?}", err.get_source_start(input));
            return Err(Box::new(err));
        }
    };
    trace!("{:?}", &expr);

    Ok(expr)
}

fn run_stmt(input: &str, env: &mut interpreter::Context, print_result: bool) -> DynResult {
    let stmt = parse(input)?;
    let calc_result = interpreter::interpret(env, &stmt);

    match calc_result {
        Ok(value) => {
            if print_result {
                info!("{:?}", value)
            }
        }
        Err(err) => {
            error!("Interpreter error: {}", err);
            return Err(Box::new(err));
        }
    }

    Ok(())
}
