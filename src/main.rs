mod ast;
mod id;
mod interpreter;
mod parse;
mod span;
mod token;
mod vm;

use std::{cell::RefCell, rc::Rc};

use crate::ast::Statement;
use log::{debug, error, info, trace};
use rustyline::{error::ReadlineError, DefaultEditor};

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
        _ => panic!("expect a mode"),
    }
}

fn repl() -> DynResult {
    info!("Running in REPL mode");

    let mut rl = DefaultEditor::new()?;

    let rc = Rc::new(RefCell::new(std::io::stdout()));
    let mut itp_ctx = interpreter::Environment::new(rc);
    let mut line: String;

    loop {
        match rl.readline("> ") {
            Ok(repl_line) => {
                line = repl_line;
                rl.add_history_entry(&line)?;
                run_stmt(line.trim_end(), &mut itp_ctx, true).unwrap_or_else(|e| error!("{}", e));
            }
            Err(ReadlineError::Eof) => break,
            Err(ReadlineError::Interrupted) => break,
            Err(err) => {
                return Err(Box::new(err));
            }
        }
    }

    Ok(())
}

fn run_vm() -> DynResult {
    let mut vm = vm::VM::new();

    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;

        if line.trim_end() == "quit" {
            return Ok(());
        }

        let stmt = parse(&line)?;
        vm::compile_and_run(&mut vm, &stmt)?;
    }
}

fn read_from_file(file_path: &str) -> DynResult {
    info!("Read from file");
    let contents = std::fs::read_to_string(file_path)?;
    let rc = Rc::new(RefCell::new(std::io::stdout()));
    let mut itp = interpreter::Environment::new(rc);
    run_stmt(&contents, &mut itp, false)
}

fn parse(input: &str) -> Result<Statement, Box<dyn std::error::Error>> {
    let tokens = match parse::lex(input) {
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

    let stmt = match parse::parse(input, &tokens) {
        Ok(list) => list,
        Err(err) => {
            error!("Parse error {:?}: {}", err.get_source_start(input), err);
            return Err(Box::new(err));
        }
    };
    trace!("{:?}", &stmt);

    Ok(stmt)
}

fn run_stmt(input: &str, itp_ctx: &mut interpreter::Environment, print_result: bool) -> DynResult {
    let stmt = parse(input)?;

    match interpreter::Interpreter::new(itp_ctx, input).interpret(&stmt) {
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
