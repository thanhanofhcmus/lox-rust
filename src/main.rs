mod ast;
mod interpreter;
mod lex;
mod parse;
mod parse_error;
mod span;
mod token;
mod vm;

use std::{cell::RefCell, io::Write, rc::Rc};

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
        _ => panic!("expect a mode"),
    }
}

fn repl() -> DynResult {
    info!("Running in REPL mode");

    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    let rc = Rc::new(RefCell::new(std::io::stdout()));

    let mut ctx = interpreter::Context::new(rc);

    loop {
        stdout.write_all(b"> ")?;
        stdout.flush().unwrap();

        let mut line = String::new();
        stdin.read_line(&mut line)?;

        if line.trim_end() == "quit" {
            return Ok(());
        }

        run_stmt(line.trim_end(), &mut ctx, true).unwrap_or_else(|e| error!("{}", e));
    }
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
    run_stmt(&contents, &mut interpreter::Context::new(rc), false)
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

fn run_stmt(input: &str, ctx: &mut interpreter::Context, print_result: bool) -> DynResult {
    let stmt = parse(input)?;
    let calc_result = interpreter::interpret(ctx, &stmt);

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
