mod ast;
mod id;
mod interpret;
mod parse;
mod span;
mod token;

use std::{cell::RefCell, rc::Rc};

use crate::ast::AST;
use log::{debug, error, info, trace};
use rustyline::{DefaultEditor, error::ReadlineError};

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
        "-i" => repl(args),
        "-f" => read_from_file(args.get(2).expect("must provide file name")),
        _ => panic!("expect a mode"),
    }
}

fn repl(args: Vec<String>) -> DynResult {
    info!("Running in REPL mode");

    let rc = Rc::new(RefCell::new(std::io::stdout()));
    let mut itp_env = interpret::Environment::new(rc);

    let mut rl = DefaultEditor::new()?;
    rl.add_history_entry("_dbg_heap_stats();")?;
    rl.add_history_entry("_dbg_state();")?;

    if let Some(line) = args.get(2) {
        rl.add_history_entry(line)?;
        _ = run_stmt(line.trim_end(), &mut itp_env, true);
    }

    let mut line: String;

    loop {
        match rl.readline("> ") {
            Ok(repl_line) => {
                line = repl_line;
                rl.add_history_entry(&line)?;
                _ = run_stmt(line.trim_end(), &mut itp_env, true);
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

fn read_from_file(file_path: &str) -> DynResult {
    info!("Read from file");
    let contents = std::fs::read_to_string(file_path)?;
    let rc = Rc::new(RefCell::new(std::io::stdout()));
    let mut itp = interpret::Environment::new(rc);
    run_stmt(&contents, &mut itp, false)
}

fn lex_and_parse(input: &str, is_in_repl: bool) -> Result<AST, Box<dyn std::error::Error>> {
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

    let ast = match parse::parse(input, &tokens, is_in_repl) {
        Ok(list) => list,
        Err(err) => {
            error!("Parse error {:?}: {}", err.get_source_start(input), err);
            return Err(Box::new(err));
        }
    };
    trace!("{:?}", &ast);

    Ok(ast)
}

fn run_stmt(
    input: &str,
    interpret_env: &mut interpret::Environment,
    is_in_repl: bool,
) -> DynResult {
    let stmt = lex_and_parse(input, is_in_repl)?;

    match interpret::Interpreter::new(interpret_env, input).interpret(&stmt) {
        Ok(_) => {}
        Err(err) => {
            error!("Interpreter error: {}", err);
            return Err(Box::new(err));
        }
    }

    Ok(())
}
