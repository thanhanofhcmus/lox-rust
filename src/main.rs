mod ast;
mod id;
mod interpret;
mod parse;
mod span;
mod string_utils;
mod token;
mod typecheck;
mod types;

use std::{cell::RefCell, rc::Rc};

use log::{debug, error, info, trace};
use rustyline::{DefaultEditor, error::ReadlineError};

use crate::ast::AST;

type DynResult = Result<(), Box<dyn std::error::Error>>;

fn main() -> DynResult {
    if let Err(e) = dotenvy::dotenv() {
        println!("dotenvy load with error {}", e);
    }
    env_logger::init();

    let mut args = std::env::args().collect::<Vec<String>>();

    // Position-independent flag: consume it before mode dispatch.
    let strict_assert = {
        let before = args.len();
        args.retain(|a| a != "--strict-assert");
        args.len() != before
    };

    let input = args.get(1).expect("must have one argument");
    debug!("{:?}", input);

    // TODO: add flags to ignore typecheck

    match input.as_str() {
        "-i" => repl(args, strict_assert),
        "-p" => promt(args, strict_assert),
        "-f" => read_from_file(
            args.get(2).expect("must provide file name"),
            strict_assert,
        ),
        _ => panic!("expect a mode"),
    }
}

fn promt(args: Vec<String>, strict_assert: bool) -> DynResult {
    info!("Running in Prompt mode");

    let line = args.get(2).expect("must provide prompt");

    let mut typecheck_env = typecheck::Environment::new();

    let rc = Rc::new(RefCell::new(std::io::stdout()));
    let mut interpreter_env = interpret::Environment::new(rc, strict_assert);

    run_stmt(line, None, &mut typecheck_env, &mut interpreter_env, false)
}

fn repl(args: Vec<String>, strict_assert: bool) -> DynResult {
    info!("Running in REPL mode");

    let mut typecheck_env = typecheck::Environment::new();

    let rc = Rc::new(RefCell::new(std::io::stdout()));
    let mut interpreter_env = interpret::Environment::new(rc, strict_assert);

    let mut rl = DefaultEditor::new()?;
    rl.add_history_entry("_dbg_heap_stats();")?;
    rl.add_history_entry("_dbg_state();")?;

    if let Some(line) = args.get(2) {
        rl.add_history_entry(line)?;
        _ = run_stmt(
            line.trim_end(),
            None,
            &mut typecheck_env,
            &mut interpreter_env,
            true,
        );
    }

    let mut line: String;

    loop {
        match rl.readline("> ") {
            Ok(repl_line) => {
                line = repl_line;
                rl.add_history_entry(&line)?;
                _ = run_stmt(
                    line.trim_end(),
                    None,
                    &mut typecheck_env,
                    &mut interpreter_env,
                    true,
                );
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

fn read_from_file(file_path: &str, strict_assert: bool) -> DynResult {
    info!("Read from file");
    let contents = std::fs::read_to_string(file_path)?;
    let mut typecheck_env = typecheck::Environment::new();
    let rc = Rc::new(RefCell::new(std::io::stdout()));
    let mut itp = interpret::Environment::new(rc, strict_assert);
    run_stmt(
        &contents,
        Some(file_path),
        &mut typecheck_env,
        &mut itp,
        false,
    )
}

fn lex_and_parse(
    input: &str,
    source_name: Option<&str>,
    is_in_repl: bool,
) -> Result<AST<()>, Box<dyn std::error::Error>> {
    trace!("Lexing start");

    let tokens = match parse::lex(input) {
        Ok(list) => list,
        Err(err) => {
            error!(
                "Lex error:\n{}",
                err.generate_user_facing_error(source_name, input)
            );
            return Err(Box::new(err));
        }
    };

    debug!("Lexing done, with tokens:");

    for token in &tokens {
        debug!(
            "{} - {:?}: {:?}",
            token.span,
            token.token,
            token.span.str_from_source(input),
        );
    }

    debug!("Parsing start");

    let ast = match parse::parse(input, &tokens, is_in_repl) {
        Ok(list) => list,
        Err(err) => {
            error!(
                "Parse error:\n{}",
                err.generate_user_facing_error(source_name, input)
            );
            return Err(Box::new(err));
        }
    };

    debug!("Parsing done");

    trace!("{:?}", &ast);

    Ok(ast)
}

fn run_stmt(
    input: &str,
    source_name: Option<&str>,
    typecheck_env: &mut typecheck::Environment,
    interpret_env: &mut interpret::Environment,
    is_in_repl: bool,
) -> DynResult {
    let ast = lex_and_parse(input, source_name, is_in_repl)?;

    debug!("type checking start");

    let mut typechecker = typecheck::TypeChecker::new(typecheck_env, input);

    let ast = match typechecker.convert(ast) {
        Ok(v) => v,
        Err(err) => {
            error!(
                "Typecheck error:\n{}",
                err.generate_user_facing_error(
                    source_name,
                    input,
                    typecheck_env.get_type_interner()
                )
            );
            return Err(Box::new(err));
        }
    };

    debug!("type checking done");

    debug!("interpreting start");

    match interpret::Interpreter::new(interpret_env, input).interpret(&ast) {
        Ok(_) => {}
        Err(err) => {
            error!(
                "Interpreter error:\n{}",
                err.generate_user_facing_error(source_name, input)
            );
            return Err(Box::new(err));
        }
    }

    debug!("interpreting done");

    Ok(())
}
