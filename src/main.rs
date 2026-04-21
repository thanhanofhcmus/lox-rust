mod ast;
mod id;
mod interpret;
mod parse;
mod span;
mod string_interner;
mod string_utils;
mod symbol_names;
mod token;
mod typecheck;
mod types;

use std::{cell::RefCell, env, rc::Rc};

use log::{debug, error, info, trace};
use rustyline::{DefaultEditor, error::ReadlineError};

use crate::{ast::AST, symbol_names::SymbolNames};

type DynResult = Result<(), Box<dyn std::error::Error>>;

/// Encapsulates the runtime environment to avoid duplicating setup across modes.
struct RunnerContext {
    symbol_names: SymbolNames,
    typecheck_env: typecheck::Environment,
    interpret_env: interpret::Environment,
    strict_assert: bool,
}

impl RunnerContext {
    fn new(strict_assert: bool) -> Self {
        Self {
            symbol_names: SymbolNames::new(),
            typecheck_env: typecheck::Environment::new(),
            interpret_env: interpret::Environment::new(),
            strict_assert,
        }
    }

    fn lex_and_parse(
        &mut self,
        input: &str,
        source_name: Option<&str>,
        is_in_repl: bool,
    ) -> Result<AST<()>, Box<dyn std::error::Error>> {
        trace!("Lexing start");

        let tokens = parse::lex(input).map_err(|err| {
            error!(
                "Lex error:\n{}",
                err.generate_user_facing_error(source_name, input)
            );
            Box::new(err) as Box<dyn std::error::Error>
        })?;

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
        let ast =
            parse::parse(input, &tokens, &mut self.symbol_names, is_in_repl).map_err(|err| {
                error!(
                    "Parse error:\n{}",
                    err.generate_user_facing_error(source_name, input)
                );
                Box::new(err) as Box<dyn std::error::Error>
            })?;

        debug!("Parsing done");
        trace!("{:?}", &ast);

        Ok(ast)
    }

    fn run_stmt(&mut self, input: &str, source_name: Option<&str>, is_in_repl: bool) -> DynResult {
        let ast = self.lex_and_parse(input, source_name, is_in_repl)?;

        debug!("type checking start");
        let mut typechecker = typecheck::TypeChecker::new(&mut self.typecheck_env);
        let ast = typechecker.convert(ast).map_err(|err| {
            error!(
                "Typecheck error:\n{}",
                err.generate_user_facing_error(
                    source_name,
                    input,
                    &self.symbol_names,
                    self.typecheck_env.get_type_interner()
                )
            );
            Box::new(err) as Box<dyn std::error::Error>
        })?;
        debug!("type checking done");

        debug!("interpreting start");
        let rc = Rc::new(RefCell::new(std::io::stdout()));
        let mut interpreter = interpret::Interpreter::new(
            &mut self.interpret_env,
            self.typecheck_env.get_type_interner(),
            &mut self.symbol_names,
            input,
            rc,
            self.strict_assert,
        );

        interpreter.interpret(&ast).map_err(|err| {
            error!(
                "Interpreter error:\n{}",
                err.generate_user_facing_error(source_name, input, &self.symbol_names)
            );
            Box::new(err) as Box<dyn std::error::Error>
        })?;
        debug!("interpreting done");

        Ok(())
    }
}

enum Mode {
    Repl { initial_line: Option<String> },
    Prompt { line: String },
    File { path: String },
}

struct Config {
    mode: Mode,
    strict_assert: bool,
}

impl Config {
    fn parse() -> Self {
        let mut args: Vec<String> = env::args().skip(1).collect();

        // Position-independent flag: consume it before mode dispatch.
        let strict_assert = {
            let before = args.len();
            args.retain(|a| a != "--strict-assert");
            args.len() != before
        };

        if args.is_empty() {
            panic!("expect a mode (-i, -p, -f)");
        }

        let mode_str = args.remove(0);
        let mode = match mode_str.as_str() {
            "-i" => Mode::Repl {
                initial_line: args.into_iter().next(),
            },
            "-p" => Mode::Prompt {
                line: args.into_iter().next().expect("must provide prompt line"),
            },
            "-f" => Mode::File {
                path: args.into_iter().next().expect("must provide file name"),
            },
            _ => panic!("unknown mode: {}", mode_str),
        };

        Self {
            mode,
            strict_assert,
        }
    }
}

fn main() -> DynResult {
    if let Err(e) = dotenvy::dotenv() {
        println!("dotenvy load with error {}", e);
    }
    env_logger::init();

    let config = Config::parse();
    let mut ctx = RunnerContext::new(config.strict_assert);

    match config.mode {
        Mode::Repl { initial_line } => run_repl(&mut ctx, initial_line),
        Mode::Prompt { line } => run_prompt(&mut ctx, &line),
        Mode::File { path } => run_file(&mut ctx, &path),
    }
}

fn run_prompt(ctx: &mut RunnerContext, line: &str) -> DynResult {
    info!("Running in Prompt mode");
    ctx.run_stmt(line, None, false)
}

fn run_file(ctx: &mut RunnerContext, file_path: &str) -> DynResult {
    info!("Read from file: {}", file_path);
    let contents = std::fs::read_to_string(file_path)?;
    ctx.run_stmt(&contents, Some(file_path), false)
}

fn run_repl(ctx: &mut RunnerContext, initial_line: Option<String>) -> DynResult {
    info!("Running in REPL mode");

    let mut rl = DefaultEditor::new()?;
    rl.add_history_entry("_dbg_heap_stats();")?;
    rl.add_history_entry("_dbg_state();")?;

    if let Some(line) = initial_line {
        rl.add_history_entry(&line)?;
        let _ = ctx.run_stmt(line.trim_end(), None, true);
    }

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                let _ = ctx.run_stmt(line.trim_end(), None, true);
            }
            Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => break,
            Err(err) => return Err(Box::new(err)),
        }
    }

    Ok(())
}
