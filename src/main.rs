mod ast;
mod dag;
mod id;
mod identifier_registry;
mod interpret;
mod module;
mod parse;
mod span;
mod string_interner;
mod string_utils;
mod token;
mod type_index;
mod typecheck;
mod types;

use std::{cell::RefCell, collections::HashMap, env, path::Path, rc::Rc};

use log::{debug, error, info, trace};
use rustyline::{DefaultEditor, error::ReadlineError};
use thiserror::Error;

use crate::{
    ast::AST,
    dag::DAG,
    id::Id,
    identifier_registry::IdentifierRegistry,
    interpret::InterpretError,
    module::ModuleMetadata,
    parse::ParseError,
    typecheck::TypecheckError,
    types::{TypeId, TypeInterner},
};

/// Internal control-flow signal for "the script failed and the formatted
/// error was already printed to stderr via `error!(...)`". Display is empty
/// on purpose — main suppresses it and just exits non-zero.
#[derive(Debug, Error)]
enum RunError {
    #[error("")]
    Parse(ParseError),
    #[error("")]
    Typecheck(TypecheckError),
    #[error("")]
    Interpret(InterpretError),
}

type RunResult = Result<(), RunError>;
type DynResult = Result<(), Box<dyn std::error::Error>>;

/// Encapsulates the runtime environment to avoid duplicating setup across modes.
struct RunnerContext {
    global_identifier_registry: IdentifierRegistry,
    typecheck_module_registry: typecheck::ModuleRegistry,
    interpret_module_registry: interpret::ModuleRegistry,
    type_interner: TypeInterner,

    parse_cache: HashMap<ModuleMetadata, AST<()>>,
    typecheck_cache: HashMap<ModuleMetadata, AST<TypeId>>,

    repl_typecheck_module: typecheck::Module,
    repl_interpreter_module: interpret::Module,

    strict_assert: bool,
}

impl RunnerContext {
    fn new(strict_assert: bool) -> Self {
        let type_interner = TypeInterner::new();
        let typecheck_module_registry = typecheck::ModuleRegistry::new();
        Self {
            global_identifier_registry: IdentifierRegistry::new(),
            typecheck_module_registry,
            interpret_module_registry: interpret::ModuleRegistry::new(),
            type_interner,

            parse_cache: HashMap::new(),
            typecheck_cache: HashMap::new(),

            repl_typecheck_module: typecheck::Module::default(),
            repl_interpreter_module: interpret::Module::default(),

            strict_assert,
        }
    }

    fn lex_and_parse(&mut self, input: &str, source_name: Option<&str>, is_in_repl: bool) -> Result<AST<()>, RunError> {
        trace!("Lexing start");

        let tokens = parse::lex(input).map_err(|err| {
            error!("Lex error:\n{}", err.generate_user_facing_error(source_name, input));
            RunError::Parse(err)
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
        let ast = parse::parse(input, &tokens, &mut self.global_identifier_registry, is_in_repl).map_err(|err| {
            error!("Parse error:\n{}", err.generate_user_facing_error(source_name, input));
            RunError::Parse(err)
        })?;

        debug!("Parsing done");
        trace!("{:?}", &ast);

        Ok(ast)
    }

    fn create_typecheck_env(&'_ mut self) -> typecheck::Environment<'_> {
        typecheck::Environment::new(&mut self.type_interner, &self.typecheck_module_registry)
    }

    fn create_repl_typecheck_env(&'_ mut self) -> typecheck::Environment<'_> {
        let repl_module = std::mem::take(&mut self.repl_typecheck_module);
        typecheck::Environment::from_module(repl_module, &mut self.type_interner, &self.typecheck_module_registry)
    }

    fn type_check(
        &mut self,
        ast: AST<()>,
        input: &str,
        source_name: Option<&str>,
        is_repl: bool,
    ) -> Result<(AST<types::TypeId>, typecheck::Module), RunError> {
        debug!("type checking start");
        let typecheck_env = if is_repl { self.create_repl_typecheck_env() } else { self.create_typecheck_env() };
        let mut typechecker = typecheck::TypeChecker::new(typecheck_env);

        match typechecker.convert(ast) {
            Err(err) => {
                error!(
                    "Typecheck error:\n{}",
                    err.generate_user_facing_error(
                        source_name,
                        input,
                        &self.global_identifier_registry,
                        &self.type_interner,
                    )
                );
                Err(RunError::Typecheck(err))
            }
            Ok(ast) => {
                debug!("type checking done");
                let module = typechecker.make_module();
                Ok((ast, module))
            }
        }
    }

    fn interpret(
        &mut self,
        ast: AST<TypeId>,
        input: &str,
        source_name: Option<&str>,
        is_repl: bool,
    ) -> Result<interpret::Module, RunError> {
        debug!("interpreting start");
        let print_writer = Rc::new(RefCell::new(std::io::stdout()));
        let strict_assert = self.strict_assert;
        let interpret_env = if is_repl {
            let repl_module = std::mem::take(&mut self.repl_interpreter_module);
            interpret::Environment::from_module(repl_module, &self.interpret_module_registry)
        } else {
            interpret::Environment::new(&self.interpret_module_registry)
        };

        let mut interpreter = interpret::Interpreter::new(
            interpret_env,
            &mut self.global_identifier_registry,
            input,
            print_writer,
            strict_assert,
        );
        let result = interpreter.interpret(&ast).map(|_| interpreter.make_module());

        match result {
            Ok(module) => {
                debug!("interpreting done");
                Ok(module)
            }
            Err(err) => {
                error!(
                    "Interpreter error:\n{}",
                    err.generate_user_facing_error(
                        source_name,
                        input,
                        interpreter.get_env(),
                        interpreter.get_identifer_registry(),
                    )
                );
                Err(RunError::Interpret(err))
            }
        }
    }

    fn run_stmt(&mut self, input: &str, source_name: Option<&str>) -> RunResult {
        // for a module, we should only be: Parse once - Typecheck once - Interpret once

        let is_in_repl = source_name.is_none();

        let mut module_dag = DAG::new();
        let mut import_queue = vec![];

        let ast = self.lex_and_parse(input, source_name, is_in_repl)?;

        let root_module_metadata = ModuleMetadata {
            package: Id::SELF,
            // The REPL module or file
            path: source_name.unwrap_or(".").to_string(),
        };

        let root_module_node_id = module_dag.add_node(root_module_metadata.clone());
        for imp in &ast.imports {
            let md_node_id = module_dag.add_node(imp.metadata.clone());
            module_dag.add_edge(root_module_node_id, md_node_id);
            import_queue.push(md_node_id);
        }

        self.parse_cache.insert(root_module_metadata.clone(), ast);

        // both parse and build the module DAG
        while let Some(current_node_id) = import_queue.pop() {
            let node_metadata = module_dag.get_node(current_node_id).unwrap().data.clone();
            if self.parse_cache.contains_key(&node_metadata) {
                continue;
            }

            let path = &node_metadata.path;

            if node_metadata.package != Id::SELF {
                unimplemented!();
            }

            // read the file
            // TODO: move file loader to an interface
            let file_path = Path::new(&path);
            if !file_path.exists() || !file_path.is_file() {
                panic!("path is point to no file");
            }

            let content = std::fs::read_to_string(file_path).unwrap();
            let untyped_ast = self.lex_and_parse(&content, Some(path), true)?;

            for imp in &untyped_ast.imports {
                let md_node_id = module_dag.add_node(imp.metadata.clone());
                module_dag.add_edge(current_node_id, md_node_id);
                import_queue.push(md_node_id);
            }

            self.parse_cache.insert(node_metadata, untyped_ast);
        }

        if module_dag.has_circle() {
            unimplemented!()
        }
        module_dag.transitive_reduce();

        let order = module_dag.get_leaf_first_order();

        // we might want to have just a single global registry that contains every symbols there is

        // typecheck
        for &current_node_id in &order {
            let node_metadata = module_dag.get_node(current_node_id).unwrap().data.clone();
            let untyped_ast = self.parse_cache.get(&node_metadata).unwrap().clone();

            let is_repl = is_in_repl && root_module_metadata == node_metadata;

            let (typed_ast, module) = self.type_check(untyped_ast, input, source_name, is_repl)?;
            self.typecheck_cache.insert(node_metadata.clone(), typed_ast);
            if is_repl {
                self.repl_typecheck_module = module;
            } else {
                self.typecheck_module_registry.insert(node_metadata, module);
            }
        }

        // interpret
        for &current_node_id in &order {
            let node_metadata = module_dag.get_node(current_node_id).unwrap().data.clone();
            let typed_ast = self.typecheck_cache.get(&node_metadata).unwrap().clone();

            let is_repl = is_in_repl && root_module_metadata == node_metadata;

            let module = self.interpret(typed_ast, input, source_name, is_repl)?;
            if is_repl {
                self.repl_interpreter_module = module;
            } else {
                self.interpret_module_registry.insert(node_metadata, module);
            }
        }

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

        Self { mode, strict_assert }
    }
}

fn main() -> DynResult {
    if let Err(e) = dotenvy::dotenv() {
        println!("dotenvy load with error {}", e);
    }
    env_logger::init();

    let config = Config::parse();
    let mut ctx = RunnerContext::new(config.strict_assert);

    let run_result = match config.mode {
        Mode::Repl { initial_line } => run_repl(&mut ctx, initial_line),
        Mode::Prompt { line } => run_prompt(&mut ctx, &line),
        Mode::File { path } => run_file(&mut ctx, &path),
    };

    match run_result {
        Ok(()) => Ok(()),
        Err(error) => match error.downcast::<RunError>() {
            // Script-level failure — formatted error was already logged.
            // Suppress the lossy Box<dyn Error> Display and exit non-zero.
            Ok(_) => std::process::exit(1),
            // Other (e.g. IO, readline) — let main propagate normally.
            Err(other) => Err(other),
        },
    }
}

fn run_prompt(ctx: &mut RunnerContext, line: &str) -> DynResult {
    info!("Running in Prompt mode");
    ctx.run_stmt(line, None)
        .map_err(|err| Box::new(err) as Box<dyn std::error::Error>)
}

fn run_file(ctx: &mut RunnerContext, file_path: &str) -> DynResult {
    info!("Read from file: {}", file_path);
    let contents = std::fs::read_to_string(file_path)?;
    ctx.run_stmt(&contents, Some(file_path))
        .map_err(|err| Box::new(err) as Box<dyn std::error::Error>)
}

fn run_repl(ctx: &mut RunnerContext, initial_line: Option<String>) -> DynResult {
    info!("Running in REPL mode");

    let mut rl = DefaultEditor::new()?;
    rl.add_history_entry("_dbg_heap_stats();")?;
    rl.add_history_entry("_dbg_state();")?;

    if let Some(line) = initial_line {
        rl.add_history_entry(&line)?;
        let _ = ctx.run_stmt(line.trim_end(), None);
    }

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                let _ = ctx.run_stmt(line.trim_end(), None);
            }
            Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => break,
            Err(err) => return Err(Box::new(err)),
        }
    }

    Ok(())
}
