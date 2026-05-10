use std::{cell::RefCell, collections::HashMap, path::Path, rc::Rc};

use log::{debug, error, info, trace};
use rustyline::{DefaultEditor, error::ReadlineError};
use thiserror::Error;

use crate::{
    ast::AST,
    dag::{DAG, NodeId},
    id::Id,
    identifier_registry::IdentifierRegistry,
    interpret::{self, InterpretError},
    module::{ModuleMetadata, ModuleStringInterner},
    parse::{self, ParseError},
    typecheck::{self, TypecheckError},
    types::{TypeId, TypeInterner},
};

/// Internal control-flow signal for "the script failed".
/// Each variant carries a user-facing message and the underlying error.
#[derive(Debug, Error)]
pub enum RunError {
    #[error("{message}")]
    Parse {
        message: String,
        #[source]
        inner: ParseError,
    },
    #[error("{message}")]
    Typecheck { message: String, inner: TypecheckError },
    #[error("{message}")]
    Interpret { message: String, inner: InterpretError },
    #[error("circular import detected")]
    CircularImport,
    #[error("module not found: {0}")]
    ModuleNotFound(String),
    #[error("failed to read module: {0}")]
    ModuleReadError(#[from] std::io::Error),
}

pub type RunResult = Result<(), RunError>;
pub type DynResult = Result<(), Box<dyn std::error::Error>>;

/// Encapsulates the runtime environment to avoid duplicating setup across modes.
pub struct RunnerContext {
    global_identifier_registry: IdentifierRegistry,
    typecheck_module_registry: typecheck::ModuleRegistry,
    interpret_module_registry: interpret::ModuleRegistry,
    type_interner: TypeInterner,
    module_string_interner: ModuleStringInterner,

    parse_cache: HashMap<ModuleMetadata, AST<()>>,
    typecheck_cache: HashMap<ModuleMetadata, AST<TypeId>>,

    repl_typecheck_module: typecheck::Module,
    repl_interpreter_module: interpret::Module,

    strict_assert: bool,
}

impl RunnerContext {
    pub fn new(strict_assert: bool) -> Self {
        let type_interner = TypeInterner::new();
        Self {
            global_identifier_registry: IdentifierRegistry::default(),
            typecheck_module_registry: typecheck::ModuleRegistry::default(),
            interpret_module_registry: interpret::ModuleRegistry::default(),
            module_string_interner: ModuleStringInterner::default(),
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
            let msg = err.generate_user_facing_error(source_name, input);
            error!("Lex error:\n{}", msg);
            RunError::Parse {
                message: msg,
                inner: err,
            }
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
        let ast = parse::parse(
            input,
            &tokens,
            &mut self.global_identifier_registry,
            &mut self.module_string_interner,
            is_in_repl,
        )
        .map_err(|err| {
            let msg = err.generate_user_facing_error(source_name, input);
            error!("Parse error:\n{}", msg);
            RunError::Parse {
                message: msg,
                inner: err,
            }
        })?;

        debug!("Parsing done");
        trace!("{:?}", &ast);

        Ok(ast)
    }

    fn create_typecheck_env(&mut self) -> typecheck::Environment<'_> {
        typecheck::Environment::new(&mut self.type_interner, &self.typecheck_module_registry)
    }

    fn create_repl_typecheck_env(&mut self) -> typecheck::Environment<'_> {
        let repl_module = std::mem::take(&mut self.repl_typecheck_module);
        typecheck::Environment::from_module(repl_module, &mut self.type_interner, &self.typecheck_module_registry)
    }

    fn type_check(
        &mut self,
        ast: AST<()>,
        input: &str,
        source_name: Option<&str>,
        is_repl_module: bool,
    ) -> Result<(AST<TypeId>, typecheck::Module), RunError> {
        debug!("type checking start");
        let typecheck_env = if is_repl_module { self.create_repl_typecheck_env() } else { self.create_typecheck_env() };
        let mut typechecker = typecheck::TypeChecker::new(typecheck_env);

        match typechecker.convert(ast) {
            Err(err) => {
                let msg = err.generate_user_facing_error(
                    source_name,
                    input,
                    &self.global_identifier_registry,
                    &self.type_interner,
                );
                error!("Typecheck error:\n{}", msg);
                Err(RunError::Typecheck {
                    message: msg,
                    inner: err,
                })
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
        is_repl_module: bool,
    ) -> Result<interpret::Module, RunError> {
        debug!("interpreting start");
        let print_writer = Rc::new(RefCell::new(std::io::stdout()));
        let strict_assert = self.strict_assert;
        let interpret_env = if is_repl_module {
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
                let msg = err.generate_user_facing_error(
                    source_name,
                    input,
                    interpreter.get_env(),
                    interpreter.get_identifer_registry(),
                );
                error!("Interpreter error:\n{}", msg);
                Err(RunError::Interpret {
                    message: msg,
                    inner: err,
                })
            }
        }
    }

    fn run_stmt(&mut self, input: &str, source_name: Option<&str>) -> RunResult {
        let is_in_repl = source_name.is_none();

        let (mut module_dag, root_module_metadata) = self.parse_module_tree(input, source_name, is_in_repl)?;

        if module_dag.has_cycle() {
            error!("Circular import detected");
            return Err(RunError::CircularImport);
        }
        module_dag.transitive_reduce();

        let order = module_dag.get_leaf_first_order();

        self.typecheck_module_tree(
            &module_dag,
            &order,
            &root_module_metadata,
            input,
            source_name,
            is_in_repl,
        )?;

        self.interpret_module_tree(
            &module_dag,
            &order,
            &root_module_metadata,
            input,
            source_name,
            is_in_repl,
        )?;

        Ok(())
    }

    /// Parse the root module and discover + parse all imported modules.
    /// Returns the module DAG and the root module's metadata.
    fn parse_module_tree(
        &mut self,
        input: &str,
        source_name: Option<&str>,
        is_in_repl: bool,
    ) -> Result<(DAG<ModuleMetadata>, ModuleMetadata), RunError> {
        let mut module_dag = DAG::new();
        let mut import_queue = vec![];

        let ast = self.lex_and_parse(input, source_name, is_in_repl)?;

        let root_module_metadata = ModuleMetadata {
            package: Id::SELF,
            path: self.module_string_interner.intern(source_name.unwrap_or(".")),
        };

        let root_module_node_id = module_dag.add_node(root_module_metadata.clone());
        for imp in &ast.imports {
            let md_node_id = module_dag.add_node(imp.metadata.clone());
            module_dag.add_edge(root_module_node_id, md_node_id);
            import_queue.push(md_node_id);
        }

        self.parse_cache.insert(root_module_metadata.clone(), ast);

        // discover and parse all imported modules (BFS)
        while let Some(current_node_id) = import_queue.pop() {
            let node_metadata = module_dag
                .get_node(current_node_id)
                .expect("import queue should only contain valid DAG node ids")
                .data
                .clone();
            if self.parse_cache.contains_key(&node_metadata) {
                continue;
            }

            let path = self
                .module_string_interner
                .get(node_metadata.path)
                .expect("import path id should have been interned during parsing")
                .to_string();

            if node_metadata.package != Id::SELF {
                error!("External packages are not yet supported: {path}");
                return Err(RunError::ModuleNotFound(path));
            }

            // TODO: move file loader to an interface
            let file_path = Path::new(&path);
            if !file_path.exists() || !file_path.is_file() {
                return Err(RunError::ModuleNotFound(path));
            }

            let content = std::fs::read_to_string(file_path)?;
            let untyped_ast = self.lex_and_parse(&content, Some(&path), false)?;

            for imp in &untyped_ast.imports {
                let md_node_id = module_dag.add_node(imp.metadata.clone());
                module_dag.add_edge(current_node_id, md_node_id);
                import_queue.push(md_node_id);
            }

            self.parse_cache.insert(node_metadata, untyped_ast);
        }

        Ok((module_dag, root_module_metadata))
    }

    /// Typecheck every module in leaf-first order.
    fn typecheck_module_tree(
        &mut self,
        module_dag: &DAG<ModuleMetadata>,
        order: &[NodeId<ModuleMetadata>],
        root_module_metadata: &ModuleMetadata,
        input: &str,
        source_name: Option<&str>,
        is_in_repl: bool,
    ) -> RunResult {
        for &current_node_id in order {
            let node_metadata = module_dag
                .get_node(current_node_id)
                .expect("leaf-first order should only contain valid DAG node ids")
                .data
                .clone();
            let untyped_ast = self
                .parse_cache
                .get(&node_metadata)
                .expect("parse_cache should be populated for every module before typecheck")
                .clone();

            let is_repl_module = is_in_repl && *root_module_metadata == node_metadata;

            let (typed_ast, module) = self.type_check(untyped_ast, input, source_name, is_repl_module)?;
            self.typecheck_cache.insert(node_metadata.clone(), typed_ast);
            if is_repl_module {
                self.repl_typecheck_module = module;
            } else {
                self.typecheck_module_registry.insert(node_metadata, module);
            }
        }

        Ok(())
    }

    /// Interpret every module in leaf-first order.
    fn interpret_module_tree(
        &mut self,
        module_dag: &DAG<ModuleMetadata>,
        order: &[NodeId<ModuleMetadata>],
        root_module_metadata: &ModuleMetadata,
        input: &str,
        source_name: Option<&str>,
        is_in_repl: bool,
    ) -> RunResult {
        for &current_node_id in order {
            let node_metadata = module_dag
                .get_node(current_node_id)
                .expect("leaf-first order should only contain valid DAG node ids")
                .data
                .clone();
            let typed_ast = self
                .typecheck_cache
                .get(&node_metadata)
                .expect("typecheck_cache should be populated for every module before interpret")
                .clone();

            let is_repl_module = is_in_repl && *root_module_metadata == node_metadata;

            let module = self.interpret(typed_ast, input, source_name, is_repl_module)?;
            if is_repl_module {
                self.repl_interpreter_module = module;
            } else {
                self.interpret_module_registry.insert(node_metadata, module);
            }
        }

        Ok(())
    }
}

pub fn run_prompt(ctx: &mut RunnerContext, line: &str) -> DynResult {
    info!("Running in Prompt mode");
    ctx.run_stmt(line, Some("<prompt>"))
        .map_err(|err| Box::new(err) as Box<dyn std::error::Error>)
}

pub fn run_file(ctx: &mut RunnerContext, file_path: &str) -> DynResult {
    info!("Read from file: {}", file_path);
    let contents = std::fs::read_to_string(file_path)?;
    ctx.run_stmt(&contents, Some(file_path))
        .map_err(|err| Box::new(err) as Box<dyn std::error::Error>)
}

pub fn run_repl(ctx: &mut RunnerContext, initial_line: Option<String>) -> DynResult {
    info!("Running in REPL mode");

    let mut rl = DefaultEditor::new()?;
    rl.add_history_entry("_dbg_heap_stats();")?;
    rl.add_history_entry("_dbg_state();")?;

    if let Some(line) = initial_line {
        rl.add_history_entry(&line)?;
        // error is aready reported un run_stmt
        _ = ctx.run_stmt(line.trim_end(), None);
    }

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                // error is aready reported un run_stmt
                _ = ctx.run_stmt(line.trim_end(), None);
            }
            Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => break,
            Err(err) => return Err(Box::new(err)),
        }
    }

    Ok(())
}
