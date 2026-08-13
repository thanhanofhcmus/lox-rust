use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

use log::{debug, error, info, trace};
use rustyline::{DefaultEditor, error::ReadlineError};
use thiserror::Error;

use crate::{
    ast::AST,
    dag::DAG,
    identifier_registry::IdentifierRegistry,
    input_source::InputSource,
    interpret::{self, Heap, InterpretError},
    module::{ModuleIdentity, ModuleStringInterner},
    parse::{self, ParseError},
    typecheck::{self, TypecheckError},
    types::{TypeId, TypeInterner},
};

const REPL_LINE: &str = "<line>";
const PROMPT_LINE: &str = "<prompt>";

crate::define_type_index!(pub struct ModuleDagId);

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
    interpret_heap: interpret::Heap,

    // caches keyed by resolved ModuleIdentity
    parse_cache: HashMap<ModuleIdentity, AST<()>>,
    source_cache: HashMap<ModuleIdentity, InputSource>,
    typecheck_cache: HashMap<ModuleIdentity, AST<TypeId>>,

    repl_typecheck_module: typecheck::Module,
    repl_interpreter_module: interpret::Module,

    strict_assert: bool,
}

impl RunnerContext {
    pub fn new(strict_assert: bool) -> Self {
        let mut type_interner = TypeInterner::new();
        let mut module_string_interner = ModuleStringInterner::default();

        let mut typecheck_module_registry = typecheck::ModuleRegistry::default();
        let mut interpret_module_registry = interpret::ModuleRegistry::default();

        // Pre-populate std modules into both registries
        {
            let tc_modules =
                crate::std_module::create_typecheck_modules(&mut module_string_interner, &mut type_interner);
            for (identity, module) in tc_modules {
                typecheck_module_registry.insert(identity, module);
            }

            let it_modules = crate::std_module::create_interpret_modules(&mut module_string_interner);
            for (identity, module) in it_modules {
                interpret_module_registry.insert(identity, module);
            }
        }

        Self {
            global_identifier_registry: IdentifierRegistry::default(),
            typecheck_module_registry,
            interpret_module_registry,
            module_string_interner,
            type_interner,
            interpret_heap: Heap::new(),

            parse_cache: HashMap::new(),
            source_cache: HashMap::new(),
            typecheck_cache: HashMap::new(),

            repl_typecheck_module: typecheck::Module::default(),
            repl_interpreter_module: interpret::Module::default(),

            strict_assert,
        }
    }

    fn lex_and_parse(&mut self, source: &InputSource) -> Result<AST<()>, RunError> {
        trace!("Lexing start");

        let input = source.get_text().map_err(RunError::ModuleReadError)?;

        let tokens = parse::lex(&input).map_err(|err| {
            let msg = err.generate_user_facing_error(source);
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
                token.span.str_from_source(&input),
            );
        }

        debug!("Parsing start");
        let ast = parse::parse(
            &input,
            &tokens,
            &mut self.global_identifier_registry,
            &mut self.module_string_interner,
        )
        .map_err(|err| {
            let msg = err.generate_user_facing_error(source);
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

    fn type_check(&mut self, ast: AST<()>, source: &InputSource) -> Result<(AST<TypeId>, typecheck::Module), RunError> {
        debug!("type checking start");
        let typecheck_env = if matches!(source, InputSource::Repl(_)) {
            let repl_module = self.repl_typecheck_module.clone();
            typecheck::Environment::from_module(repl_module, &mut self.type_interner, &self.typecheck_module_registry)
        } else {
            typecheck::Environment::new(&mut self.type_interner, &self.typecheck_module_registry)
        };
        let mut typechecker = typecheck::TypeChecker::new(typecheck_env);

        match typechecker.convert(ast) {
            Err(err) => {
                let msg = err.generate_user_facing_error(source, &self.global_identifier_registry, &self.type_interner);
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

    fn interpret(&mut self, ast: AST<TypeId>, source: &InputSource) -> Result<interpret::Module, RunError> {
        debug!("interpreting start");
        let print_writer = Rc::new(RefCell::new(std::io::stdout()));
        let strict_assert = self.strict_assert;
        let interpret_env = if matches!(source, InputSource::Repl(_)) {
            let repl_module = self.repl_interpreter_module.clone();
            interpret::Environment::from_module(repl_module, &mut self.interpret_heap, &self.interpret_module_registry)
        } else {
            interpret::Environment::new(&mut self.interpret_heap, &self.interpret_module_registry)
        };

        let mut interpreter = interpret::Interpreter::new(
            interpret_env,
            &mut self.global_identifier_registry,
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
                let msg =
                    err.generate_user_facing_error(source, interpreter.get_env(), interpreter.get_identifer_registry());
                error!("Interpreter error:\n{}", msg);
                Err(RunError::Interpret {
                    message: msg,
                    inner: err,
                })
            }
        }
    }

    pub fn run_stmt(&mut self, input: &str, source_name: &str) -> RunResult {
        let is_in_repl = source_name == REPL_LINE;

        let saved_tc = self.repl_typecheck_module.clone();
        let saved_it = self.repl_interpreter_module.clone();

        let root_source = if is_in_repl {
            InputSource::Repl(input.to_string())
        } else if source_name == PROMPT_LINE {
            InputSource::Prompt(input.to_string())
        } else {
            // Absolute path for file mode
            let abs = std::path::absolute(Path::new(source_name)).unwrap_or_else(|_| PathBuf::from(source_name));
            InputSource::File(abs)
        };

        let mut module_dag = self.parse_module_tree(input, source_name, &root_source)?;

        if module_dag.has_cycle() {
            error!("Circular import detected");
            return Err(RunError::CircularImport);
        }
        module_dag.transitive_reduce();

        let order = module_dag.get_leaf_first_order();

        if let Err(e) = self.typecheck_module_tree(&module_dag, &order) {
            if is_in_repl {
                self.repl_typecheck_module = saved_tc;
            }
            return Err(e);
        }

        match self.interpret_module_tree(&module_dag, &order) {
            Ok(()) => Ok(()),
            Err(e) => {
                if is_in_repl {
                    self.repl_typecheck_module = saved_tc;
                    self.repl_interpreter_module = saved_it;
                }
                Err(e)
            }
        }
    }

    /// Parse the root module and discover + parse all imported modules.
    /// Returns the module DAG and the root module's ModuleIdentity.
    fn parse_module_tree(
        &mut self,
        _input: &str,
        source_name: &str,
        root_source: &InputSource,
    ) -> Result<DAG<ModuleIdentity, ModuleDagId>, RunError> {
        let mut module_dag = DAG::new();
        let mut import_queue: Vec<ModuleDagId> = vec![];

        let mut ast = self.lex_and_parse(root_source)?;

        let root_identity = ModuleIdentity {
            resolved_path: self.module_string_interner.intern(source_name),
            is_std: false,
        };

        let root_identity_node_id = module_dag.add_node(root_identity.clone());

        // Resolve each import relative to the importing module's directory.
        let importer_dir = Path::new(source_name).parent().unwrap_or(Path::new("."));

        // Resolve and set identities on the root module's ImportNodes,
        // and add DAG edges in one pass.
        for imp in &mut ast.imports {
            let identity = resolve_import_identity(
                imp.metadata.package,
                imp.metadata.path,
                importer_dir,
                &mut self.module_string_interner,
            )?;
            imp.identity = Some(identity.clone());
            let md_node_id = module_dag.add_node(identity);
            module_dag.add_edge(root_identity_node_id, md_node_id);
            if !imp.identity.as_ref().is_some_and(|id| id.is_std) {
                import_queue.push(md_node_id);
            }
        }

        self.parse_cache.insert(root_identity.clone(), ast);
        self.source_cache.insert(root_identity.clone(), root_source.clone());

        // discover and parse all imported modules (BFS)
        while let Some(current_node_id) = import_queue.pop() {
            let current_identity = module_dag
                .get_node(current_node_id)
                .expect("import queue should only contain valid DAG node ids")
                .data
                .clone();
            if self.parse_cache.contains_key(&current_identity) {
                continue;
            }

            // Std modules are pre-populated in the registries — nothing to parse.
            if current_identity.is_std {
                continue;
            }

            let path = self
                .module_string_interner
                .get(current_identity.resolved_path)
                .expect("import path id should have been interned during parsing")
                .to_string();

            // TODO: move file loader to an interface
            let file_path = Path::new(&path);
            if !file_path.exists() || !file_path.is_file() {
                return Err(RunError::ModuleNotFound(path));
            }

            // Canonicalize so spans work correctly with absolute paths
            let abs_path = std::path::absolute(file_path).unwrap_or_else(|_| file_path.to_path_buf());
            let file_source = InputSource::File(abs_path);

            let mut untyped_ast = self.lex_and_parse(&file_source)?;

            // Resolve this module's own imports relative to its directory,
            // set identities on ImportNodes, and add DAG edges in one pass.
            let importer_dir = file_path.parent().unwrap_or(Path::new("."));

            for imp in &mut untyped_ast.imports {
                let identity = resolve_import_identity(
                    imp.metadata.package,
                    imp.metadata.path,
                    importer_dir,
                    &mut self.module_string_interner,
                )?;
                imp.identity = Some(identity.clone());
                let next_node_id = module_dag.add_node(identity);
                module_dag.add_edge(current_node_id, next_node_id);
                if !imp.identity.as_ref().is_some_and(|id| id.is_std) {
                    import_queue.push(next_node_id);
                }
            }

            self.parse_cache.insert(current_identity.clone(), untyped_ast);
            self.source_cache.insert(current_identity, file_source);
        }

        Ok(module_dag)
    }

    /// Typecheck every module in leaf-first order.
    fn typecheck_module_tree(
        &mut self,
        module_dag: &DAG<ModuleIdentity, ModuleDagId>,
        order: &[ModuleDagId],
    ) -> RunResult {
        for &current_node_id in order {
            let node_identity = module_dag
                .get_node(current_node_id)
                .expect("leaf-first order should only contain valid DAG node ids")
                .data
                .clone();

            // Std modules are pre-populated in the typecheck registry — skip.
            if node_identity.is_std {
                continue;
            }

            let untyped_ast = self
                .parse_cache
                .get(&node_identity)
                .expect("parse_cache should be populated for every module before typecheck")
                .clone();

            let module_source = self
                .source_cache
                .get(&node_identity)
                .expect("source_cache should be populated for every module before typecheck")
                .clone(); // TODO: remove clone

            let (typed_ast, module) = self.type_check(untyped_ast, &module_source)?;
            self.typecheck_cache.insert(node_identity.clone(), typed_ast);
            if matches!(&module_source, InputSource::Repl(_)) {
                self.repl_typecheck_module = module;
            } else {
                self.typecheck_module_registry.insert(node_identity, module);
            }
        }

        Ok(())
    }

    /// Interpret every module in leaf-first order.
    fn interpret_module_tree(
        &mut self,
        module_dag: &DAG<ModuleIdentity, ModuleDagId>,
        order: &[ModuleDagId],
    ) -> RunResult {
        for &current_node_id in order {
            let node_identity = module_dag
                .get_node(current_node_id)
                .expect("leaf-first order should only contain valid DAG node ids")
                .data
                .clone();

            // Std modules are pre-populated in the interpret registry — skip.
            if node_identity.is_std {
                continue;
            }

            let typed_ast = self
                .typecheck_cache
                .get(&node_identity)
                .expect("typecheck_cache should be populated for every module before interpret")
                .clone();

            let module_source = self
                .source_cache
                .get(&node_identity)
                .expect("source_cache should be populated for every module before interpret")
                .clone();

            let module = self.interpret(typed_ast, &module_source)?;
            if matches!(&module_source, InputSource::Repl(_)) {
                self.repl_interpreter_module = module;
            } else {
                self.interpret_module_registry.insert(node_identity, module);
            }
        }

        Ok(())
    }
}

/// Resolve a relative import path against the importing module's directory.
fn resolve_relative_path(importer_dir: &Path, rel_path: &str) -> String {
    // Only resolve paths that are explicitly relative (./ or ../).
    // Other paths are treated as CWD-relative (legacy behavior).
    if rel_path.starts_with("./") || rel_path.starts_with("../") {
        importer_dir
            .join(rel_path)
            .components()
            .collect::<PathBuf>()
            .to_string_lossy()
            .into_owned()
    } else {
        rel_path.to_string()
    }
}

/// Resolve the identity of an imported module, handling std virtual modules.
fn resolve_import_identity(
    package: crate::id::Id,
    path: crate::module::ModuleStrId,
    importer_dir: &Path,
    msi: &mut ModuleStringInterner,
) -> Result<ModuleIdentity, RunError> {
    if package == crate::id::Id::STD {
        let rel_path = msi.get(path).expect("path should be interned");
        Ok(ModuleIdentity {
            resolved_path: msi.intern(&format!("std:{}", rel_path)),
            is_std: true,
        })
    } else {
        let rel_path = msi.get(path).expect("path should be interned");
        let resolved = resolve_relative_path(importer_dir, rel_path);
        Ok(ModuleIdentity {
            resolved_path: msi.intern(&resolved),
            is_std: false,
        })
    }
}

pub fn run_prompt(ctx: &mut RunnerContext, line: &str) -> DynResult {
    info!("Running in Prompt mode");
    ctx.run_stmt(line, PROMPT_LINE)
        .map_err(|err| Box::new(err) as Box<dyn std::error::Error>)
}

pub fn run_file(ctx: &mut RunnerContext, file_path: &str) -> DynResult {
    info!("Read from file: {}", file_path);
    let contents = std::fs::read_to_string(file_path)?;
    ctx.run_stmt(&contents, file_path)
        .map_err(|err| Box::new(err) as Box<dyn std::error::Error>)
}

pub fn run_repl(ctx: &mut RunnerContext, initial_line: Option<String>) -> DynResult {
    info!("Running in REPL mode");

    let mut rl = DefaultEditor::new()?;
    rl.add_history_entry("_dbg_heap_stats();")?;
    rl.add_history_entry("_dbg_state();")?;

    if let Some(line) = initial_line {
        rl.add_history_entry(&line)?;
        // error is already reported in run_stmt
        _ = ctx.run_stmt(line.trim_end(), REPL_LINE);
    }

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                // error is already reported in run_stmt
                _ = ctx.run_stmt(line.trim_end(), REPL_LINE);
            }
            Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => break,
            Err(err) => return Err(Box::new(err)),
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn repl_state_preserved_after_runtime_error() {
        let mut ctx = RunnerContext::new(true);

        // Build up state over multiple REPL lines.
        ctx.run_stmt("var x: number = 1;", REPL_LINE).unwrap();
        ctx.run_stmt("var y: any = true;", REPL_LINE).unwrap();

        // This typechecks but fails at runtime (int + bool).
        let err = ctx.run_stmt("x + y;", REPL_LINE).unwrap_err();
        assert!(matches!(err, RunError::Interpret { .. }));

        // Previous state must still be intact.
        assert!(ctx.run_stmt("print(x);", REPL_LINE).is_ok());
    }
}
