use std::{cell::RefCell, collections::HashMap, path::Path, path::PathBuf, rc::Rc};

use log::{debug, error, info, trace};
use rustyline::{DefaultEditor, error::ReadlineError};
use thiserror::Error;

use crate::{
    ast::AST,
    dag::DAG,
    id::Id,
    identifier_registry::IdentifierRegistry,
    interpret::{self, Heap, InterpretError},
    module::{ModuleIndex, ModuleMetadata, ModuleStringInterner},
    parse::{self, ParseError},
    typecheck::{self, TypecheckError},
    types::{TypeId, TypeInterner},
};

const REPL_LINE: &str = "<line>";
const PROMPT_LINE: &str = "<prompt>";

crate::define_type_index!(pub struct ModuleDagId);

/// The module DAG and supporting data produced by `parse_module_tree`.
type ModuleTree = (
    DAG<ModuleIndex, ModuleDagId>,
    ModuleIndex,
    HashMap<ModuleMetadata, ModuleIndex>,
);

/// Shared parameters passed through the typecheck / interpret phases.
struct PhaseParams<'a> {
    module_dag: &'a DAG<ModuleIndex, ModuleDagId>,
    order: &'a [ModuleDagId],
    root_index: &'a ModuleIndex,
    input: &'a str,
    source_name: &'a str,
    is_in_repl: bool,
    metadata_to_index: &'a HashMap<ModuleMetadata, ModuleIndex>,
}

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

    parse_cache: HashMap<ModuleIndex, AST<()>>,
    typecheck_cache: HashMap<ModuleIndex, AST<TypeId>>,

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
            interpret_heap: Heap::new(),

            parse_cache: HashMap::new(),
            typecheck_cache: HashMap::new(),

            repl_typecheck_module: typecheck::Module::default(),
            repl_interpreter_module: interpret::Module::default(),

            strict_assert,
        }
    }

    fn lex_and_parse(&mut self, input: &str, source_name: &str) -> Result<AST<()>, RunError> {
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
        let repl_module = self.repl_typecheck_module.clone();
        typecheck::Environment::from_module(repl_module, &mut self.type_interner, &self.typecheck_module_registry)
    }

    fn type_check(
        &mut self,
        ast: AST<()>,
        input: &str,
        source_name: &str,
        is_repl_module: bool,
        metadata_to_index: &HashMap<ModuleMetadata, ModuleIndex>,
    ) -> Result<(AST<TypeId>, typecheck::Module), RunError> {
        debug!("type checking start");
        let typecheck_env = if is_repl_module { self.create_repl_typecheck_env() } else { self.create_typecheck_env() };
        let mut typechecker = typecheck::TypeChecker::new(typecheck_env);

        match typechecker.convert(ast, metadata_to_index) {
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
        source_name: &str,
        is_repl_module: bool,
        metadata_to_index: &HashMap<ModuleMetadata, ModuleIndex>,
    ) -> Result<interpret::Module, RunError> {
        debug!("interpreting start");
        let print_writer = Rc::new(RefCell::new(std::io::stdout()));
        let strict_assert = self.strict_assert;
        let interpret_env = if is_repl_module {
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
        let result = interpreter
            .interpret(&ast, metadata_to_index)
            .map(|_| interpreter.make_module());

        match result {
            Ok(module) => {
                debug!("interpreting done");
                Ok(module)
            }
            Err(err) => {
                let env = interpreter.get_env();
                let ir = interpreter.get_identifer_registry();
                let msg = err.generate_user_facing_error(source_name, input, env, ir);
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

        let (mut module_dag, root_index, metadata_to_index) = self.parse_module_tree(input, source_name)?;

        if module_dag.has_cycle() {
            error!("Circular import detected");
            return Err(RunError::CircularImport);
        }
        module_dag.transitive_reduce();

        let order = module_dag.get_leaf_first_order();

        let params = PhaseParams {
            module_dag: &module_dag,
            order: &order,
            root_index: &root_index,
            input,
            source_name,
            is_in_repl,
            metadata_to_index: &metadata_to_index,
        };

        if let Err(e) = self.typecheck_module_tree(&params) {
            if is_in_repl {
                self.repl_typecheck_module = saved_tc;
            }
            return Err(e);
        }

        match self.interpret_module_tree(&params) {
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
    /// Returns the module DAG, the root module's index, and a map from
    /// source-level ModuleMetadata to resolved ModuleIndex.
    fn parse_module_tree(&mut self, input: &str, source_name: &str) -> Result<ModuleTree, RunError> {
        let mut module_dag: DAG<ModuleIndex, ModuleDagId> = DAG::new();
        let mut import_queue: Vec<(ModuleDagId, PathBuf)> = vec![];
        let mut metadata_to_index: HashMap<ModuleMetadata, ModuleIndex> = HashMap::new();

        let ast = self.lex_and_parse(input, source_name)?;

        // Resolve root module.
        let root_canonical = canonicalize_source(source_name)?;
        let root_index = ModuleIndex {
            package: Id::SELF,
            canonical_path: root_canonical.clone(),
        };

        let root_metadata = ModuleMetadata {
            package: Id::SELF,
            path: self.module_string_interner.intern(source_name),
        };
        metadata_to_index.insert(root_metadata, root_index.clone());

        let root_node_id = module_dag.add_node(root_index.clone());
        let root_dir = root_canonical.parent().unwrap_or(Path::new(".")).to_path_buf();

        for imp in &ast.imports {
            let imp_index = resolve_import(&imp.metadata, &self.module_string_interner, &root_dir)?;
            metadata_to_index.insert(imp.metadata.clone(), imp_index.clone());
            let md_node_id = module_dag.add_node(imp_index.clone());
            module_dag.add_edge(root_node_id, md_node_id);
            import_queue.push((
                md_node_id,
                imp_index
                    .canonical_path
                    .parent()
                    .unwrap_or(Path::new("."))
                    .to_path_buf(),
            ));
        }

        // In REPL mode the same root_index (SELF, cwd) is reused across
        // multiple lines — clear stale cache entries so the new line is used.
        if source_name == REPL_LINE {
            self.parse_cache.remove(&root_index);
            self.typecheck_cache.remove(&root_index);
        }

        self.parse_cache.insert(root_index.clone(), ast);

        // discover and parse all imported modules
        while let Some((current_node_id, parent_dir)) = import_queue.pop() {
            let node_index = module_dag
                .get_node(current_node_id)
                .expect("import queue should only contain valid DAG node ids")
                .data
                .clone();
            if self.parse_cache.contains_key(&node_index) {
                continue;
            }

            if node_index.package != Id::SELF {
                let path = node_index.canonical_path.display().to_string();
                error!("External packages are not yet supported: {path}");
                return Err(RunError::ModuleNotFound(path));
            }

            let file_path = &node_index.canonical_path;
            if !file_path.exists() || !file_path.is_file() {
                let path = file_path.display().to_string();
                return Err(RunError::ModuleNotFound(path));
            }

            let content = std::fs::read_to_string(file_path)?;

            let untyped_ast = self.lex_and_parse(&content, file_path.to_str().unwrap_or("<import>"))?;

            for imp in &untyped_ast.imports {
                let imp_index = resolve_import(&imp.metadata, &self.module_string_interner, &parent_dir)?;
                metadata_to_index.insert(imp.metadata.clone(), imp_index.clone());
                let child_dir = imp_index
                    .canonical_path
                    .parent()
                    .unwrap_or(Path::new("."))
                    .to_path_buf();
                let md_node_id = module_dag.add_node(imp_index);
                module_dag.add_edge(current_node_id, md_node_id);
                import_queue.push((md_node_id, child_dir));
            }

            self.parse_cache.insert(node_index, untyped_ast);
        }

        Ok((module_dag, root_index, metadata_to_index))
    }

    /// Typecheck every module in leaf-first order.
    fn typecheck_module_tree(&mut self, p: &PhaseParams) -> RunResult {
        for &current_node_id in p.order {
            let node_index = p
                .module_dag
                .get_node(current_node_id)
                .expect("leaf-first order should only contain valid DAG node ids")
                .data
                .clone();
            let untyped_ast = self
                .parse_cache
                .get(&node_index)
                .expect("parse_cache should be populated for every module before typecheck")
                .clone();

            let is_repl_module = p.is_in_repl && *p.root_index == node_index;

            let (typed_ast, module) =
                self.type_check(untyped_ast, p.input, p.source_name, is_repl_module, p.metadata_to_index)?;
            self.typecheck_cache.insert(node_index.clone(), typed_ast);
            if is_repl_module {
                self.repl_typecheck_module = module;
            } else {
                self.typecheck_module_registry.insert(node_index, module);
            }
        }

        Ok(())
    }

    /// Interpret every module in leaf-first order.
    fn interpret_module_tree(&mut self, p: &PhaseParams) -> RunResult {
        for &current_node_id in p.order {
            let node_index = p
                .module_dag
                .get_node(current_node_id)
                .expect("leaf-first order should only contain valid DAG node ids")
                .data
                .clone();
            let typed_ast = self
                .typecheck_cache
                .get(&node_index)
                .expect("typecheck_cache should be populated for every module before interpret")
                .clone();

            let is_repl_module = p.is_in_repl && *p.root_index == node_index;

            let module = self.interpret(typed_ast, p.input, p.source_name, is_repl_module, p.metadata_to_index)?;
            if is_repl_module {
                self.repl_interpreter_module = module;
            } else {
                self.interpret_module_registry.insert(node_index, module);
            }
        }

        Ok(())
    }
}

/// Canonicalize `source_name`.  For REPL/PROMPT sentinels, returns cwd().
/// For real files, resolves the absolute canonical path.
fn canonicalize_source(source_name: &str) -> Result<PathBuf, RunError> {
    if source_name == REPL_LINE || source_name == PROMPT_LINE {
        return Ok(std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));
    }
    let path = Path::new(source_name);
    if path.is_relative() {
        std::fs::canonicalize(path).map_err(RunError::ModuleReadError)
    } else {
        Ok(path.to_path_buf())
    }
}

/// Resolve an import's `ModuleMetadata` (package + raw relative path) into a
/// `ModuleIndex` with a canonical filesystem path.
fn resolve_import(
    metadata: &ModuleMetadata,
    msi: &ModuleStringInterner,
    parent_dir: &Path,
) -> Result<ModuleIndex, RunError> {
    let raw = msi
        .get(metadata.path)
        .ok_or_else(|| RunError::ModuleNotFound("<unknown>".to_string()))?
        .to_string();
    let joined = parent_dir.join(&raw);
    let canonical = std::fs::canonicalize(&joined).map_err(|_| RunError::ModuleNotFound(raw))?;
    Ok(ModuleIndex {
        package: metadata.package,
        canonical_path: canonical,
    })
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
        _ = ctx.run_stmt(line.trim_end(), REPL_LINE);
    }

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
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

    #[test]
    fn repl_caches_cleared_between_lines() {
        // Each REPL line reuses the same (SELF, cwd) ModuleIndex.
        // Stale caches must be cleared so the new line's AST is used.
        let mut ctx = RunnerContext::new(true);

        ctx.run_stmt("var a = 1;", REPL_LINE).unwrap();
        ctx.run_stmt("var b = 2;", REPL_LINE).unwrap();
        ctx.run_stmt("assert(a + b == 3, \"accumulates state\");", REPL_LINE)
            .unwrap();

        let root_index = ModuleIndex {
            package: Id::SELF,
            canonical_path: std::env::current_dir().unwrap(),
        };
        // REPL modules must not leak into the shared registries.
        assert!(ctx.typecheck_module_registry.get(&root_index).is_none());
        assert!(ctx.interpret_module_registry.get(&root_index).is_none());
    }

    #[test]
    fn import_relative_to_importing_module() {
        use std::io::Write;

        let dir = tempfile::tempdir().unwrap();
        let dir_path = dir.path();

        // Create a nested module tree:
        //   main.lox imports lib/a.lox
        //   lib/a.lox imports sub/b.lox (relative to a.lox's directory)
        let lib_dir = dir_path.join("lib");
        let sub_dir = lib_dir.join("sub");
        std::fs::create_dir_all(&sub_dir).unwrap();

        let main_path = dir_path.join("main.lox");
        let a_path = lib_dir.join("a.lox");
        let b_path = sub_dir.join("b.lox");

        let mut f = std::fs::File::create(&main_path).unwrap();
        writeln!(f, "import \"self:lib/a.lox\" as a;").unwrap();
        writeln!(f, "assert(a::x == 42, \"nested import\");").unwrap();
        drop(f);

        let mut f = std::fs::File::create(&a_path).unwrap();
        writeln!(f, "import \"self:sub/b.lox\" as b;").unwrap();
        writeln!(f, "var x: number = b::value;").unwrap();
        drop(f);

        let mut f = std::fs::File::create(&b_path).unwrap();
        writeln!(f, "var value: number = 42;").unwrap();
        drop(f);

        let mut ctx = RunnerContext::new(true);
        // run_file uses source_name = the file path, so parse_module_tree
        // resolves relative imports from the root module's directory.
        ctx.run_stmt(
            &std::fs::read_to_string(&main_path).unwrap(),
            main_path.to_str().unwrap(),
        )
        .unwrap();
    }

    #[test]
    fn same_module_imported_twice_parsed_once() {
        use std::io::Write;

        let dir = tempfile::tempdir().unwrap();
        let dir_path = dir.path();

        let mods_dir = dir_path.join("mods");
        std::fs::create_dir_all(&mods_dir).unwrap();

        let util_path = mods_dir.join("util.lox");
        let mut f = std::fs::File::create(&util_path).unwrap();
        writeln!(f, "var x: number = 42;").unwrap();
        drop(f);

        let main_path = dir_path.join("main.lox");
        let mut f = std::fs::File::create(&main_path).unwrap();
        // Import the same file via two different relative paths.
        writeln!(f, "import \"self:mods/util.lox\" as util;").unwrap();
        writeln!(f, "import \"self:mods/../mods/util.lox\" as util2;").unwrap();
        writeln!(f, "assert(util::x == 42, \"dedup import\");").unwrap();
        drop(f);

        let mut ctx = RunnerContext::new(true);
        ctx.run_stmt(
            &std::fs::read_to_string(&main_path).unwrap(),
            main_path.to_str().unwrap(),
        )
        .unwrap();

        // Both imports resolve to the same canonical path → parse_cache has 2 entries (main + util), not 3.
        assert_eq!(
            ctx.parse_cache.len(),
            2,
            "parse_cache should have main + util, not a duplicate"
        );
    }
}
