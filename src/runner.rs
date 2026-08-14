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
    input_source::{InputSource, normalize_path, resolve_relative_path},
    interpret::{self, Heap, InterpretError},
    module::{ModuleIdentity, ModuleMetadata, ModuleStringInterner},
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
    #[error("external packages are not yet supported: {0}")]
    UnsupportedPackage(String),
    #[error("failed to read module: {0}")]
    ModuleReadError(#[from] std::io::Error),
    #[error("internal error: invalid module graph edge")]
    ModuleGraphEdge,
}

pub type RunResult = Result<(), RunError>;
pub type DynResult = Result<(), Box<dyn std::error::Error>>;

struct PendingModule<Ty> {
    /// Kept for diagnostics only;
    /// the typecheck and interpret stages render their errors against it.
    source: InputSource,
    is_persistent: bool,
    ast: AST<Ty>,
}

/// Everything this run has to build, leaf-first: a module always comes after
/// every module it imports. Cache hits — std modules and modules an earlier run
/// already compiled — are not in here at all; they are filtered out once, when
/// the tree is flattened out of the DAG.
type ModuleTree<Ty> = Vec<(ModuleIdentity, PendingModule<Ty>)>;

/// Encapsulates the runtime environment to avoid duplicating setup across modes.
pub struct RunnerContext {
    global_identifier_registry: IdentifierRegistry,

    typecheck_module_registry: typecheck::ModuleRegistry,
    interpret_module_registry: interpret::ModuleRegistry,

    type_interner: TypeInterner,
    module_string_interner: ModuleStringInterner,
    interpret_heap: interpret::Heap,

    // The REPL / prompt working modules. Not part of the module cache
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

            repl_typecheck_module: typecheck::Module::default(),
            repl_interpreter_module: interpret::Module::default(),

            strict_assert,
        }
    }

    fn is_compiled(&self, identity: &ModuleIdentity) -> bool {
        identity.is_std || self.interpret_module_registry.get(identity).is_some()
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
        trace!("{:?}", ast);

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
            // Absolute, normalized path for file mode, so the root module's
            // identity matches the one an import of the same file resolves to.
            let abs = std::path::absolute(Path::new(source_name)).unwrap_or_else(|_| PathBuf::from(source_name));
            InputSource::File(normalize_path(&abs))
        };

        let tree = self.discover_module_tree(source_name, &root_source)?;

        // Typecheck the whole tree before running any of it, so a type error in
        // one module is never reported after another module's top level has already had side effects.
        let tree = match self.typecheck_module_tree(tree) {
            Ok(tree) => tree,
            Err(e) => {
                if is_in_repl {
                    self.repl_typecheck_module = saved_tc;
                }
                return Err(e);
            }
        };

        match self.interpret_module_tree(tree) {
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

    /// Resolve the imports of an already-parsed module: stamp each
    /// `ImportNode` with its identity, add the DAG edge, and queue any
    /// non-std module for parsing.
    fn resolve_module_imports(
        &mut self,
        ast: &mut AST<()>,
        importer_dir: &Path,
        module_node_id: ModuleDagId,
        module_dag: &mut DAG<ModuleIdentity, ModuleDagId>,
        import_queue: &mut Vec<ModuleDagId>,
    ) -> RunResult {
        for imp in &mut ast.imports {
            let identity = self.resolve_import_identity(&imp.metadata, importer_dir)?;

            imp.identity = Some(identity);
            let imported_node_id = module_dag.add_node(identity);

            module_dag
                .add_edge(module_node_id, imported_node_id)
                .map_err(|_| RunError::ModuleGraphEdge)?;
            if !self.is_compiled(&identity) {
                import_queue.push(imported_node_id);
            }
        }
        Ok(())
    }

    /// Parse the root module, discover + parse every module it imports that is
    /// not already compiled, reject cycles, and return the leaf-first work list.
    ///
    /// The DAG is local to this function: once the tree is ordered and
    /// flattened, nothing downstream needs the graph.
    fn discover_module_tree(
        &mut self,
        source_name: &str,
        root_source: &InputSource,
    ) -> Result<ModuleTree<()>, RunError> {
        let mut module_dag = DAG::new();
        // Doubles as the visited set while the graph is being walked.
        let mut discovered: HashMap<ModuleIdentity, PendingModule<()>> = HashMap::new();
        let mut import_queue: Vec<ModuleDagId> = vec![];

        let mut ast = self.lex_and_parse(root_source)?;

        // For a file, the root's identity is its absolute normalized path —
        // the same spelling `resolve_import_identity` produces — so the root
        // and an import of that same file are one module, not two.
        // REPL/prompt sources have no path and keep their synthetic name.
        let root_path = match root_source.path() {
            Some(path) => path.to_string_lossy().into_owned(),
            None => source_name.to_string(),
        };

        let root_identity = ModuleIdentity {
            resolved_path: self.module_string_interner.intern(&root_path),
            is_std: false,
        };

        let root_identity_node_id = module_dag.add_node(root_identity);

        // Resolve each import relative to the importing module's directory.
        let importer_dir = Path::new(&root_path).parent().unwrap_or(Path::new(".")).to_path_buf();

        self.resolve_module_imports(
            &mut ast,
            &importer_dir,
            root_identity_node_id,
            &mut module_dag,
            &mut import_queue,
        )?;

        discovered.insert(
            root_identity,
            PendingModule {
                source: root_source.clone(), // TODO: remove clone
                is_persistent: root_source.is_persistent(),
                ast,
            },
        );

        // Discover and parse all imported modules (depth-first)
        while let Some(current_node_id) = import_queue.pop() {
            let current_identity = module_dag
                .get_node(current_node_id)
                .expect("import queue should only contain valid DAG node ids")
                .data;

            if discovered.contains_key(&current_identity) {
                continue;
            }

            let path = self
                .module_string_interner
                .get(current_identity.resolved_path)
                .expect("import path id should have been interned during parsing")
                .to_string();

            // TODO: move file loader to an interface
            // `resolve_import_identity` already made this absolute and normalized.
            let file_path = PathBuf::from(&path);
            if !file_path.is_file() {
                error!("Module not found: {}", path);
                return Err(RunError::ModuleNotFound(path));
            }

            let importer_dir = file_path.parent().unwrap_or(Path::new(".")).to_path_buf();
            let file_source = InputSource::File(file_path);

            let mut untyped_ast = self.lex_and_parse(&file_source)?;

            self.resolve_module_imports(
                &mut untyped_ast,
                &importer_dir,
                current_node_id,
                &mut module_dag,
                &mut import_queue,
            )?;

            discovered.insert(
                current_identity,
                PendingModule {
                    source: file_source,
                    is_persistent: true,
                    ast: untyped_ast,
                },
            );
        }

        if module_dag.has_cycle() {
            error!("Circular import detected");
            // Nothing was cached: `discovered` dies with this run and no module
            // reached the registries. A later import of the same module walks
            // the graph again and reports the cycle again, instead of finding a
            // cached-but-edgeless module and silently accepting it.
            return Err(RunError::CircularImport);
        }
        module_dag.transitive_reduce();

        // Flatten the graph into the work list. `remove_entry` yields nothing
        // for a node that was never queued — a std module, or one an earlier run
        // already compiled — so the cache-hit filter is applied once, here,
        // rather than being re-derived by every later stage.
        let tree = module_dag
            .get_leaf_first_order()
            .into_iter()
            .filter_map(|node_id| {
                let identity = &module_dag
                    .get_node(node_id)
                    .expect("leaf-first order should only contain valid DAG node ids")
                    .data;
                discovered.remove_entry(identity)
            })
            .collect();

        Ok(tree)
    }

    /// Typecheck the whole tree, leaf-first, so a module's imports are in the
    /// typecheck registry by the time it is checked.
    fn typecheck_module_tree(&mut self, tree: ModuleTree<()>) -> Result<ModuleTree<TypeId>, RunError> {
        let mut typed_tree = Vec::with_capacity(tree.len());

        for (
            identity,
            PendingModule {
                source,
                is_persistent,
                ast,
            },
        ) in tree
        {
            let (typed_ast, module) = self.type_check(ast, &source)?;

            if is_persistent {
                self.typecheck_module_registry.insert(identity, module);
            } else {
                self.repl_typecheck_module = module;
            }

            typed_tree.push((
                identity,
                PendingModule {
                    source,
                    is_persistent,
                    ast: typed_ast,
                },
            ));
        }

        Ok(typed_tree)
    }

    /// Run the whole tree, leaf-first, so a module's imports have run before it
    /// does. Only reachable with a `ModuleTree<TypeId>`, i.e. after typechecking.
    fn interpret_module_tree(&mut self, tree: ModuleTree<TypeId>) -> RunResult {
        for (
            identity,
            PendingModule {
                source,
                is_persistent,
                ast,
            },
        ) in tree
        {
            let module = self.interpret(ast, &source)?;

            if is_persistent {
                self.interpret_module_registry.insert(identity, module);
            } else {
                self.repl_interpreter_module = module;
            }
        }

        Ok(())
    }

    /// Resolve the identity of an imported module, handling std virtual modules.
    fn resolve_import_identity(
        &mut self,
        metadata: &ModuleMetadata,
        importer_dir: &Path,
    ) -> Result<ModuleIdentity, RunError> {
        let rel_path = self
            .module_string_interner
            .get(metadata.path)
            .expect("path should be interned");

        if metadata.package == crate::id::Id::STD {
            return Ok(ModuleIdentity {
                resolved_path: self.module_string_interner.intern(&format!("std:{}", rel_path)),
                is_std: true,
            });
        }

        // Only `self:` and `std:` exist today. Without this check any other
        // package silently falls through to relative-path resolution and either
        // reports a bare "module not found" or loads an unrelated local file.
        if metadata.package != crate::id::Id::SELF {
            let spelling = format!(
                "{}:{}",
                self.global_identifier_registry.get_or_unknown(metadata.package),
                rel_path
            );
            error!("External packages are not yet supported: {}", spelling);
            return Err(RunError::UnsupportedPackage(spelling));
        }

        let resolved = resolve_relative_path(importer_dir, rel_path);
        Ok(ModuleIdentity {
            resolved_path: self.module_string_interner.intern(&resolved),
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

    #[test]
    fn repl_import_persists_across_lines() {
        let mut ctx = RunnerContext::new(true);

        // Import a module on one line…
        ctx.run_stmt("import \"self:tests/fixtures/modules/math.lox\" as math;", REPL_LINE)
            .unwrap();

        // …and use it on a later line. Before the fix that made `Module`
        // carry `imported_modules`, this line failed to resolve `math`
        // because the import information was dropped by `make_module`.
        ctx.run_stmt(
            "assert(math::add(1, 2) == 3, \"import works across REPL lines\");",
            REPL_LINE,
        )
        .unwrap();
    }

    #[test]
    fn repl_imported_struct_type_persists_across_lines() {
        let mut ctx = RunnerContext::new(true);

        // Import a module that exports a struct, then construct the struct on
        // a later line. This exercises the *typecheck* import-info round-trip
        // (struct_scope lookups via imported_modules), not just interpretation.
        ctx.run_stmt("import \"self:tests/fixtures/modules/geometry.lox\" as geo;", REPL_LINE)
            .unwrap();

        ctx.run_stmt(
            "var p = geo::Point { x = 3, y = 4 }; assert(p.x == 3, \"imported struct across REPL lines\");",
            REPL_LINE,
        )
        .unwrap();
    }

    /// The identity a `self:`-import of `rel_path` resolves to.
    fn identity_of(ctx: &mut RunnerContext, rel_path: &str) -> ModuleIdentity {
        let abs = std::path::absolute(Path::new(rel_path)).expect("cwd should be readable");
        ModuleIdentity {
            resolved_path: ctx
                .module_string_interner
                .intern(&normalize_path(&abs).to_string_lossy()),
            is_std: false,
        }
    }

    fn heap_objects(ctx: &RunnerContext) -> usize {
        ctx.interpret_heap.get_stats().number_of_total_objects
    }

    #[test]
    fn module_is_compiled_once_and_reused_by_later_imports() {
        let mut ctx = RunnerContext::new(true);

        ctx.run_stmt("import \"self:tests/fixtures/modules/leaf.lox\" as leaf;", REPL_LINE)
            .unwrap();

        let identity = identity_of(&mut ctx, "tests/fixtures/modules/leaf.lox");
        assert!(ctx.is_compiled(&identity), "leaf.lox should be cached after one import");

        let after_first_import = heap_objects(&ctx);
        assert!(
            after_first_import > 0,
            "leaf.lox must allocate, otherwise the check below proves nothing"
        );

        // A second import is a cache hit: the module must not be re-interpreted,
        // which would run its top level again and allocate a second array.
        ctx.run_stmt("import \"self:tests/fixtures/modules/leaf.lox\" as leaf2;", REPL_LINE)
            .unwrap();
        assert_eq!(
            heap_objects(&ctx),
            after_first_import,
            "re-importing a compiled module re-ran its top level"
        );

        // Both aliases still resolve, and to the same module.
        ctx.run_stmt(
            "assert(leaf::leaf_value == leaf2::leaf_value, \"both aliases name one module\");",
            REPL_LINE,
        )
        .unwrap();
    }

    #[test]
    fn repl_and_prompt_are_never_cached_as_modules() {
        let mut ctx = RunnerContext::new(true);

        ctx.run_stmt("var x: number = 1;", REPL_LINE).unwrap();
        ctx.run_stmt("var y: number = 2;", PROMPT_LINE).unwrap();

        for name in [REPL_LINE, PROMPT_LINE] {
            let identity = ModuleIdentity {
                resolved_path: ctx.module_string_interner.intern(name),
                is_std: false,
            };
            assert!(!ctx.is_compiled(&identity), "{name} must not be cached");
            assert!(
                ctx.typecheck_module_registry.get(&identity).is_none(),
                "{name} must not be registered as a module"
            );
        }
    }

    #[test]
    fn failed_run_does_not_poison_later_imports() {
        let mut ctx = RunnerContext::new(true);

        // The import queue is drained from the back, so `uses_leaf.lox` and its
        // own import are parsed *before* the missing module aborts the run —
        // which is exactly the state that used to be cached and poison the
        // session: `uses_leaf.lox` stayed cached as "parsed" but lost its edge to
        // `leaf.lox`, so every later import of it failed on `leaf::leaf_value`.
        let err = ctx
            .run_stmt(
                "import \"self:tests/fixtures/modules/does_not_exist.lox\" as gone; \
                 import \"self:tests/fixtures/modules/uses_leaf.lox\" as u;",
                REPL_LINE,
            )
            .unwrap_err();
        assert!(matches!(err, RunError::ModuleNotFound(_)), "unexpected error: {err:?}");

        let identity = identity_of(&mut ctx, "tests/fixtures/modules/uses_leaf.lox");
        assert!(
            !ctx.is_compiled(&identity),
            "a module from a failed run must not count as compiled"
        );

        ctx.run_stmt(
            "import \"self:tests/fixtures/modules/uses_leaf.lox\" as u; \
             assert(u::doubled == 14, \"module rebuilt cleanly after a failed run\");",
            REPL_LINE,
        )
        .unwrap();
    }

    #[test]
    fn circular_import_is_detected_on_every_attempt() {
        let mut ctx = RunnerContext::new(true);

        // A cyclic module never reaches the cache, so the cycle must be
        // rediscovered every time rather than masked by a cached parse whose
        // import edges were dropped.
        for attempt in 1..=2 {
            let err = ctx
                .run_stmt("import \"self:tests/fixtures/errors/circular_a.lox\" as a;", REPL_LINE)
                .unwrap_err();
            assert!(
                matches!(err, RunError::CircularImport),
                "attempt {attempt}: unexpected error: {err:?}"
            );
        }
    }
}
