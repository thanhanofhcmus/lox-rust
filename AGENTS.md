# AGENTS.md

Project overview and conventions for AI coding agents working in this repo.

## Project

**lox-rust** — a Lox language interpreter written in Rust (edition 2024). Based on [Crafting Interpreters](https://craftinginterpreters.com/) with author extensions (gradual typing, structs, tuples, modules, etc.). Tree-walking interpreter (no bytecode VM yet).

## Build & Run

```bash
cargo build --release              # build
cargo check                        # type-check without codegen (fast)
cargo clippy                       # lint
cargo test                         # run all tests
./target/release/lox-rust -i       # REPL
./target/release/lox-rust -p '...' # single statement
./target/release/lox-rust -f file  # run file
```

## Architecture: 4-Phase Pipeline

```
Source text → Lex (tokens) → Parse (AST<()>) → Typecheck (AST<TypeId>) → Interpret
```

1. **Lex** (`src/parse/lex.rs`) — tokenizes source into `Token` stream. Handles raw strings, escapes, number literals.
2. **Parse** (`src/parse/parser.rs`) — recursive descent parser. Produces `AST<()>` (untyped AST). Also resolves import identities and populates the `ModuleStringInterner`.
3. **Typecheck** (`src/typecheck/typechecker.rs`) — gradual type system with `any` as top type. Converts `AST<()>` → `AST<TypeId>`. Checks struct field types, function arity, tuple destructuring, etc.
4. **Interpret** (`src/interpret/interpreter.rs`) — tree-walking interpreter. Executes the typed AST. All values are garbage-collected via mark-sweep.

The pipeline is orchestrated in `src/runner.rs` (`RunnerContext`), which also handles module DAG resolution (transitive import discovery, cycle detection, leaf-first order).

## Source Layout

| Path | Purpose |
|------|---------|
| `src/main.rs` | CLI entry point, parses args via `cli.rs` |
| `src/cli.rs` | Argument parsing (REPL, prompt, file modes) |
| `src/runner.rs` | Orchestrates the full pipeline per input source; manages module DAG, caches, registries |
| `src/token.rs` | `Token` enum — all language tokens |
| `src/span.rs` | `Span` — byte-offset range into source, with display helpers |
| `src/ast.rs` | `AST`, `Statement`, `Expression`, `ClauseNode` and related node types |
| `src/id.rs` | `Id` — interned identifier handle (from `IdentifierRegistry`) |
| `src/identifier_registry.rs` | `IdentifierRegistry` — global interner for identifiers |
| `src/string_interner.rs` | `StringInterner` — general-purpose string interner with GC mark-sweep for runtime strings |
| `src/types.rs` | `TypeId` (tagged usize by category), `Type` enum, `TypeInterner`, `StructType`, `TypeScope` |
| `src/type_index.rs` | `define_type_index!` macro — newtype wrapper for DAG node indices |
| `src/parse/mod.rs` | Re-exports `lex` and `parse` |
| `src/parse/lex.rs` | Lexer |
| `src/parse/parser.rs` | Recursive descent parser |
| `src/parse/error.rs` | `ParseError` with user-facing error rendering |
| `src/parse/context.rs` | Parser context/state |
| `src/typecheck/mod.rs` | Re-exports |
| `src/typecheck/typechecker.rs` | Type checker |
| `src/typecheck/environment.rs` | Type-level scopes and module registry |
| `src/typecheck/error.rs` | `TypecheckError` with user-facing error rendering |
| `src/interpret/mod.rs` | Re-exports |
| `src/interpret/interpreter.rs` | Tree-walking interpreter |
| `src/interpret/environment.rs` | Runtime scopes, module registry, `deinit_module` |
| `src/interpret/heap.rs` | GC heap — mark-sweep for `Value` objects (arrays, maps, tuples, structs, closures) |
| `src/interpret/error.rs` | `InterpretError` with runtime value rendering |
| `src/interpret/prelude.rs` | Built-in functions (`print`, `assert`, `array_*`, `map_*`, `to_json`, `from_json`) |
| `src/interpret/debug_string.rs` | Debug string rendering for interpreter internals |
| `src/interpret/values/mod.rs` | `Value`, `GcObject` (Array, Map, Tuple, Struct, Closure) |
| `src/interpret/values/value.rs` | Core `Value` type |
| `src/interpret/values/scalar.rs` | Scalar value types |
| `src/interpret/values/number.rs` | Number type (integer vs float logic) |
| `src/interpret/values/serial.rs` | JSON serialization |
| `src/interpret/values/display_writer.rs` | `DisplayWriter` for runtime value display |
| `src/module.rs` | `ModuleIdentity`, `ModuleStringInterner` |
| `src/std_module.rs` | Standard library modules (pre-populated in registries) |
| `src/dag.rs` | Generic DAG with cycle detection, transitive reduction, leaf-first ordering |
| `src/input_source.rs` | `InputSource` enum — Repl, Prompt, File variants |
| `src/lib.rs` | Module declarations |

## Key Types

- **`AST<T>`** — parameterized AST; `T=()` means untyped, `T=TypeId` means typed. Two-phase transformation during typecheck.
- **`TypeId`** — tagged `usize`: top 4 bits = category (scalar/array/map/tuple/function/struct), lower bits = index within category. Not a `define_type_index!` type.
- **`Value`** — runtime value. `Copy` (currently). Wraps scalars directly, heap objects via `GcObjectId`.
- **`GcObject`** — heap-allocated: `Array`, `Map`, `Tuple`, `Struct`, `Closure`. All managed by mark-sweep GC in `heap.rs`.
- **`Id`** — interned identifier handle (from `IdentifierRegistry`).
- **`Span`** — byte range `(start, end)` into source text.
- **`DAG<N, I>`** — generic directed acyclic graph with node data `N` and index type `I`.

## Testing

Tests live in `tests/`. Fixture-based e2e tests in `tests/fixtures/`.

```
tests/
├── common/mod.rs     # run_fixture(), assert_ok(), assert_err_contains()
├── e2e.rs            # runs all fixtures in tests/fixtures/
└── fixtures/
    ├── 01_scalars.lox
    ├── ...
    ├── 22_import_struct.lox
    ├── modules/      # support modules for import tests
    └── errors/       # negative tests (expected errors)
```

Each `.lox` fixture is a self-checking program loaded by `run_fixture()` and run with `--strict-assert`. Failed `assert(...)` calls become `InterpretError::AssertionFailed`. The `common/mod.rs` helpers (`assert_ok`, `assert_err_contains`) check results.

To add a new fixture, create a `.lox` file and add a test function in `tests/e2e.rs`.

Unit tests live inline in `src/` modules (e.g. `runner.rs` has `#[cfg(test)] mod tests`).

## Code Conventions

- **Edition 2024** — uses new `unsafe` keyword rules, `impl Trait` in more positions, etc.
- **rustfmt**: `max_width = 120`
- **Error handling**: `thiserror` for error types; each pipeline phase has its own error type (`ParseError`, `TypecheckError`, `InterpretError`) with a `generate_user_facing_error()` method for pretty-printing with source spans.
- **Interning**: identifiers go through `IdentifierRegistry`; general strings through `StringInterner` or `ModuleStringInterner`.
- **Two-phase AST**: parse produces `AST<()>`, typecheck converts to `AST<TypeId>`. This is the standard pattern — always work with the right phase.
- **Module DAG**: imports are discovered transitively, topologically sorted leaf-first, and processed in order. Every phase (parse, typecheck, interpret) processes modules in dependency order.
- **`define_type_index!` macro** (`src/type_index.rs`): creates newtype wrappers for index types used in DAG/arena patterns (e.g. `ModuleDagId`).

## Key Known Issues (from docs/todos.md)

- `Value` is `Copy` but ref-counting is vestigial — GC uses mark-sweep but RC counts are inconsistent. Decide: full RC or full mark-sweep.
- Reassignment doesn't typecheck the RHS against the declared type.
- Variadic functions skip argument type validation.
- No external package support (`std:` and `thirdparty:` parsed but not loaded).
- Chained module access (`a::b::c`) not supported.
- No GC trigger — only manual `_dbg_gc_mark_sweep()` reclaims memory.
- Struct fields use `Vec`; display/JSON field order is insertion order, not alphabetical or canonical.

## Agent Discipline

- **Never auto-commit** — commits must be explicitly initiated by the user. Never run `git commit` unless asked.
- **All tests must pass before calling work done** — run `cargo test` and ensure everything passes before considering a task complete.
- **Regression tests for bugs** — when fixing a bug, always write a test that reproduces it first (red → green). This prevents regressions.
- **Pre-commit hygiene** — before committing, always run in order:
  ```bash
  cargo fmt
  cargo check
  cargo clippy
  ```
- **Keep AGENTS.md up to date** — if the session introduces new conventions, architectural changes, or discovers patterns worth documenting, update `AGENTS.md` before wrapping up.

