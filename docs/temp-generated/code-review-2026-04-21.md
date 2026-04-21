# Code Review: lox-rust

_Date: 2026-04-21_

A tree-walking Lox interpreter in Rust (~9.5K LOC) with a clear pipeline: `lex → parse → typecheck → interpret`. Extends Lox with blocks-as-expressions, `when`/`case`, `for`-comprehensions, map literals, gradual types, structs, modules, and JSON builtins. Build is clean (1 clippy warning) and 214 tests pass.

---

## Strengths

- **Clean pipeline separation**: each stage is isolated, uses its own `Environment`, and the AST is generic over `T` (`()` → `TypeId`) which is a nice way to make "typed vs untyped AST" a type-level distinction (`ast.rs:5`).
- **Pratt parser** is compact and correct, with a clever single-enum ordering to model left/right binding power for associativity (`parser.rs:316-349`).
- **Unit tests**: lexer, parser, typechecker, span, id, scalar, number, and values all have focused tests. Good use of `#[should_panic]` to pin known buggy behavior (e.g. `span.rs:122-132`).
- **Gradual typing** is consistent: `unify_types` / `require_type` treat `Any` permissively; typechecker defers to runtime for `Any`-typed indexing and calls (`typechecker.rs:632, 665`).
- **User-facing errors** are well-formatted with file:line:col, code snippet, and caret pointer (`parse/error.rs:53-83`, `typecheck/error.rs:63-87`).
- **Doc comments** on many non-obvious helpers (`unify_types`, `require_type`, `with_scope`, `shallow_copy_value`).

---

## Correctness issues

### Crashes on valid-looking input

- **`parser.rs:379`** — `Token::Dot => unimplemented!()`. Any expression with `.` (module member access, field access, `.5`) panics the whole process. Per the TODO at `typechecker` it's acknowledged but reachable.
- **`interpreter.rs:365`** — `RawValueNode::StructLiteral => unimplemented!()`. Parser and typechecker both accept struct literals; runtime panics. `Point { x = 1, y = 2 };` compiles cleanly and crashes. (Implementation is planned per ADR-009; status is "parsed+checked, not executed.")
- **`interpreter.rs:83-85`** — `Statement::StructDecl` is a no-op at runtime. Combined with the above, structs are dead-on-arrival.
- **`environment.rs:203`** — `assert!(self.scope_stack.len() == 1)` in `deinit_module`. If a module leaves extra scopes (e.g., a parse error mid-block), the process panics instead of reporting an error.

### Logic bugs

- **Variadic arg check is dead (typechecker.rs:688-724)**. The code has an in-tree TODO admitting it:
  ```rust
  if variadict_type_id.is_none() {
      if variadict_type_id.is_none() && param_type_ids.len() != args.len() { ... }
      // ... arg type check ...
  }
  ```
  For any variadic function (e.g., `print`, `to_json`, `array_push`), all arg count/type validation is skipped. `print(1, 2, 3)` typechecks even though the variadict element type is `STR`.

- **`typechecker.rs:553-559`** — `.declare_id(iden.id, ANY_FUNCTION).unwrap()` for the current declaration's function name. The in-tree TODO says this panics on duplicate; in practice the surrounding `with_scope` masks it, but the code path still has no real error branch. The analogous param loop (line 562-565) uses `.map_err(DuplicateDeclaration)`.

- **`convert_struct_literal`** (`typechecker.rs:478-533`) doesn't detect **duplicate field names** in a literal. `Point { x = 1, x = 2 }` passes the count check and both entries validate against the same declared `x` field. The interpreter path will see both when implemented.

- **Typechecker never registers imported names.** `main.rs:177`/`interpreter.rs:62` call `typechecker.convert(ast)` without feeding import identifiers into the typecheck environment, so `import "foo" as foo; foo;` fails with `UndefinedIdentifier` at typecheck time. (Moot while `Token::Dot` is `unimplemented!()`, but the asymmetry will surface once dot-access is implemented.)

### Memory / GC soundness

- **Ref counting is effectively vestigial.** `HeapEntry::new` starts at `ref_count = 1`; `insert_variable_current_scope` bumps to 2; `pop_scope` decrements to 1 — never to 0. Heap objects **never** reach 0 via the RC path, so their slots accumulate until someone calls the `_dbg_gc_mark_sweep` builtin.
- **There is no automatic GC trigger.** The project memory notes a trigger at ≥100 used objects, but `rg` finds no such threshold in the code. `mark`/`sweep` are only wired to `_dbg_*` builtins (`predule.rs:101-125`). A long-running program leaks until the user manually runs `_dbg_gc_mark_sweep()`.
- **`heap.rs:371`** — `entry.ref_count.sub_assign(1)` has no saturation. If an invariant breaks and it reaches 0, this panics in debug and wraps to `usize::MAX` in release. Use `checked_sub`/`saturating_sub` or return an error.
- **`heap.rs:296`** TODO: `deep_copy_reassign_object` doesn't dispose intermediate objects replaced during a chained assignment. `a[0][1] = x` leaks the old copies of `a[0]`.
- **`environment.rs:212`** TODO: `deinit_module` transfers the scope vars directly into the module map **without** touching ref counts. Every heap value held by any module leaks permanently.
- **Collection inserts don't bump contained refs.** `insert_array_variable(vec)` creates a new heap entry with `ref_count=1` but does not `shallow_copy_value` the contained Values. Meanwhile `array_push_fn` / `map_insert_fn` / `array_insert_fn` do bump refs. The policy is inconsistent — array/map literals and array-push have different invariants.

### Edge cases

- **`span.rs:35-53`** — `to_start_row_col` has a documented off-by-one after newlines (pinned in tests at `row_col_right_after_newline_has_col_zero`). User-facing errors therefore point one column too early on non-first lines. Also uses byte indices, so columns will be wrong for multi-byte UTF-8.
- **`id.rs:13-17`** — `Id` is a 64-bit `DefaultHasher` output. Silent aliasing on hash collision (two different names become equal). Birthday-bound collision probability is low (~50% at 2^32 distinct names), but any collision is a silent wrong-binding bug, not a compile error. Consider `(hash, StrId)` or interning.
- **`main.rs`** uses `.expect("must have one argument")` and `panic!("expect a mode")` — CLI misuse crashes with Rust stacktraces rather than a helpful error.
- **`interpret_when_expr`** (`interpreter.rs:579-589`) falls through to `Value::Unit` when no arm matches, but the typechecker unifies only the arm expressions (`typechecker.rs:186-190`). A `when` whose typechecker thinks returns `Number` can return `Unit` at runtime.
- **Parser recursion isn't depth-limited.** Scope depth is limited to `SCOPE_SIZE_LIMIT = 100` in the interpreter, but `parse_pratt` has no such guard — deeply nested input (e.g. `((((...))))`) can stack-overflow before reaching the interpreter.
- **`lex_comment`** includes the trailing `\n` in the span. Minor but the token's source-slice will carry the newline.
- **`string_utils.rs:12`** `panic!` on trailing `\` — relies on lexer to reject. The lexer's `lex_string` check (`parse/lex.rs:217`) uses `>` where `>=` would be more obviously correct; current behavior still reports `UnclosedString` via fallthrough but is subtle.

---

## Design notes

- **`insert_variable_current_scope` + `shallow_copy_value` asymmetry** (`environment.rs:259-270`): the helper both inserts AND bumps the ref, so callers that forget bump-on-insert (e.g., returning a fresh heap value from a builtin straight into a `Vec<Value>`) produce different invariants. A stricter type like `OwnedValue` / `BorrowedValue` would surface this.
- **`interpret_normal_fn_call_expr:494`** clones the entire `Function { params, body }` on every call. `Function` in the heap should be `Rc<Function>` so calls borrow. Currently a recursive call clones the body per invocation — quadratic in the presence of deep `for` loops.
- **`interpret_fn_decl:456-461`** clones params and body on every evaluation of a fn literal (e.g., every loop iteration). Share via `Rc`.
- **`ValueReturn`** (`interpreter.rs:14-50`) is a hand-rolled exception mechanism. Could be expressed directly as `Result<Value, ReturnOrError>` / `ControlFlow<Value>`.
- **AST has both `Expression` and `ClauseNode`**, each with `extra: T`. The distinction (statement-ish vs. value-ish) isn't surfaced in names. Either split types by role (`ExprStmt` vs. `ValueExpr`) or merge them — the current layout makes the parser and interpreter duplicate "consume semicolon for Clause/Return" logic (`parser.rs:36-42, 149-155`).
- **Token names**: `LPointParen`/`RPointParen` for `{}` (most call these curly braces), `Percentage` for `%`, `PercentLPointParent` for `%{`, `RFArrtow` for `=>` (typo). Worth renaming before any external consumer (tree-sitter grammar) depends on them.
- **Hash map ordering in tests**: `debug_string` sorts string-interner entries but `debug_state_string` does not sort scope variables or modules — test output for those will be non-deterministic across runs.

---

## Code quality / typos

- **File/module name**: `interpret/predule.rs` → `prelude` (affects module declaration in `interpret/mod.rs` too).
- **Spellings**: `variadict` → `variadic` (throughout `types.rs`, `typecheck/environment.rs`, `typechecker.rs`), `RFArrtow` → `RFatArrow`, `LAST_RESVERED_COUNTER` → `LAST_RESERVED`, `Unclosesed` (`string_utils.rs:12`), `intepret_builtin_fn_call_expr` (`interpreter.rs:522`), `ArrayForComprehention` → `...hension`, "horable horable cheat" (`typechecker.rs:11`), "canot perform operation" (`error.rs:33`), "hance not indexable" (`error.rs:54`), "is is does not exists" (`error.rs:105`), "defered" (`typechecker.rs:616, 631`), `promt` → `prompt` (`main.rs:40`).
- **Dead/unreachable**: `convert_struct_literal`'s `Type::Any` arm (`typechecker.rs:528`) can't be reached given `lookup_type_id_from_id` is only populated by struct decl, so the prior `ok_or(UndefinedIdentifier)` fires first.
- **Clippy warning**: `values/value.rs:105` — `return Err(...)` inside a match arm should drop the `return`.
- **`main.rs:22`** prints dotenv errors via `println!` rather than through the logger that's about to be initialized on line 24.

---

## Test coverage

Strong where it exists:
- ✅ Lexer, Pratt parser, binding power, typechecker unification, Number/Scalar/Value primitives, string interner, span (including pinned off-by-ones), id hashing.
- ❌ **No end-to-end interpreter tests.** `interpreter.rs` has no `#[cfg(test)]`. Verifying that "evaluate program X → produces output Y" is entirely manual (REPL / file).
- ❌ **No heap/GC tests.** Given the RC/MS complexity and several known TODOs, the lack of tests here is a risk. A unit test that creates `var a = [1,2]; var b = a;` and asserts ref-count expectations would catch the "leak by 1" invariant.
- ❌ **No module/import tests.** Imports do file I/O and recursive typecheck+interpret; no integration coverage.
- ❌ No tests for string comparison ordering, `for` comprehension filter, `when` fall-through, deep reassignment (`a[0][1] = x`).

---

## Top recommendations (ordered by value / risk)

1. **Guard the panics that escape to end users**: replace `unimplemented!()` in `parser.rs:379` and `interpreter.rs:365` with proper errors (or implement them). Replace `assert!` in `deinit_module` and `.unwrap()` in `typechecker.rs:558`.
2. **Fix the variadic arg check** (`typechecker.rs:688-724`) — split into variadic and fixed paths so `print`/`to_json`/array builtins actually validate their arguments.
3. **Decide the memory story**: either (a) make RC correct (start `ref_count = 0`, bump on every "reference", fix collection inserts), or (b) drop RC and lean fully on mark-sweep with an automatic trigger. Current hybrid leaks by design.
4. **Add end-to-end interpreter tests** — a small harness that runs source → prints → asserts. Plus heap invariant tests. This is the biggest safety net missing.
5. **Address ref-counting safety hole**: replace `sub_assign(1)` with `checked_sub(1)` (`heap.rs:371`) and return an error / surface an internal invariant violation rather than panicking.
6. **Parser recursion guard** — mirror the scope-depth guard on `parse_pratt` to avoid stack overflow on pathological input.
7. **Batch-fix typos** (`variadict`, `predule`, `RFArrtow`, etc.) before any external consumer pins these names.
8. **Small perf wins**: wrap `Function` in `Rc` to eliminate body clones per call; avoid cloning args vec in `for` loop iteration.

Overall the codebase is coherent and readable with a thoughtful type system and solid parsing. The biggest risks are the partially-implemented struct runtime, the silently-broken variadic type-check, and the memory management mismatch between an RC intent and the actual code behavior.
