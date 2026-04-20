## Todos:
- Implement dot member access for structs (`p.x`) — parser parses it but runtime panics
- Validate struct field types at construction (typechecker checks struct name exists but not field value types)
- Implement more string stuff, formatted string
- Map comprehension (`%{ k => v for k, v in map }`)
- Map for loop (`for k, v in map { ... }`)
- Rework module
- And standard library module
- More docs
- More test, try fuzzing
- On-demand parsing
- Experimenting the byte code VM
- Add tree-sitter
- Add LSP
- Implement better error messages

## Bugs / Correctness Issues:
- `heap.rs` `update_ref_count`: `ref_count.sub_assign(1)` can underflow (panic in debug, wrap in release) — use saturating subtraction or a checked decrement
- `heap.rs` `deep_copy_reassign_object()`: old value replaced during deep copy is returned and dropped without decrementing its ref count
- `environment.rs` `deinit_module()`: heap not handled — GC objects owned by module-scope variables are not accounted for when a module is finalized
- `environment.rs` `get_variable_all_scope()`: accessing an un-imported module silently returns nil instead of a runtime error
- `environment.rs` `ScopeStack::push()`: scope overflow calls `panic!` instead of returning a recoverable `Error::StackOverflow`

## Design Issues:
- GC trigger removed (was naive: fired on every assignment once live objects exceeded 100); implement adaptive trigger (e.g. double threshold after each collection)
- Module resolution only handles 2-part chains (`module.var`); deeper chains silently misbehave — needs spec and enforcement
- `Any` type is infectious: once a value is typed `Any` all downstream expressions lose static checking. For-loop iteration variables are always `Any` — element type is not propagated from the collection type
- Struct values stored as `Value::Map` at runtime; no enforcement that runtime map contents match declared schema after construction

- [X] Maybe implement node-id
- [X] Make last expression in a block return the value of the block (like rust)
- [X] Add prelude
- [X] Implement `map` data structure
- [X] String escape sequences (`\n`, `\t`, `\r`, `\"`, `\\`)
- [X] Block scoping — blocks now create their own `ScopeKind::Block` scope; variables no longer leak into enclosing scope
- [X] Raw strings — `r"..."` syntax; backslashes are literal, `\"` prevents early close without consuming the backslash
- [X] Multiline string — already supported by lexer; newlines inside `"..."` and `r"..."` are consumed as-is
- [X] For loop — `for x in array { ... }`; loop variable is readonly inside the body
- [X] Array for comprehension — `[for x in array: expr]` with optional `if` filter; loop variable readonly
- [X] Array prelude functions — `array_len`, `array_push`, `array_pop`, `array_insert`
- [X] Map prelude functions — `map_length`, `map_keys`, `map_values`, `map_insert`, `map_remove`
- [X] Add type annotation, type checker — gradual typing with `Any` as top type; two-phase AST (`AST<()>` → `AST<TypeId>`); see ADR 008
- [X] Struct declaration and typecheck — `struct Name { field: Type }` syntax, nominal typing, stored as `Value::Map` at runtime; see ADR 009
- [X] String interner GC — `StringInterner` now participates in mark-and-sweep via `reset_marks()` / `mark_to_keep()` / `sweep()`; memory leak resolved
