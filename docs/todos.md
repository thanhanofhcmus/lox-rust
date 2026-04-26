## Bugs

### Memory & Reference Counting
- [ ] **`Value: Copy` vs ref-counted handles** — assignment/pass-by-value duplicates handles without `shallow_copy_value`, making ref-counts non-authoritative. Either drop `Copy` or commit to mark-sweep and delete the RC scaffolding. *(Foundational — fix first)*
- [ ] **`heap.rs` `update_ref_count`** — `ref_count.sub_assign(1)` can underflow; use saturating or checked sub
- [ ] **`heap.rs` `deep_copy_reassign_object()`** — replaced intermediates dropped without ref-count decrement; chained `a[0][1] = x` leaks each sub-object
- [ ] **`environment.rs` `deinit_module()`** — heap not handled; module-scope GC objects never disposed; ref-counts permanently skewed

### Type Checker
- [ ] **Reassignment rhs type check** — `a = rhs` doesn't verify `rhs`'s type against `a`'s declared type; `var a: number = 1; a = "oops";` passes typecheck
- [ ] **`typechecker.rs` `convert_fn_call`** — skips ALL argument validation for variadic functions; affects `print`, `to_json`, `array_push`, `array_insert`, etc.
- [ ] **`typechecker.rs` `convert_struct_literal`** — `Type::Any` arm is unreachable; prior `ok_or(UndefinedIdentifier)` always fires first

### Modules
- [ ] **`environment.rs` `get_variable()`** — un-imported module access silently returns `nil` instead of erroring

### Collections
- [ ] **`Map = BTreeMap<MapKey, Value>`** — `MapKey::Str` orders by `StrId` (interner insertion order), not string bytes; iteration order non-deterministic across runs. Switch to `IndexMap` or order by actual bytes

### Lexer & Parsing
- [ ] **`string_utils.rs::unescape`** — panics on trailing `\`; tighten or return an error

### Identifiers
- [ ] **`id.rs` `Id`** — uses 64-bit `DefaultHasher` only; silent collisions possible. Pair the hash with a string id

---

## Design Issues

### Memory & GC
- [ ] **GC trigger removed** — only `_dbg_gc_mark_sweep()` reclaims today. Add adaptive trigger (double threshold per collection)
- [ ] **Reference counting is vestigial** — `HeapEntry::new` starts at 1, scope insert bumps to 2, `pop_scope` decrements to 1 (never 0). Bump policy inconsistent across insert vs push/insert operations. Decide: full RC or full mark-sweep

### Runtime Internals
- [ ] **`Function` values cloned per evaluation and per call** — wrap in `Rc<Function>`
- [ ] **`ValueReturn` (`interpreter.rs:14`)** — hand-rolled exception; replace with `Result<_, ReturnOrError>` / `ControlFlow`
- [ ] **`ScopeStack::push()` recursion limit** — hard-coded `SCOPE_SIZE_LIMIT = 100`; recursion beyond that aborts rather than TCO-ing

### AST & Parser
- [ ] **AST has both `Expression` and `ClauseNode`** — distinction isn't named; "consume semicolon for Clause/Return" is duplicated
- [ ] **Token names** — rename before external consumers lock in: `LPointParen`/`RPointParen` for `{}`, `Percentage` for `%`, `PercentLPointParent` for `%{`

### Modules
- [ ] **Module resolution only handles 2-part chains** — deeper chains silently misbehave
- [ ] **No circular-import detection** in `interpret_import_stmt`

---

## TODO

### Type System & Destructuring
- [ ] Struct destructuring — `var Point { x, y } = p;` (shorthand) and `var Point { x = px, y = py } = p;` (renamed). Partial destructuring, `Any` rhs
- [ ] Nested / mixed destructuring — `var %(a, %(b, c)) = …;`, `var %(a, Point { x, y }) = …;`. Queue after struct destructuring
- [ ] Recursive struct fields — `struct Node { next: Node }`. Needs `TypeInterner` reserve/finalize
- [ ] `when` arm typecheck — unify arm types; if any arm has no value, none should

### Functions
- [ ] Mutual recursion between fn declarations — needs a proper per-scope pre-pass

### Structs
- [ ] Struct runtime: `Statement::StructDecl` is still a runtime no-op — revisit if/when struct methods land
- [ ] Struct display / JSON field order — switch runtime `Struct.fields` to `IndexMap` or derive order from `StructType.fields`

### Methods & UFCS
- [ ] Method calls on values — `arr.push(v)`, `m.keys()`. UFCS in general
- [ ] Module member access — `math::sin`. Type-associated calls — `Car::new()`. Both use double-colon infix

### Modules & Imports
- [ ] Import system — `import "std:math.lox" as math;`, `import "self:relative/path"`. Notions of `package`, `self`, `std`
- [ ] Rework module loader — file loader interface, circular-import detection, deeper resolution chains

### Collections
- [ ] Map comprehension — `%{ for %(k, v) in map if k * v == 10 : k => v }`

### Strings
- [ ] Formatted strings
- [ ] JSON deserialisation for structs / tuples — needs schema hint e.g. `from_json("...", Point)`

### Standard Library
- [ ] Standard library module

### Errors & CLI
- [ ] Better error messages — source info pointing exactly to where the error is
- [ ] CLI — show a nice error message when no arguments are given

### Testing & Docs
- [ ] More tests & fuzzing
- [ ] More docs

### Infrastructure & Tooling
- [ ] On-demand parsing
- [ ] Bytecode VM experiment
- [ ] Tree-sitter grammar
- [ ] LSP

---

## Done

### Syntax & Lexer
- [X] Raw strings — `r"..."` syntax; backslashes are literal
- [X] Multiline string — newlines inside `"..."` and `r"..."` consumed as-is
- [X] String escape sequences (`\n`, `\t`, `\r`, `\"`, `\\`)
- [X] Lexer disambiguation for tuple / member access — `p.0.1` lexes as chained tuple indices; hard-errors on ambiguous cases
- [X] `parse/lex.rs` `lex_string` escape-end check — uses `*offset >= input.len()`

### Types & Type Checker
- [X] Type annotation + type checker — gradual typing with `Any` as top type; two-phase AST (`AST<()>` → `AST<TypeId>`)
- [X] Named type annotations — struct names usable as types once declared
- [X] Validate struct literal field types at construction (`StructFieldTypeMismatch`)
- [X] Reject duplicate field names in struct literals (`DuplicateStructLiteralField`)
- [X] `typechecker.rs` duplicate-fn-name unwrap — returns `DuplicateDeclaration`
- [X] Tuple destructuring typecheck — validates tuple kind, arity, and per-member types; `Any` rhs stays permissive

### Structs
- [X] Struct declaration parsing and typecheck — nominal typing, field count / name / type validation
- [X] Struct runtime: literal construction, heap disposal, mark-sweep (including string field values)
- [X] Struct equality in `Value::deep_eq` — nominal, field-count + recursive deep_eq
- [X] JSON serialisation for structs & tuples — `to_json` renders structs as JSON objects, tuples as JSON arrays
- [X] Dot member access (read): `p.x`, chained, mixed with subscription; `Any` passes through to runtime
- [X] Dot member access (write): `p.x = y`, chained, mixed chains
- [X] Dedicated member-access errors — `MemberAccessOnInvalidType` and `GcObjectNotStructOrTuple`

### Tuples
- [X] Tuple syntax — literals `%(a, b)`, type annotations `%(number, str)`, indexed access `x.0`, destructuring in `var` and `for`
- [X] Tuple destructuring runtime errors — `CannotDestructureAsTuple`, `TupleDestructureArityMismatch`
- [X] Tuple / named-type fixture coverage — construction, typing, destructuring, writes, map-for-with-tuple

### Collections
- [X] Implement `map` data structure
- [X] Map `for` iteration — `for %(k, v) in map { ... }` via a `%(key, value)` tuple per entry
- [X] Array for comprehension — `[for x in array: expr]` with optional `if` filter; loop variable readonly
- [X] Array prelude functions — `array_len`, `array_push`, `array_pop`, `array_insert`
- [X] Map prelude functions — `map_length`, `map_keys`, `map_values`, `map_insert`, `map_remove`

### Control Flow & Scoping
- [X] Block scoping — blocks create their own `ScopeKind::Block` scope; variables no longer leak
- [X] For loop — `for x in array { ... }`; loop variable is readonly inside the body
- [X] Make last expression in a block return the value of the block (like Rust)
- [X] Self-binding recursive functions — declared name visible inside its own fn body

### Runtime & Memory
- [X] String interner GC — `reset_marks` / `mark_to_keep` / `sweep`
- [X] Runtime error rendering via live stores — `InterpretError::resolve_description` formats values through `DisplayWriter` + `Environment` + `IdentifierRegistry`
- [X] `environment.rs` `deinit_module()` no longer panics — returns `ModuleScopeStackUnbalanced`
- [X] `span.rs` `to_start_row_col()` — counts Unicode scalar values, resets col on newline

### Prelude & Infrastructure
- [X] Add prelude
- [X] Implement node-id
- [X] Parser recursion depth limit — `MAX_RECURSION_DEPTH = 256` + `RecursionLimitExceeded`

