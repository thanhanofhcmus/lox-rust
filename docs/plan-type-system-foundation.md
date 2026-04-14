# Plan: Type System Foundation

**Date:** 2026-04  
**Status:** Draft — not yet implemented  
**Must be done before:** `plan-struct.md` (the `FnDeclNode` refactor here is Phase 1 of that plan too)

---

## What this plan covers

The minimum structural work needed so that:

- Type annotations can appear in source code and are stored in the AST.
- A separate type checker can walk the AST and report errors.
- All future work (structs, resolution syntax, traits) has a stable AST and type vocabulary to build on.
- The interpreter is **not changed** — it continues to work identically, with or without annotations.

This is **not** a full type system. It is the load-bearing infrastructure that everything else plugs into.

---

## Core principle: gradual typing

Unannotated code gets type `Any`. `Any` is compatible with every other type in both directions — assigning `Any` to a typed variable is allowed, and passing a typed value where `Any` is expected is allowed. This means fully unannotated code produces no type errors and is always valid. Type annotations can be added incrementally.

---

## Two-level type representation

There are two distinct things that should never be mixed:

**`TypeExpr`** — a syntactic node in the AST representing what the programmer wrote. It always has a source `Span` so errors can point to the right place. It is produced by the parser.

**`Type`** — a semantic enum used by the type checker. It has no spans. It is produced by the type checker resolving a `TypeExpr` against the current environment (looking up struct names, expanding aliases, etc.).

The interpreter never sees `Type`. The type checker never produces `TypeExpr`. They share only the AST and the `Id` type.

---

## Part 1 — `TypeExpr`: what goes in the AST

`TypeExpr` needs to cover every kind of type a user can write. For the foundation:

- **Named types**: a bare identifier like `i64`, `bool`, `str`, `nil`, `any`, or a user-defined name like `Vec3`. The token `nil` is a keyword so the parser must accept it specially here.
- **Array types**: `[T]` where `T` is another `TypeExpr`.
- **Map types**: `%{ K => V }` where `K` and `V` are `TypeExpr`. Uses the existing `%{` and `=>` tokens.
- **Function types**: `fn(A, B) -> C` where `A`, `B`, `C` are `TypeExpr`. Uses the existing `fn` and `->` tokens. Note that `fn` in type position has no param names, only types — different from a function declaration.

`TypeExpr` lives in `src/ast.rs`. Each variant should carry the full `Span` of the annotation for error reporting.

---

## Part 2 — Updated AST nodes

### `ParamNode` — replaces bare `IdentifierNode` in function parameters

Currently `FnDeclNode` stores `arg_names: Vec<IdentifierNode>`. This needs to become a richer node that holds: the parameter name (span + id), whether it is `self` (set during impl parsing), and an optional type annotation (`Option<TypeExpr>`).

The optional type annotation is `None` for all existing code. The interpreter only ever reads the name — it never touches the annotation. The type checker reads the annotation.

### Updated `FnDeclNode`

Two additions: params become `Vec<ParamNode>`, and there is a new `Option<TypeExpr>` for the return type. The return type is `None` when `->` is absent. The `RTArrow` token already exists.

### Updated `Statement::Declare`

Add an `Option<TypeExpr>` between the identifier and the expression to support `var x: i64 = 5`. The interpreter ignores it. When absent it is `None`.

---

## Part 3 — Parser changes

### New: `parse_type_expr`

A dedicated function that parses a `TypeExpr`. It is called whenever a type is expected — after `:` in a parameter, after `->` in a function, after `:` in a `var` declaration. It needs to handle all four `TypeExpr` variants. The important edge cases:

- `nil` is `Token::Nil` (a keyword), not `Token::Identifier`. Accept it as a named type.
- A function type `fn(A, B) -> C` starts with `Token::Fn`. Since the parser knows it is in type position (not expression position), there is no ambiguity with a function declaration.
- The `Colon` token is used for type annotations (`x: i64`) and also in array repeat (`[:v:n]`). These never appear in the same context — array repeat is inside `[...]`, type annotations are after a name. No ambiguity.

### Updated: `parse_param_node`

Replaces the simple `get_identifier` closure. Parses either `self` (keyword) or an identifier, then optionally `: TypeExpr`. Returns a `ParamNode`.

### Updated: `parse_function_decl`

Use `parse_param_node` instead of `get_identifier`. After the closing `)`, optionally consume `RTArrow` then `parse_type_expr` for the return type.

### Updated: `parse_declaration`

After consuming the identifier, optionally consume `Colon` then `parse_type_expr` before the `=`.

### New parse error variant

`ExpectedType` — emitted when `parse_type_expr` sees a token that cannot start a type.

---

## Part 4 — Interpreter compatibility

The interpreter's internal `Function` struct holds `arg_ids: Vec<Id>`. When building a `Function` from a `FnDeclNode`, extract just the name ids from each `ParamNode`. The type information is discarded — the interpreter does not use it.

Every place in the interpreter that currently pattern-matches on `FnDeclNode.arg_names` needs to be updated to use `FnDeclNode.params` and read `.name_id` from each. This is mechanical. There should be no behavior change.

Every place that pattern-matches `Statement::Declare(iden, expr)` needs to add a wildcard for the new `Option<TypeExpr>` field.

---

## Part 5 — The `Type` enum

Lives in a new module `src/typecheck/`. The primitive types are:

- `Nil`, `Bool`, `Int` (i64), `Float` (f64), `Str`
- `Array(Type)`, `Map(Type, Type)`, `Fn(Vec<Type>, Type)`
- `Struct(Id)` — for user-defined types; nominal (identity is the name, not the shape)
- `Any` — the gradual typing escape hatch
- `Never` — the bottom type for expressions that never produce a value (a bare `return`, a panic)

**Numeric coercion rule**: `Int` op `Float` produces `Float`. This matches what the runtime already does in `Number`. `Int` and `Float` are not otherwise interchangeable — assigning a literal `5` to a `f64`-annotated variable is a type error unless literal widening is implemented (see open questions).

**Assignability rule**: a type `A` is assignable to type `B` when they are the same type, or when either is `Any`, or when `A` is `Never`. Arrays and maps are covariant. Functions are structurally typed. Structs are nominal (must be the exact same `Id`).

---

## Part 6 — `TypeEnv` — type scope stack

The type checker needs its own scope stack that maps `Id` → `Type`, mirroring how the interpreter's `Environment` maps `Id` → `Value`. It pushes a new scope at block boundaries, pops on exit, and looks up outward through scopes.

It should be seeded with the prelude functions and their types (e.g. `print: fn(any) -> nil`, `array_len: fn([any]) -> i64`, etc.).

This lives in `src/typecheck/` as a separate struct — not in the interpreter's environment.

---

## Part 7 — `TypeError` and error reporting

A type error needs: a `Span` (for pointing to the right source location) and a kind. The minimum error kinds for the foundation:

- **TypeMismatch**: expected type X, got type Y. Used for `var x: T = expr_of_wrong_type`.
- **BinaryOpTypeMismatch**: operator cannot apply to these two types (e.g. `"hello" - 5`).
- **UndefinedVariable**: name not found in type environment.
- **UnknownType**: a named type in an annotation that the type checker doesn't know about.
- **WrongArgCount**: function called with wrong number of arguments.
- **ArgTypeMismatch**: one argument has the wrong type.
- **ReturnTypeMismatch**: body type doesn't match annotated return type.
- **NotCallable**: expression is not a function.
- **NotIndexable**: `[`-subscription on something that isn't an array or map.

Each error should implement `Display` with a human-readable message.

---

## Part 8 — The `TypeChecker`

A struct that owns a `TypeEnv` and a list of collected `TypeError`s. It walks the AST with a family of `check_*` methods that mirror the structure of the AST:

- `check_stmt` dispatches on `Statement` variants
- `check_expr` dispatches on `Expression` variants  
- `check_clause` dispatches on `ClauseNode` variants
- `check_block` pushes/pops a scope, checks each stmt, returns the type of `last_expr`

Each method returns the `Type` of the expression it checked. Errors are collected into the error list — they do **not** short-circuit. The checker tries to continue past errors (using `Any` as the recovery type) to find as many errors as possible in a single pass.

**Key inference rules to implement:**

- Literals produce their primitive type directly.
- `var x = expr` without annotation: `x` gets the inferred type of `expr`.
- `var x: T = expr` with annotation: verify `expr`'s type is assignable to `T`; `x` gets type `T`.
- Identifiers look up the `TypeEnv`.
- Binary operators: arithmetic produces numeric result type per the coercion rules; comparisons produce `Bool`; logical `and`/`or` produce `Bool` if both sides are `Bool`, else `Any`.
- Array literal: infer element type from the first element; verify the rest are compatible; produce `[elem_type]`. If elements are heterogeneous, produce `[any]`.
- Map literal: infer key and value types similarly.
- Function declarations produce a `Fn(param_types, return_type)` type. Check the body in a new scope with params bound. Verify body type against annotated return type if present.
- Function calls: look up the callee type. If `Fn(params, ret)`, verify arg count and arg types. Return `ret`. If `Any`, return `Any`. Otherwise, emit `NotCallable`.

**`return` statement**: should produce type `Never` (the expression never yields a value). The enclosing function body should compare the `return`'d type against the annotated return type if one exists.

---

## Part 9 — Type resolution (`TypeExpr` → `Type`)

A helper method on `TypeChecker` that resolves a `TypeExpr` to a `Type`:

- Named primitives map directly: `"i64"` → `Int`, `"f64"` → `Float`, `"bool"` → `Bool`, `"str"` → `Str`, `"nil"` → `Nil`, `"any"` → `Any`, `"never"` → `Never`.
- Named non-primitives: look up in a struct registry (not yet populated; emit `UnknownType` for now and return `Any` as recovery). When struct support lands, this lookup becomes real.
- `Array`, `Map`, `Fn` variants: recursively resolve inner `TypeExpr`s.

---

## Part 10 — Module layout

Everything type-related goes in `src/typecheck/`:

- `mod.rs` — re-exports the public surface (`TypeChecker`, `Type`, `TypeError`)
- `types.rs` — the `Type` enum, `TypeEnv`, assignability logic
- `error.rs` — `TypeError`, `TypeErrorKind`, `Display` impl
- `checker.rs` — `TypeChecker`, all `check_*` methods

This module has no dependency on `src/interpret/`. Both depend on `src/ast.rs` and `src/id.rs`.

---

## Part 11 — Integration into `main.rs`

After parsing (which now produces the richer AST with optional `TypeExpr` nodes), optionally run the type checker:

- Controlled by a `--check` CLI flag, or always-on — your choice.
- Print errors to stderr with span information.
- Do **not** block interpretation — the interpreter runs regardless.
- Later: a `--strict` flag could make type errors fatal.

---

## Implementation order

**Phase 1 — AST and parser (no behavior change)**  
Add `TypeExpr`. Update `FnDeclNode`, `ParamNode`, `Statement::Declare`. Update the parser to parse optional annotations. Update the interpreter mechanically to handle the new field shapes. Run all existing tests — nothing should change.

**Phase 2 — Type infrastructure (no user-visible effect)**  
Create `src/typecheck/`. Add `Type`, `TypeEnv`, `TypeError`. Add a stub `TypeChecker` that walks the AST and returns an empty error list. Wire it into `main.rs` behind a flag.

**Phase 3 — Literal and declaration checking**  
Implement literal inference and `var` declaration checking. `var x: i64 = "hello"` now produces an error. `var x = 5` correctly infers `x: i64`.

**Phase 4 — Expression and operator checking**  
Implement binary operator rules, array/map literal inference, subscription checking.

**Phase 5 — Function checking**  
Implement function declaration type inference, function call arg checking, return type checking.

---

## Open questions

**Literal widening**: should `var x: f64 = 5` (integer literal, float annotation) be allowed? The runtime coerces seamlessly. Allowing it is more ergonomic; rejecting it is stricter. Most languages allow literal widening for numeric literals. Decision needed before Phase 3.

**`nil` as the bottom or as a unit**: is `nil` the type of the `nil` literal only, or is it also the implicit return type of functions that don't return a value? Recommendation: `nil` is the explicit "no value" type; blocks with no `last_expr` produce `nil`. `Never` is reserved for expressions that abort (bare `return`, future panics).

**`string` concatenation with `+`**: the runtime allows `"a" + "b"`. Should the type checker permit `str + str → str`? Straightforward to add as a special case in the binary operator rules.

**`when` expression result type**: if all arms produce the same type, the result is that type. If arms differ, use `Any` for now. Union types would make this precise later.

**Error recovery strategy**: when a type error is found, the checker returns `Any` for the problematic expression and continues. This can cause cascading false positives. If cascading is annoying in practice, add a "poisoned" flag to suppress downstream errors after a root cause.

**Span gaps**: many AST nodes (`BinaryOpNode`, `FnCallNode`, `SubscriptionNode`) do not currently carry `Span` fields. Type errors on those nodes will have imprecise source locations. This can be improved incrementally — it does not block the foundation. Track gaps with `// TODO: span` and fix when the checker is otherwise stable.
