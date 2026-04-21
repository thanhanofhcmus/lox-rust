# ADR 008: Gradual Type System with Two-Phase AST

**Date:** 2026-04  
**Status:** Accepted

## Context

Lox is a dynamically typed language. Adding a static type system raises the question of strictness:

- **Fully static** — every expression must be typed at compile time; untyped code is rejected.
- **Fully dynamic** — no static checks; all errors surface at runtime.
- **Gradual typing** — types are optional; typed and untyped code can coexist, with static checks applied where types are known and runtime checks deferred where they are not.

The goal was to add *optional* type annotations that catch obvious errors without requiring every variable to be annotated, making the transition from dynamic Lox natural.

A second problem was representing "has been type-checked" vs "has not" at the type level. The interpreter needs type information (e.g., to resolve struct field accesses), but the parser produces an untyped AST. Without a clear separation, it is easy to accidentally run the interpreter on an untyped node.

## Decision

### Two-Phase AST via a Type Parameter

`AST<T>` and all node types (`Expression<T>`, `ExprCase<T>`, etc.) are parameterised on `T`, which represents the type annotation attached to each expression:

- After parsing: `AST<()>` — every expression carries `()`, meaning "no type info yet".
- After type-checking: `AST<TypeId>` — every expression carries a `TypeId`, the interned type of that expression.

The typechecker's signature is:

```rust
pub fn check(&mut self, ast: AST<()>) -> Result<AST<TypeId>, TypecheckError>
```

The interpreter only accepts `AST<TypeId>`. Passing an untyped `AST<()>` to the interpreter is a compile-time type error in Rust. This makes skipping the typecheck pass impossible by construction.

### Type Interning with TypeId

`TypeId` (`src/types.rs`) is a `newtype(usize)`. The `TypeInterner` maintains a bidirectional mapping between `Type` values and `TypeId` integers, with pre-allocated reserved slots:

| TypeId | Type |
|--------|------|
| 0 | Any |
| 1 | Bool |
| 2 | Number |
| 3 | Str |
| 4 | Unit |
| 5 | Nil |
| 6 | Any (function) |

User-defined types (struct types, array/map/function types) are interned starting at ID 11. Interning means `TypeId` equality implies structural type equality — no recursive comparison needed.

### Any as the Top Type

`Type::Any` is the gradual typing escape hatch. The typechecker applies it in two ways:

1. **Inference fallback:** when a variable is declared without a type annotation and the initialiser's type cannot be determined, the variable receives `Any`.
2. **Unification fallback:** when two branches of a control-flow expression (e.g., `if`/`when`) produce incompatible concrete types, the result widens to `Any` rather than raising an error.

Operations involving `Any` are always permitted by the typechecker. For example, calling a value of type `Any` as a function, or indexing it, generates no static error — the check is deferred to runtime.

**Propagation rule:** if either operand of a binary expression is `Any`, the result is `Any`.

### Two Initialisation Scopes

The typechecker's `TypeEnvironment` is initialised with **two** scopes rather than one:

- **Scope 0 (outermost):** built-in functions (`print`, `assert`, array/map operations, etc.) are inserted here.
- **Scope 1:** user globals and subsequently declared variables are inserted here.

This means user code can shadow a built-in by declaring `var print = ...` — the declaration lands in scope 1, and lookup finds it before scope 0 — without triggering a `ReDeclareVariable` error (which fires only when the same name is declared twice in the *same* scope).

### Variadic Built-in Function Types

Some built-in functions accept a variable number of arguments of a single type (e.g., `print` accepts any number of `Str` arguments). These are represented as:

```rust
Type::Function {
    params: vec![TypeId::STR],        // required minimum
    variadict: Some(TypeId::STR),     // type of each extra argument
    return_: TypeId::STR,
}
```

The typechecker allows any number of arguments beyond `params.len()` as long as each extra argument matches `variadict`.

## Consequences

**Positive:**
- The Rust type system enforces the parse → typecheck → interpret pipeline order. It is impossible to interpret an untyped AST.
- `TypeId` comparisons are O(1) integer comparisons; structural type equality is free.
- Gradual typing means existing untyped Lox code runs unchanged; type annotations can be added incrementally.
- `Any` prevents the typechecker from blocking code that is dynamically correct but statically ambiguous.

**Negative:**
- `Any` is infectious: once a value is `Any`, all downstream expressions are also `Any`, silently disabling further static checks. Poorly annotated code can reduce to no type checking at all.
- The two-phase AST requires all node transformations to reconstruct the full tree with `TypeId` attached; no in-place mutation.
- Type unification widening to `Any` rather than raising an error on branch type mismatch can hide real bugs (e.g., an `if` branch returning `Number` and another returning `Bool`).
- For-loop iteration variables are always typed as `Any` (the collection element type is not yet propagated), limiting inference inside loop bodies.
