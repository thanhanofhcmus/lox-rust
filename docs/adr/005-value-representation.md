# ADR 005: Value Representation — Hybrid Number and Copy Semantics

**Date:** 2026-02 (commits `31035f7`, early architecture)  
**Status:** Accepted

## Context

Two related decisions shape how `Value` is represented at runtime:

1. **Number type**: The original Lox uses a single `double` for all numbers, which loses precision for large integers and makes array indexing awkward (`arr[2.0]`).
2. **Value ownership**: In a tree-walking interpreter, values are passed constantly — into functions, out of expressions, stored in environments. A non-`Copy` `Value` requires either moving (consuming ownership) or cloning (allocating), both creating significant borrow-checker friction and noise.

Heap-allocated types (arrays, maps, functions) cannot be stored inline in an enum if we want `Copy`, because they contain dynamically-sized data.

## Decision

### Hybrid Integer/Float Number

`Number` (`src/interpret/values/number.rs:14`) is an enum with two variants:

```rust
pub enum Number {
    Integer(i64),
    Floating(f64),
}
```

**Arithmetic rules:**
- `+`, `-`, `*`, `%`: if both operands are `Integer`, the result is `Integer`; otherwise `Floating`. Implemented via a single `impl_binary_op!` macro (`number.rs:40`).
- `/` (division): `Integer / Integer` stays `Integer` only if exact (`l % r == 0`); otherwise promotes to `Floating`. `4/2 → Integer(2)`, `5/2 → Floating(2.5)`.
- Mixed-type arithmetic always produces `Floating`.

**Equality:** Uses an epsilon of `1e-10` for all cases involving floats (`number.rs:95`). Integer-to-integer comparison is exact.

**Ordering:** `PartialOrd`/`Ord` implemented by casting to `f64` for mixed comparisons; `NaN` is ordered as `Less` by convention.

**Array indexing:** `Number::try_into::<usize>()` (`number.rs:77`) accepts non-negative integers and whole-number floats (`2.0`), rejecting fractional or negative values with `Error::ValueMustBeUsize`.

### Value as a Copy Type

`Value` (`src/interpret/values/value.rs`) derives `Copy` by storing heap-allocated types behind a `GcHandle(usize)` — an index into `Heap::slots`:

```rust
#[derive(Clone, Copy)]
pub enum Value {
    Unit,
    Scalar(Scalar),          // nil, bool, or Number — all Copy
    Str(StrId),              // interned string ID — Copy (see ADR 003)
    Array(GcHandle),         // index into heap — Copy
    Map(GcHandle),           // index into heap — Copy
    Function(GcHandle),      // index into heap — Copy
    BuiltinFunction(BuiltinFn), // fn pointer — Copy
}
```

`GcHandle` is `#[derive(Clone, Copy)]` and is simply a `usize` internally. Actual data for heap types lives in `Heap::slots: Vec<Option<HeapEntry>>`. Copying a `Value::Array(handle)` copies only the 8-byte index, not the array contents.

Because copying a `GcHandle` creates a second owner without the GC knowing, **manual ref count management is required**:
- `environment.rs:insert_variable_current_scope()` calls `heap.shallow_copy_value(value)` to increment the ref count.
- `environment.rs:pop_scope()` calls `heap.shallow_dispose_value(value)` for each variable leaving scope.
- When reassigning, `shallow_copy_value(new)` is called **before** `shallow_dispose_value(old)` to handle the self-assignment case (`x = x`) safely.

## Consequences

**Positive:**
- Integer arithmetic is exact — `1 + 1 == 2` not `2.0`.
- Array indexing with integer literals is natural and requires no cast.
- `i64` handles integers up to ~9.2 × 10¹⁸ without precision loss.
- Interpreter functions accept and return `Value` by value freely — no lifetime annotations, no explicit `.clone()` calls.
- Borrow-checker friction around passing values is essentially eliminated.

**Negative:**
- Fuzzy float equality (`1e-10` delta) can mask genuine bugs.
- `NaN` ordering is defined arbitrarily (`Ordering::Less`) rather than propagating as a distinct error.
- Every new variable binding must call `shallow_copy_value`; every scope exit must call `shallow_dispose_value`. Missing either silently corrupts the ref count.
- The `GcHandle` indirection adds one memory dereference for any array/map/function access.
