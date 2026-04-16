# ADR 002: Hybrid GC — Reference Counting + Mark-and-Sweep

**Date:** 2026-01  
**Status:** Accepted (with known issues)

## Context

Arrays, maps, and functions must be heap-allocated because their size is not known at compile time and they need to outlive the scope that created them (closures, returned arrays, etc.). Two classic GC approaches exist:

- **Reference counting** — simple, deterministic, but cannot collect cycles.
- **Mark-and-sweep** — handles cycles, but requires a full trace of the heap.

The language has value semantics for arrays and maps (assigning an array to a variable should behave like copying), but implementing true deep copies on every assignment would be prohibitively expensive.

## Decision

Use a hybrid approach implemented in `src/interpret/heap.rs`:

`HeapEntry` (`heap.rs:83`) carries two fields:
- `ref_count: usize` — tracks how many variable bindings point to this object, used to implement **copy-on-write** semantics cheaply.
- `is_marked_for_delete: bool` — used by the **mark-and-sweep** collector.

**Reference counting** (copy-on-write layer):
- `shallow_copy_value()` increments `ref_count` when a value is bound to a variable.
- `shallow_dispose_value()` decrements `ref_count`; if it reaches zero, the object is immediately marked for deletion and its children are recursively disposed.

**Mark-and-sweep** (cycle / leak collection):
- `mark(root_values)` marks all objects for deletion, then traces from roots (all live variables in all scopes and modules) to unmark reachable objects.
- `sweep()` frees any remaining marked objects and returns their slots to `free_list`.
- Triggered automatically in `environment.rs:266` when `number_of_used_objects >= GC_TRIGGER_POINT` (100).

Slots are reused via `free_list: Vec<usize>` to avoid unbounded growth of the `slots` vec.

## Consequences

**Positive:**
- Cycles are collected (pure ref counting cannot do this).
- Value semantics are implementable without deep copies on every assignment.
- The `free_list` means slot indices are stable handles even after collection.

**Negative / Known Issues:**
- `ref_count.sub_assign(1)` in `update_ref_count` has no underflow guard — wraps to `usize::MAX` in release builds, panics in debug. Should use saturating subtraction.
- The GC trigger point (100 objects) is a fixed constant. If the live set sits at exactly 101 objects, `mark()`+`sweep()` runs on every single assignment — O(n) overhead at steady state. An adaptive threshold (double after each collection) would avoid this.
- `deep_copy_reassign_object` (`heap.rs:256`) replaces values during a structural copy but does not decrement the ref count of the displaced old value (noted as `TODO: dispose old value` at `heap.rs:274`).
- String objects are not part of this GC (see ADR 003).
