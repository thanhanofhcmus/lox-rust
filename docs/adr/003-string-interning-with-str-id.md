# ADR 003: String Interning via StrId (Strings Removed from GC Heap)

**Date:** 2026-02 (commit `320fe3d`)  
**Status:** Accepted (with known memory leak)

## Context

The original design stored strings on the GC heap as `GcObject::Str(String)`, accessed via `GcHandle` like arrays and maps. This caused a **borrow checker conflict**:

Map key lookup required resolving two string handles to their content for comparison:

```rust
// Old get_at_index needed &Heap to compare string map keys
pub fn get_at_index(&self, indexee: Value, heap: &Heap) -> Result<Value, Error> {
    (MapKey::Str(l_handle), MapKey::Str(r_handle)) => {
        let l_str = heap.get_string(l_handle);
        let r_str = heap.get_string(r_handle);
        l_str == r_str
    }
```

At the same time, the interpreter often held a `&mut` borrow on the environment (which owns the heap), making it impossible to also pass `&Heap` to `get_at_index`. Rust's borrow checker rejected this.

Additionally, `GcObject::Str` meant `Value` could not implement `Copy` (strings owned heap data), forcing expensive `.clone()` calls throughout the interpreter.

## Decision

Introduce a `StringInterner` (`src/interpret/string_interner.rs`) that deduplicates strings and assigns each unique string a lightweight `StrId(usize)`.

- **Same content → same ID:** `intern(s)` checks a `HashMap<String, StrId>` before creating a new entry. This means string equality is reduced to integer equality — no heap lookup needed.
- **`Value::Str(StrId)`** stores only the 8-byte ID, not a handle. `StrId` is `Copy`.
- **`MapKey::Str(StrId)`** implements `Ord` directly (by ID), so `BTreeMap<MapKey, Value>` works without any reference to the heap — the borrow conflict is eliminated.
- `get_at_index` no longer needs `&Heap` as a parameter.
- `Value` now implements `Copy` (all variants are trivially copyable).

The `StringInterner` lives inside `Heap` (`heap.rs:118`) and is accessed via `heap.insert_string()` / `heap.get_string()`.

## Consequences

**Positive:**
- `Value` is `Copy` — interpreter functions pass values by value with no cloning.
- String equality is O(1) (integer comparison).
- Map key lookup is clean with no extra borrow.
- Repeated strings (e.g., the same key used in many maps) share storage.

**Negative / Known Issues:**
- Strings are **never freed.** The `mark()` phase has a `// TODO: mark for string_interner` comment at `heap.rs:174`. No reachability analysis is done on `StrId` values, so strings accumulate for the lifetime of the process.
- This is a memory leak for programs that generate many unique strings at runtime (e.g., via string concatenation in a loop).

**Future fix:** Change `StringInterner`'s backing store from `Vec<String>` (index-stable) to `HashMap<StrId, String>` with a monotonic counter. Then during `mark()`, collect all reachable `StrId` values (from `Value::Str` and `MapKey::Str` across all live objects), and call `string_interner.sweep(&reachable_ids)` to drop unreferenced entries from both maps.
