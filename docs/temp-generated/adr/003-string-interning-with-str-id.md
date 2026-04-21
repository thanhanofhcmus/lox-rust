# ADR 003: String Interning via StrId (Strings Removed from GC Heap)

**Date:** 2026-02 (commit `320fe3d`)  
**Status:** Accepted (memory leak resolved 2026-04)

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

**Negative:**
- `StrId` equality (`id_a == id_b`) implies string equality only within a single interner instance. Across restarts or hypothetical multi-interner scenarios the invariant breaks — not a current concern but worth noting.
- Monotonic ID allocation means IDs are never reused after sweep; a very long-running program generating unique strings could exhaust `usize` (unlikely in practice).

**Resolution (2026-04):** The memory leak was fixed in `src/interpret/string_interner.rs`. `StringInterner` now participates in the mark-and-sweep GC cycle via three methods:

- `reset_marks()` — clears the live-set before each GC cycle.
- `mark_to_keep(id)` — called for every `StrId` reachable from live values (in `Value::Str` and `MapKey::Str`).
- `sweep()` — drops all entries from both the `str_to_id` and `id_to_str` maps that were not marked.

The heap's `mark()` function now calls `string_interner.reset_marks()` at the start and `string_interner.mark_to_keep(id)` for each string encountered during root tracing, followed by `string_interner.sweep()` at the end — matching the pattern described in the original future-fix note.
