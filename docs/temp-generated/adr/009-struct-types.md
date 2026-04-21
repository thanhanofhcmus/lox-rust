# ADR 009: User-Defined Struct Types

**Date:** 2026-04  
**Status:** Accepted (field type validation incomplete)

## Context

Lox's built-in composite types (arrays and maps) are untyped collections. There was no way to define a named record type with a fixed, typed set of fields. Adding structs introduces:

- A named-type declaration form.
- A literal construction form.
- Type-system integration (struct types are first-class `TypeId` entries).
- Field access at runtime.

Several design choices had to be made: how to name fields (should they share the same namespace as variables?), how to track struct definitions across scopes, and whether field types are validated at construction or deferred to access.

## Decision

### Declaration Syntax

```lox
struct Point {
    x: Number,
    y: Number,
}
```

`struct` declarations are top-level statements (`Statement::StructDecl`). The struct name and each field name are tracked as `SymbolId` values — a newtype wrapper around `Id` (the hash-based identifier) — to distinguish them from ordinary variable `Id` values in the type environment.

The `TypeInterner` stores a `StructType { name: SymbolId, fields: Vec<(SymbolId, TypeId)> }` and assigns it a fresh `TypeId`.

### Literal Syntax

```lox
var p = Point { x: 1, y: 2 };
```

The parser produces a `RawValueNode::StructLiteral { name, fields }` node. The typechecker resolves `name` to the declared `StructType` and annotates the expression with the struct's `TypeId`.

### Field Access

Field access uses dot notation as part of the identifier chain:

```
get(p, "x")         # or p.x if dot access is implemented
```

At the time of writing, the dot operator in the Pratt parser (`Token::Dot`) has a `unimplemented!()` body — member access is parsed but not yet executed. Field values are accessible via the runtime `Value::Map`-backed struct representation (structs are stored as maps keyed by `StrId` field names) through subscript syntax as an interim measure.

### Type System Integration

Struct types are interned in `TypeInterner` alongside `Array`, `Map`, and `Function` types:

```rust
Type::Struct(StructType {
    name: SymbolId,
    fields: Vec<(SymbolId, TypeId)>,
})
```

Structural vs nominal typing: two structs with identical field names and types but different names are **distinct types** (nominal). The `TypeId` assigned to each `struct` declaration is unique, keyed by name.

### Typechecker Scope

`struct` declarations register the struct's `TypeId` in the type environment under the struct's `SymbolId`. The typechecker's `check_struct_decl()` method:

1. Resolves each field's type annotation to a `TypeId`.
2. Constructs a `StructType` and interns it.
3. Inserts `(struct_symbol_id → TypeId)` into the current type scope.

Struct declarations are visible to all code in the same scope and below, matching variable scoping rules.

### Runtime Representation

At runtime, struct values are stored as `Value::Map(GcHandle)` where each key is `MapKey::Str(StrId)` (the interned field name) and the value is the field's `Value`. No separate runtime struct type is introduced — the type identity is purely a compile-time concept tracked by `TypeId`.

This means structs share the map's GC behaviour (copy-on-write on reassignment, heap allocation, ref counting) without any additional runtime infrastructure.

## Consequences

**Positive:**
- Struct types are nominal — two `Point` structs have the same type; a `Point` and a `Coord` with identical fields do not.
- No new runtime `Value` variant needed — leverages the existing `Map` heap object and GC.
- `StructType` is stored in `TypeInterner` alongside all other types, keeping the type system uniform.
- Struct field names (`SymbolId`) are in a separate namespace from variable names (`Id`), preventing accidental collisions in lookup tables.

**Negative / Known Limitations:**
- **Field type validation at construction is incomplete.** The typechecker checks that the struct name resolves to a known struct, but does not yet verify that each provided field's value type matches the declared field type.
- **Dot member access is unimplemented.** `p.x` parses but panics at runtime. Field values must currently be accessed via runtime helpers or subscript workarounds.
- **No inheritance or trait-like polymorphism.** Struct types are flat and nominal with no subtyping.
- **Structs are stored as maps at runtime.** Any `MapKey::Str` key can be inserted into the underlying map object if accessed directly, bypassing the struct's declared schema.
- **No partial construction or default field values.** All fields must be supplied in the literal.
