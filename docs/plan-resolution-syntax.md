# Plan: 3-Level Resolution Syntax

**Date:** 2026-04  
**Status:** Draft — not yet implemented  
**Must be done before:** `plan-struct.md` (struct method dispatch builds on `.` and `:`)

---

## What this plan covers

Replace the current broken `prefix_ids` hack in `IdentifierNode` with three distinct, unambiguous resolution levels. Each level has its own operator, its own parsing context, and its own runtime semantics.

| Operator | Example | Meaning | When resolved |
|----------|---------|---------|---------------|
| `::` | `std::math::sin` | Module path | Load-time (future) |
| `:` | `Car:new()` | Type-associated call (no receiver) | Runtime, type-registry lookup |
| `.` | `arr.push(v)` | Method call (receiver as first arg) | Runtime, value-type dispatch |

The current design conflates all three into `IdentifierNode.prefix_ids`. This is buggy: `get_variable_all_scope` looks up `b` first when given `a.b`, ignoring the prefix entirely — a local variable named `b` shadows the intended meaning of `a.b`. It is also limited to one level deep.

---

## Core design decisions

**`.` always requires parentheses in this plan.** `arr.length()` is valid; `arr.length` without `()` is a parse error at this stage. When struct field access is added (see `plan-struct.md`), `.field` without `()` will become valid — but that is a structured extension point, not part of this plan.

**`::` is handled at the prefix level, not as a Pratt infix.** A full path `std::math::sin` is a single primary atom, not a chain of two operators. It is parsed entirely inside `parse_primary` when an identifier is followed by `::`.

**`:` is a Pratt infix that only applies to a bare identifier on the left.** `Car:new()` is valid; `(get_type()):new()` is not. The parser enforces this at parse time by checking that the left side is a `ClauseNode::Identifier`.

**The `prefix_ids` field on `IdentifierNode` is removed entirely.** A bare identifier has a name and nothing else. All dot-chaining is handled by Pratt infix parsing, not by identifier prefixes.

---

## New grammar

```
-- Level 1: module path (new primary node)
path              = IDENTIFIER ("::" IDENTIFIER)+

-- Level 2: type-associated call (Pratt infix)
type_method_call  = primary ":" IDENTIFIER "(" (expr ",")* ")"

-- Level 3: method call (Pratt infix)
method_call       = primary "." IDENTIFIER "(" (expr ",")* ")"
```

Both postfix operators bind at the same power level as `[` (subscription) and `(` (function call). They are left-associative, so `arr.filter(f).map(g)` parses as `(arr.filter(f)).map(g)`.

---

## Token changes

One new token: `DoubleColon` for `::`. The lexer peeks one character ahead when it sees `:` — if the next character is also `:`, it emits `DoubleColon` and advances twice; otherwise it emits `Colon` as today.

Existing uses of `Colon` are unaffected: array repeat syntax `[:val:repeat]` is inside `[...]`, and map literals `%{ "k": v }` are inside `%{...}`. Neither context produces an ambiguity.

---

## AST changes

### Simplify `IdentifierNode`

Remove `prefixes` and `prefix_ids`. A bare name — a `Span` and a resolved `Id` — is the only thing a simple identifier carries. Remove the methods that existed only for prefix logic: `new_from_vec`, `new_from_name`, `is_simple`, `create_name`.

### New: `PathNode`

Represents a `::` module path with at least two segments. Stores a `Vec<Span>` for error reporting and a `Vec<Id>` for the resolved segment names. A single identifier without `::` stays as `ClauseNode::Identifier`.

### New: `MethodCallNode`

Represents `receiver.method(args)`. Holds the receiver as a boxed `ClauseNode`, the method name (span + `Id`), and the argument expressions. The receiver is evaluated at runtime; its value type determines which dispatch table to use.

### New: `TypeMethodCallNode`

Represents `TypeName:method(args)`. Holds only the type name (span + `Id`) and the method name (span + `Id`), plus the argument expressions. There is no receiver expression — the type name maps directly to a dispatch table entry.

### `ClauseNode` — new variants

Add `Path(PathNode)`, `MethodCall(MethodCallNode)`, and `TypeMethodCall(TypeMethodCallNode)` alongside the existing variants.

---

## Parser changes

### `parse_identifier_node` — stop eating dots

Currently this function calls `parse_repeated_with_separator` with `Token::Dot` to build `prefix_ids`. Change it to consume exactly one identifier. Dots are now parsed by the Pratt infix loop, not here.

### `parse_primary` — handle `::` paths

After consuming a bare identifier, peek at the next token. If it is `DoubleColon`, continue consuming `:: IDENTIFIER` pairs to build a `PathNode`. If not, return a plain `ClauseNode::Identifier`. One-token lookahead, no backtracking.

### `get_binding_power` — give `Colon` chain power

Add `Token::Colon` to the existing `(ChainLeft, ChainRight)` arm alongside `Token::Dot`, `Token::LSquareParen`, and `Token::LRoundParen`.

### `parse_pratt_infix` — implement `Dot` and `Colon` arms

**`Token::Dot`:** Consume the dot. Consume an identifier for the method name. Then consume `(args)`. At this stage, dot always requires parentheses — no parens is a parse error. When struct field access is added later, this arm will check for `(` and branch between `MethodCallNode` and `FieldAccessNode` (see `plan-struct.md`).

**`Token::Colon`:** Consume the colon. Consume an identifier for the method name. Verify the left-hand side is a `ClauseNode::Identifier` (bare type name only). Consume `(args)`. If the left side is not a bare identifier, emit `InvalidTypeMethodTarget`.

### Reassignment targets

Method calls and type-associated calls are not valid lvalues. `convert_chaining` should reject them with a parse error. Only subscriptions (`arr[i]`) are valid chain steps at this stage. Field assignment via `.field` is added by `plan-struct.md`.

---

## Environment changes

### Simplify `get_variable_all_scope`

Remove the `prefix_ids` fallback. Variable lookup takes a plain `Id`, walks the scope stack, and falls back to the prelude. Module lookup no longer goes through identifier lookup at all — it is handled by the `ClauseNode::Path` interpreter arm.

### New: `get_module_export`

A dedicated function: given `(module_id, export_id)`, look up the module map and return the exported value. Initially only one-level paths (`module::export`) are supported. Deeper paths require a hierarchical module structure and can be added later.

---

## Runtime dispatch

The key new concept is **value-type dispatch** for `.` and **type-name dispatch** for `:`.

For `.` (method calls), the receiver value is evaluated first. Its runtime type (`Value::Array`, `Value::Map`, `Value::Str`) selects a dispatch table. Each table is a small enum — `ArrayMethod`, `MapMethod`, `StrMethod` — whose `from_id` maps a method `Id` to a variant, and whose `call` runs the logic. Unknown method name → `UnknownMethod`. Value type has no table → `ValueNotDispatchable`.

For `:` (type-associated calls), no receiver is evaluated. The type name `Id` selects the dispatch table directly. Initially this covers `Array:new()` and `Map:new()` as empty-literal shortcuts. When struct support is added, user-defined types look up their `ImplTable` in the type registry (see `plan-struct.md`).

For `::` (module paths), the last segment is the exported name and everything before it is the module path. Currently only one-level paths are supported: `module::export`.

---

## Prelude migration

The prelude functions are replaced by dot methods. During transition, both can coexist — the prelude keeps the old names, and the new dot methods run the same logic. Once dot methods are stable, remove the prelude entries and update the test files.

| Old prelude call | New dot call |
|-----------------|--------------|
| `array_len(arr)` | `arr.length()` |
| `array_push(arr, v)` | `arr.push(v)` |
| `array_pop(arr)` | `arr.pop()` |
| `array_insert(arr, i, v)` | `arr.insert(i, v)` |
| `map_length(m)` | `m.length()` |
| `map_keys(m)` | `m.keys()` |
| `map_values(m)` | `m.values()` |
| `map_insert(m, k, v)` | `m.insert(k, v)` |
| `map_remove(m, k)` | `m.remove(k)` |

`print`, `assert`, `from_json`, `to_json` stay in the prelude — they have no natural receiver type.

---

## Error variants

New runtime errors:

- `ModuleExportNotFound(Id, Id)` — module exists but the export name is not in it
- `ModulePathTooDeep(usize)` — path has more segments than currently supported
- `ValueNotDispatchable(Value, Id)` — value type has no method table
- `UnknownMethod(GcKind, Id)` — method name not known for this type
- `UnknownType(Id)` — type name in `:` dispatch is not registered

New parse error:

- `InvalidTypeMethodTarget(Span)` — left side of `:` is not a bare identifier

---

## Implementation order

**Phase 1 — `.` method calls**

Add `MethodCallNode` and `ClauseNode::MethodCall`. Implement the Dot arm in `parse_pratt_infix` (method call only, no field access yet). Add `dispatch_method` with `ArrayMethod` and `MapMethod` enum dispatch. Entirely additive — `parse_identifier_node` still eats dots for now, existing tests unaffected.

**Phase 2 — Strip `prefix_ids` from `IdentifierNode`**

Simplify `IdentifierNode` to name-only. Change `parse_identifier_node` to consume a single token. Remove prefix-related methods. Simplify `get_variable_all_scope` to plain scope-stack lookup. Audit all call sites — this is a breaking structural change. Run all tests; nothing should change functionally.

**Phase 3 — `::` paths and module support**

Add `Token::DoubleColon` to the lexer. Add `PathNode` and `ClauseNode::Path`. Implement the path branch in `parse_primary`. Add `get_module_export` to the environment. Add the `ClauseNode::Path` interpreter arm. Support two-segment paths only for now.

**Phase 4 — `:` type-associated calls**

Add `TypeMethodCallNode` and `ClauseNode::TypeMethodCall`. Give `Token::Colon` chain binding power. Implement the Colon arm in `parse_pratt_infix`. Add `dispatch_type_method` with initial support for `Array:new()` and `Map:new()`.

---

## Open questions

**`.` without `()`** — when struct field access is implemented (`plan-struct.md`), the Dot arm in `parse_pratt_infix` will check for `(` and produce either `MethodCallNode` or `FieldAccessNode`. This is the defined extension point for that plan.

**Chaining mutable methods** — `arr.push(1).push(2)` requires `push` to return the receiver. Currently array/map mutation methods return `nil`. Decide whether mutable methods should return `self` for ergonomic chaining.

**UFCS fallback** — should `arr.my_fn()` fall through to `my_fn(arr)` if no built-in method named `my_fn` exists? Enables user-defined "methods" but adds surprising behavior. Recommendation: no UFCS.

**`:` vs `::` for type methods** — `Car:new()` uses single `:` while module paths use `::`. This is intentional: `:` is a runtime lookup (type registry), `::` is a structural path (module hierarchy). They do not conflict because they are different operators with different grammars.
