# Plan: Struct System

**Date:** 2026-04  
**Status:** Draft — not yet implemented  
**Depends on:** `plan-resolution-syntax.md` (`.` method calls, `:` type-associated calls)  
**Shares Phase 1 with:** `plan-type-system-foundation.md` (`FnDeclNode` and `ParamNode` refactor)

---

## What this plan covers

User-defined data types with:
- Declarations listing ordered field names (`struct`)
- Separate implementation blocks attaching methods (`impl`)
- Construction literals that validate all fields at runtime
- Dot method dispatch and colon static dispatch, building on `plan-resolution-syntax.md`
- Dot field access and field assignment
- Destructuring in `var` declarations

This plan does **not** include visibility modifiers (no public/private), inheritance, or traits.

---

## Core design decisions

**`FnDeclNode` gets typed param slots now.** Parameters become `ParamNode` with an optional `TypeExpr`. All existing annotations are `None`, so the interpreter is completely unaffected. The type checker (see `plan-type-system-foundation.md`) reads these annotations later. This avoids a full function-node rewrite when type annotations are added.

**`self` is a keyword.** The lexer always emits `Token::SelfKw` when it sees `self`. Instance vs static is determined at parse time by checking the first param — not by convention. Using `self` outside an `impl` block is a parse error caught by the parser, not the runtime.

**`FieldAccess` and `Subscription` are distinct AST nodes, never unified.** `a.field` is a `FieldAccessNode`; `a["key"]` is a `SubscriptionNode`. The type checker reasons about struct fields and map keys differently, and conflating them would make that impossible.

**`StructInstance` fields are ordered, not a `HashMap`.** Fields are stored as `Vec<(Id, Value)>` in declaration order. This distinguishes struct instances from maps (where order has no meaning) and preserves the declared shape for display and equality.

**Construction validates field completeness.** Creating a struct literal checks that exactly the declared fields are present — no extras, no missing fields. The runtime enforces this now; the type checker will enforce it statically later.

**The type registry is split into a static layer and a runtime layer.** `ImplDef` holds method metadata (parameter types, return type) for the future type checker. `ImplTable` holds the actual `Value::Function` closures for the interpreter. Both are populated when an `impl` block is evaluated, but they serve entirely different consumers.

---

## Syntax

```
struct_decl     = "struct" IDENTIFIER "{" struct_fields "}"
struct_fields   = (IDENTIFIER ",")*

impl_decl       = "impl" IDENTIFIER "{" impl_method* "}"
impl_method     = IDENTIFIER "=" fn_decl

fn_decl         = "fn" "(" param_list ")" ("->" type_expr)? (block | expr)
param_list      = (param ("," param)*)?
param           = "self" | IDENTIFIER (":" type_expr)?

struct_literal  = IDENTIFIER "{" struct_literal_fields "}"
struct_literal_fields = (struct_literal_field ",")*
struct_literal_field  = IDENTIFIER ("=" clause)?     -- shorthand if no "= clause"

struct_destr    = IDENTIFIER "{" struct_destr_fields "}"
struct_destr_fields   = (struct_destr_field ",")*
struct_destr_field    = IDENTIFIER ("=" IDENTIFIER)?  -- shorthand or rename
```

Full example:

```lox
struct Vec3 {
    x,
    y,
    z,
}

impl Vec3 {
    new = fn(x, y, z) {
        Vec3 { x, y, z }
    }

    dot = fn(self, other) {
        self.x * other.x + self.y * other.y + self.z * other.z
    }

    scale = fn(self, factor) {
        Vec3 { x = self.x * factor, y = self.y * factor, z = self.z * factor }
    }
}

var v1 = Vec3:new(1.0, 2.0, 3.0)
var v2 = Vec3:new(4.0, 5.0, 6.0)

var d      = v1.dot(v2)
var scaled = v1.scale(2.0)

print(v1.x)        -- field access
v1.x = 10.0        -- field assignment

var Vec3 { x, y, z }          = v1   -- shorthand destructure
var Vec3 { x = ax, y = ay }   = v2   -- rename destructure
```

---

## New tokens

Three new keywords: `Struct`, `Impl`, and `SelfKw`. The lexer maps the source words `struct`, `impl`, and `self` to these tokens. `self` is always lexed as `SelfKw` — the parser enforces that it only appears as a parameter inside an `impl` block.

---

## AST changes

### `TypeExpr` — shared with the type system plan

A `TypeExpr` represents what a programmer writes to annotate a type. The full definition is specified in `plan-type-system-foundation.md` and covers named types, array types, map types, and function types. For Phase 1 of this plan, only the `Named(Span, Id)` variant is needed. The full `TypeExpr` is added when the type system plan is implemented. Do not define a separate stub here — implement this plan's Phase 1 and the type system plan's Phase 1 together.

### `ParamNode` — replaces bare `IdentifierNode` in function params

Each parameter carries a name (span + `Id`), an `is_self` flag (true when the token was `SelfKw`), and an optional `TypeExpr` annotation. The annotation is `None` for all existing code. The interpreter reads only the name; the type checker reads the annotation.

### `FnDeclNode` — typed params and optional return type

Replace `arg_names: Vec<IdentifierNode>` with `params: Vec<ParamNode>`. Add `return_type: Option<TypeExpr>` — absent when `->` is not written. The `RTArrow` token already exists. The interpreter extracts `name_id` from each `ParamNode` and ignores all annotation fields.

### `StructDeclNode`

Holds the type name (span + `Id`) and an ordered list of field names as `Vec<(Span, Id)>`. No types, no defaults — just names in declaration order.

### `ImplMethodNode` and `ImplDeclNode`

`ImplDeclNode` holds the type name and a list of `ImplMethodNode`s. Each method has a name (span + `Id`), an `is_instance` flag, and the full `FnDeclNode`. `is_instance` is derived at parse time from whether the first param's `is_self` is true — no runtime check needed.

### `StructLiteralNode`

Holds the type name and a list of fields. Each field is either a `Shorthand` (field name and local variable share the same name — `Vec3 { x }` reads variable `x` into field `x`) or `Explicit` (field name with a separate expression — `Vec3 { x = expr }`).

### `FieldAccessNode`

Holds the object expression and the field name as a compile-time `Id`. This is distinct from `SubscriptionNode`, which uses a runtime expression as the key. Field names are always known at parse time.

### `FollowNode` — typed chain steps for reassignment

Replace `Vec<ClauseNode>` in `ChainingReassignTargetNode.follows` with `Vec<FollowNode>`. This enum has two variants:

- `Subscription(ClauseNode)` — an index expression like `arr[i]`
- `FieldAccess(Id)` — a field name resolved at parse time, like `.x`

This distinction is necessary so the runtime can dispatch correctly when walking an assignment chain. `arr[i] = v` and `struct.x = v` need different operations on the heap object.

### `DeclareTarget` — struct destructuring in `var`

`Statement::Declare` currently takes a plain `IdentifierNode`. Change the first field to a `DeclareTarget` enum:

- `Ident(IdentifierNode)` — plain `var x = ...`
- `StructDestructure(StructDestructureNode)` — `var Vec3 { x, y } = ...`

Each destructure field is either `Shorthand` (bind `x` from field `x`) or `Rename` (bind `my_x` from field `x`).

### `Statement::Declare` — combined final form

To agree with `plan-type-system-foundation.md`, the declare statement takes three fields:

```
Declare(DeclareTarget, Option<TypeExpr>, Expression)
```

The `Option<TypeExpr>` is the variable's type annotation for `var x: i64 = ...`. It only applies to `DeclareTarget::Ident` — annotating a struct destructure is not supported for now. The interpreter ignores the annotation; the type checker uses it.

### `ClauseNode` — new variants

In addition to `Path`, `MethodCall`, and `TypeMethodCall` from `plan-resolution-syntax.md`, add:

- `StructLiteral(StructLiteralNode)` — parsed in primary position when `Identifier {` is seen
- `FieldAccess(FieldAccessNode)` — parsed in Pratt infix position when `.name` appears without following `(`

---

## Parser changes

### `parse_param_node`

Replaces the simple identifier-consuming closure in `parse_function_decl`. Handles three cases: `self` keyword (sets `is_self = true`, reads optional `: type`), bare identifier (`is_self = false`, no type), and `name: type` (identifier with annotation). Reads a flag `is_in_impl` from the parse context — if `self` appears when the flag is false, emit a parse error.

### `parse_function_decl`

Uses `parse_param_node` instead of the old closure. After the closing `)`, optionally consumes `RTArrow` followed by `parse_type_expr` for the return type.

### `parse_struct_decl` and `parse_impl_decl`

Top-level, dispatched from `parse_stmt` when `Token::Struct` or `Token::Impl` is seen. `parse_impl_decl` sets `is_in_impl = true` on the parse context before parsing each method, and clears it after. This flag enables `parse_param_node` to validate `self`.

### `parse_primary` — detect struct literals

After consuming an identifier, peek at the next token:
- `::` → parse a path (from `plan-resolution-syntax.md`)
- `{` (`LPointParen`) → parse a struct literal
- anything else → return a plain identifier

This one-token lookahead is unambiguous. `{` only starts a block at the `parse_expr` level, never inside `parse_primary`.

### `parse_pratt_infix` — Dot arm extended

The Dot arm from `plan-resolution-syntax.md` handled only method calls (dot always required `()`). This plan extends it: after consuming a dot and an identifier, peek for `(`. If present, produce a `MethodCallNode`. If absent, produce a `FieldAccessNode`. Both use the same binding power — no grammar ambiguity.

### `parse_declaration` — struct destructure

After consuming `var`, use two-token lookahead: if the next two tokens are `Identifier` followed by `{`, parse a struct destructure. Otherwise parse a plain identifier. The pattern `var x = ...` has `Identifier` then `=`, and `var T { ... }` has `Identifier` then `{` — these are distinct.

---

## Runtime representation

### `GcObject::Struct` and `StructInstance`

Add `GcObject::Struct(StructInstance)` alongside `Array`, `Map`, and `Function`. A `StructInstance` holds the type `Id` (for dispatch and destructuring validation) and an ordered `Vec<(Id, Value)>` of fields.

Fields are not stored in a `HashMap` — declaration order is preserved deliberately. Field access is a linear scan by `Id`, acceptable for small structs.

### `Value::Struct(GcHandle)`

Structs have reference semantics, matching arrays and maps. Multiple variables can reference the same instance. `get_handle` and `replace_handle` on `Value` must include the `Struct` variant.

### Type registry

A new `TypeRegistry` struct lives in the interpreter's `Environment`. It has three maps:

- `struct_defs` — field names in declaration order, populated when a `struct` statement is evaluated
- `impl_defs` — method metadata for the future type checker (param types, return types), populated when an `impl` block is evaluated
- `impl_tables` — runtime `Value::Function` closures, populated when an `impl` block is evaluated

The type checker reads `impl_defs`. The interpreter reads `impl_tables`. Both are populated together when an `impl` block runs, but they serve different consumers.

---

## Method dispatch

### Instance methods (`.method()`)

The `dispatch_method` function from `plan-resolution-syntax.md` gains a `Value::Struct(handle)` arm. Look up `type_id` from the instance, look up the method `Id` in `impl_tables`, prepend the receiver as the first argument (`self` binding), and call the function.

### Static methods (`:method()`)

The `dispatch_type_method` function from `plan-resolution-syntax.md` gains struct type support. Look up the type `Id` in `impl_tables` and call the function directly — no receiver is prepended.

### Field assignment

The heap's `deep_copy_reassign_object` currently takes a chain of raw `Value` indices. It needs to be extended to accept `FollowNode` steps. Each step dispatches on the variant: `Subscription` uses the existing `get_at_index`/`replace_at_index`; `FieldAccess` uses new `get_at_field`/`replace_at_field` methods added to `GcObject`.

---

## GC integration

Struct fields are handled exactly like array elements:

- In `mark()`: walk each field value, mark string IDs and recursively trace GcHandles
- In `shallow_dispose_value()`: recursively dispose each field when the struct's ref count reaches zero

`Environment` gets `insert_struct_variable` (creates the GcObject and returns `Value::Struct(handle)`) and `get_struct` (extracts `&StructInstance` from a handle).

---

## Error variants

Parse errors:
- `SelfOutsideImpl(Span)` — `self` used in a function that is not inside an `impl` block

Runtime errors:
- `StructAlreadyDefined(Id)` — same struct name defined twice
- `ImplForUnknownStruct(Id)` — `impl` references a type with no `struct` declaration
- `StructMissingField(Id, Id)` — construction literal omits a declared field
- `StructUnknownField(Id, Id)` — construction literal provides an undeclared field
- `StructDuplicateField(Id, Id)` — construction literal specifies the same field twice
- `StructFieldNotFound(Id, Id)` — field access or destructure names a field not in the declaration
- `ValueNotAStruct(Value, Id)` — field access on a non-struct value
- `MethodNotFound(Id, Id)` — method name not in `ImplTable` for the given type
- `UnknownStructType(Id)` — struct literal references an undefined type
- `DestructureTargetNotAStruct(Value)` — struct destructure bound to a non-struct value
- `DestructureTypeMismatch(Id, Id)` — destructure pattern type does not match value's type

---

## Implementation order

**Phase 1 — `FnDeclNode` refactor** *(implement together with `plan-type-system-foundation.md` Phase 1)*

Add `TypeExpr` (at least the `Named` variant), `ParamNode`, and update `FnDeclNode` to use `Vec<ParamNode>` and `Option<TypeExpr>` for the return type. Update `parse_function_decl` to use `parse_param_node`. Update the interpreter to read `name_id` from each `ParamNode`. All existing tests must pass unchanged — this is a pure structural rename.

**Phase 2 — Struct and impl declarations**

Add `Struct`, `Impl`, `SelfKw` tokens. Add `StructDeclNode`, `ImplDeclNode`, `ImplMethodNode` to the AST. Add `Statement::StructDecl` and `Statement::ImplDecl`. Create `TypeRegistry` and add it to `Environment`. Implement `interpret_struct_decl` and `interpret_impl_decl`. No new expression forms yet — only statement-level declarations.

**Phase 3 — Construction and field access**

Add `StructLiteralNode`, `FieldAccessNode`, and the corresponding `ClauseNode` variants. Implement the struct literal branch in `parse_primary` and the field access branch in the Dot arm. Add `GcObject::Struct`, `Value::Struct`, and the GC mark/dispose/insert support. Implement struct literal evaluation and field access evaluation.

Test goal: `var v = Vec3:new(1, 2, 3); print(v.x);` works end-to-end.

**Phase 4 — Field assignment and method dispatch**

Replace `Vec<ClauseNode>` in `ChainingReassignTargetNode` with `Vec<FollowNode>`. Update `convert_chaining` to handle `ClauseNode::FieldAccess`. Extend the heap's reassign traversal to accept `FollowNode` steps. Add the struct arm to `dispatch_method` and `dispatch_type_method`.

Test goal: `v.x = 5.0;` and `v1.dot(v2)` and `Vec3:new(...)` all work.

**Phase 5 — Destructuring**

Add `StructDestructureNode`, `DeclareTarget`, and update `Statement::Declare` to its final form `Declare(DeclareTarget, Option<TypeExpr>, Expression)`. Implement `peek_ahead_is_struct_destructure` and `parse_struct_destr_field` in the parser. Implement the destructure arm of `interpret_declare`.

Test goal: `var Vec3 { x, y, z } = v1;` binds `x`, `y`, `z` correctly.

---

## Open questions

**Struct equality**: field-by-field comparison (like arrays) or reference identity only? Recommendation: field-by-field with a type-id check first. A future trait system could override this.

**Multiple `impl` blocks**: allowed (methods merged) or an error? Recommendation: merge, but error on duplicate method names.

**`self` type annotation**: should `fn(self: Vec3, ...)` eventually be required? Leave optional for now; the type checker can enforce it.

**`.` on maps**: `map.key` (no parens) as field-access sugar for `map["key"]`? Not in scope — keep `map["key"]` as the only map access syntax to avoid ambiguity with method calls.

**Display of struct instances**: `Id` is a hash — recovering the name string for display requires the registry. Store `name_str: String` in `StructDef` alongside `name: Id` to make display work without a reverse lookup.

**Trait/impl-for-trait syntax**: `impl Vec3: Printable { ... }` — deferred until traits are designed.
