# Plan: 3-Level Resolution Syntax

**Date:** 2026-04  
**Status:** Draft — not yet implemented

## Overview

Replace the current broken `prefix_ids` hack in `IdentifierNode` with three distinct,
unambiguous resolution levels:

| Syntax | Operator | Meaning | Analog |
|--------|----------|---------|--------|
| `std::math::sin` | `::` | Module path (load-time) | Rust, C++ |
| `Car:new()` | `:` | Type-associated call (no receiver) | Rust `Type::fn()` |
| `arr.push(v)` | `.` | Method call (receiver as first arg) | Lua, Python |

The current design conflates all three into `IdentifierNode.prefix_ids`, which is both
buggy (scope lookup ignores the prefix) and limited to one level deep.

---

## 1. New Grammar

```
-- Level 1: module path (new primary node)
path              = IDENTIFIER ("::" IDENTIFIER)+

-- Level 2: type-associated call (new postfix in parse_pratt_infix)
type_method_call  = primary ":" IDENTIFIER "(" (expr ",")* ")"

-- Level 3: method call (replaces unimplemented Dot arm)
method_call       = primary "." IDENTIFIER "(" (expr ",")* ")"

-- Updated primary (path replaces prefixed identifier)
primary           = path | IDENTIFIER | group | raw_value

-- Updated clause (method/type calls are highest-binding postfix)
clause            = ... | method_call | type_method_call | subscription | call
```

The `.` and `:` postfix forms both bind at `ChainLeft/ChainRight` — same as `[` and `(`.
They are right-associative, so `arr.filter(f).map(g)` parses as `(arr.filter(f)).map(g)`.

`::` is handled at the **prefix level** (inside `parse_primary`), not as a Pratt infix,
because the full path `a::b::c` is a single primary atom, not a chain of two operators.

---

## 2. Token Changes

**File: `src/token.rs`**

Add one token:

```rust
DoubleColon, // ::
```

**File: `src/parse/lex.rs`**

When lexing `:`, peek ahead: if the next character is also `:`, emit `DoubleColon` and
advance twice; otherwise emit `Colon` as today.

Existing uses of `Colon` are all in contexts where `::` can't appear:
- Array repeat `[:val:repeat]` — inside `[...]`, the leading `[` disambiguates.
- Map literals `%{ "k": v }` — inside `%{...}`.

So this is a non-breaking lexer change.

---

## 3. AST Changes

**File: `src/ast.rs`**

### 3a. Simplify `IdentifierNode`

Remove `prefixes` and `prefix_ids`. A bare name is all that's needed:

```rust
pub struct IdentifierNode {
    pub name: Span,
    pub name_id: Id,
}

impl IdentifierNode {
    pub fn new(name: Span, input: &str) -> Self {
        Self { name, name_id: Id::new(name.str_from_source(input)) }
    }

    pub fn get_id(&self) -> Id { self.name_id }

    pub fn name_str<'a>(&self, input: &'a str) -> &'a str {
        self.name.str_from_source(input)
    }
}
```

Remove `new_from_vec`, `new_from_name` (replaced by `new`), `prefix_ids`, `prefixes`,
`is_simple`, `create_name`.

### 3b. New: `PathNode`

Represents a `::` module path. Always has at least two segments (a single identifier
stays as `ClauseNode::Identifier`).

```rust
pub struct PathNode {
    pub segments: Vec<Span>,      // spans for error reporting
    pub segment_ids: Vec<Id>,     // [std, math, sin]
}
```

### 3c. New: `MethodCallNode`

Receiver `.` method `(args)` — runtime dispatch on value type.

```rust
pub struct MethodCallNode {
    pub receiver: Box<ClauseNode>,
    pub method_name: Span,
    pub method_id: Id,
    pub args: Vec<Expression>,
}
```

### 3d. New: `TypeMethodCallNode`

TypeExpr `:` method `(args)` — dispatch on type name, no runtime receiver.

```rust
pub struct TypeMethodCallNode {
    pub type_name: Span,
    pub type_id: Id,
    pub method_name: Span,
    pub method_id: Id,
    pub args: Vec<Expression>,
}
```

### 3e. Extend `ClauseNode`

```rust
pub enum ClauseNode {
    // existing
    Unary(Box<ClauseNode>, Token),
    Binary(BinaryOpNode),
    RawValue(RawValueNode),
    Group(Box<ClauseNode>),
    Identifier(IdentifierNode),
    Subscription(SubscriptionNode),
    FnCall(FnCallNode),

    // new
    Path(PathNode),
    MethodCall(MethodCallNode),
    TypeMethodCall(TypeMethodCallNode),
}
```

---

## 4. Parser Changes

**File: `src/parse/parser.rs`**

### 4a. `parse_identifier_node` — stop eating dots

Currently uses `parse_repeated_with_separator(state, Token::Dot, ...)` to build
`prefix_ids`. Change to just consume a single identifier:

```rust
fn parse_identifier_node(state: &mut Context) -> Result<IdentifierNode, ParseError> {
    let li = state.consume_token(Token::Identifier)?;
    Ok(IdentifierNode::new(li.span, state.get_input()))
}
```

### 4b. `parse_primary` — handle `::` paths

After consuming a bare identifier, peek ahead: if the next token is `DoubleColon`,
continue consuming `:: IDENTIFIER` segments to build a `PathNode`:

```rust
fn parse_primary(state: &mut Context) -> Result<ClauseNode, ParseError> {
    if state.peek(&[Token::Identifier]) {
        let first_li = state.consume_token(Token::Identifier)?;
        if state.peek(&[Token::DoubleColon]) {
            // parse path: first :: second :: ...
            let mut spans = vec![first_li.span];
            while state.peek(&[Token::DoubleColon]) {
                state.consume_token(Token::DoubleColon)?;
                let seg_li = state.consume_token(Token::Identifier)?;
                spans.push(seg_li.span);
            }
            let input = state.get_input();
            let segment_ids = spans.iter().map(|s| Id::new(s.str_from_source(input))).collect();
            return Ok(ClauseNode::Path(PathNode { segments: spans, segment_ids }));
        }
        return Ok(ClauseNode::Identifier(IdentifierNode::new(first_li.span, state.get_input())));
    }
    // ... group and raw_value arms unchanged
}
```

### 4c. `get_binding_power` — add `Colon` as chain operator

```rust
Token::LSquareParen | Token::LRoundParen | Token::Dot | Token::Colon
    => (ChainLeft, ChainRight),
```

`Colon` already exists as a token; it just needs a binding power so the Pratt loop
picks it up as a postfix trigger.

### 4d. `parse_pratt_infix` — implement `Dot` and `Colon` arms

**`Token::Dot` — method call:**

```rust
Token::Dot => {
    state.consume_token(Token::Dot)?;
    let method_li = state.consume_token(Token::Identifier)?;
    let input = state.get_input();
    let method_id = Id::new(method_li.span.str_from_source(input));
    let args = parse_comma_list(state, Token::LRoundParen, Token::RRoundParen, parse_expr)?;
    Ok(ClauseNode::MethodCall(MethodCallNode {
        receiver: Box::new(lhs),
        method_name: method_li.span,
        method_id,
        args,
    }))
}
```

**`Token::Colon` — type-associated call:**

```rust
Token::Colon => {
    state.consume_token(Token::Colon)?;
    let method_li = state.consume_token(Token::Identifier)?;
    let input = state.get_input();
    // lhs must be a bare Identifier (the type name)
    let ClauseNode::Identifier(type_iden) = lhs else {
        return Err(ParseError::InvalidTypeMethodTarget(method_li.span));
    };
    let method_id = Id::new(method_li.span.str_from_source(input));
    let args = parse_comma_list(state, Token::LRoundParen, Token::RRoundParen, parse_expr)?;
    Ok(ClauseNode::TypeMethodCall(TypeMethodCallNode {
        type_name: type_iden.name,
        type_id: type_iden.name_id,
        method_name: method_li.span,
        method_id,
        args,
    }))
}
```

### 4e. `parse_reassignment_or_expr` — reassignment with dot chains

Currently `ChainingReassignTargetNode.base` is an `IdentifierNode` (a name) and
`follows` is a `Vec<ClauseNode>` (the index chain). The `convert_chaining` helper
that builds this from an expression will need to handle `MethodCall` lvalues.

However: `arr.push(v) = x` is not a valid lvalue — method calls are not assignable.
Only subscriptions (`arr[i]`) are. So `convert_chaining` should reject `MethodCall`
and `TypeMethodCall` and return `ParseError::InvalidAssignTarget`.

For now, `a.b.c = v` is **not** a goal — field assignment stays as `a["b"]["c"] = v`.
If struct field assignment is added later, it can be handled as a new `FieldAssignNode`.

---

## 5. Environment Changes

**File: `src/interpret/environment.rs`**

### 5a. Simplify `get_variable_all_scope`

Remove the `prefix_ids` fallback. The signature takes a plain `Id`:

```rust
pub fn get_variable(&self, id: Id) -> Option<(Value, bool /* is_readonly */)> {
    for scope in self.scope_stack.iter_outward() {
        if let Some(value) = scope.get_variable(id) {
            return Some((value, scope.is_readonly));
        }
    }
    if let Some(value) = self.preludes.get(&id) {
        return Some((*value, true));
    }
    None
}
```

Module access is no longer done through identifier lookup — it goes through `PathNode`
interpretation instead (see §6c).

### 5b. Add `get_module_export`

```rust
pub fn get_module_export(&self, module_id: Id, export_id: Id) -> Option<Value> {
    self.modules.get(&module_id)?.variables.get(&export_id).copied()
}
```

---

## 6. Interpreter Changes

**File: `src/interpret/interpreter.rs`**

Add three new match arms in `interpret_clause_expr`:

### 6a. `ClauseNode::Path` — module path resolution

```rust
ClauseNode::Path(node) => {
    // last segment is the exported name; everything before it is the module path.
    // Currently only one-level module paths are supported (module::export).
    // Multi-level (std::math::sin) requires a module hierarchy — see §8.
    let (&export_id, module_segs) = node.segment_ids.split_last().expect("path non-empty");
    match module_segs {
        [module_id] => self.environment
            .get_module_export(*module_id, export_id)
            .ok_or_else(|| Error::ModuleExportNotFound(*module_id, export_id)),
        _ => Err(Error::ModulePathTooDeep(node.segment_ids.len())),
    }
}
```

Multi-level path support can be added later as the module system matures.

### 6b. `ClauseNode::MethodCall` — runtime dispatch

```rust
ClauseNode::MethodCall(node) => {
    let receiver = self.interpret_clause_expr(&node.receiver)?;
    let mut args = Vec::with_capacity(node.args.len());
    for arg_expr in &node.args {
        args.push(self.interpret_expr(arg_expr)?.get_or_error()?);
    }
    self.dispatch_method(receiver, node.method_id, args)
}
```

`dispatch_method` is a new private method (see §7).

### 6c. `ClauseNode::TypeMethodCall` — type-associated dispatch

```rust
ClauseNode::TypeMethodCall(node) => {
    let mut args = Vec::with_capacity(node.args.len());
    for arg_expr in &node.args {
        args.push(self.interpret_expr(arg_expr)?.get_or_error()?);
    }
    self.dispatch_type_method(node.type_id, node.method_id, args)
}
```

`dispatch_type_method` is a new private method (see §7).

---

## 7. Method Dispatch Tables

**File: `src/interpret/interpreter.rs`** (or a new `src/interpret/dispatch.rs`)

### 7a. Runtime method dispatch

```rust
fn dispatch_method(
    &mut self,
    receiver: Value,
    method_id: Id,
    args: Vec<Value>,
) -> Result<Value, Error> {
    match receiver {
        Value::Array(handle) => {
            let method = ArrayMethod::from_id(method_id)
                .ok_or(Error::UnknownMethod(GcKind::Array, method_id))?;
            method.call(self, handle, args)
        }
        Value::Map(handle) => {
            let method = MapMethod::from_id(method_id)
                .ok_or(Error::UnknownMethod(GcKind::Map, method_id))?;
            method.call(self, handle, args)
        }
        Value::Str(str_id) => {
            let method = StrMethod::from_id(method_id)
                .ok_or(Error::UnknownMethod(GcKind::Str, method_id))?;
            method.call(self, str_id, args)
        }
        other => Err(Error::ValueNotDispatchable(other, method_id)),
    }
}
```

### 7b. Array methods

```rust
enum ArrayMethod { Len, Push, Pop, Insert }

impl ArrayMethod {
    fn from_id(id: Id) -> Option<Self> {
        match id.as_str() {
            "length" => Some(Self::Len),
            "push"   => Some(Self::Push),
            "pop"    => Some(Self::Pop),
            "insert" => Some(Self::Insert),
            _        => None,
        }
    }

    fn call(self, itp: &mut Interpreter, handle: GcHandle, args: Vec<Value>) -> Result<Value, Error> {
        match self {
            Self::Len    => { /* array_len_fn logic */ }
            Self::Push   => { /* array_push_fn logic */ }
            Self::Pop    => { /* array_pop_fn logic */ }
            Self::Insert => { /* array_insert_fn logic */ }
        }
    }
}
```

### 7c. Map methods

```rust
enum MapMethod { Length, Keys, Values, Insert, Remove }
// same pattern as ArrayMethod
```

### 7d. Type-associated dispatch

Initially only supports `Array:new()` and `Map:new()` as shortcuts for empty literals.
Extend as struct/type support grows.

```rust
fn dispatch_type_method(
    &mut self,
    type_id: Id,
    method_id: Id,
    args: Vec<Value>,
) -> Result<Value, Error> {
    match type_id.as_str() {
        "Array" => { /* ArrayTypeMethod::from_id(method_id)?.call(...) */ }
        "Map"   => { /* MapTypeMethod::from_id(method_id)?.call(...) */ }
        _       => Err(Error::UnknownType(type_id)),
    }
}
```

---

## 8. Error Variants

**File: `src/interpret/error.rs`**

New variants needed:

```rust
#[error("Module `{0:?}` has no export `{1:?}`")]
ModuleExportNotFound(Id, Id),

#[error("Module path depth {0} not yet supported")]
ModulePathTooDeep(usize),

#[error("Value `{0:?}` does not support method calls")]
ValueNotDispatchable(Value, Id),

#[error("Unknown method `{1:?}` on type {0:?}")]
UnknownMethod(GcKind, Id),

#[error("Unknown type `{0:?}`")]
UnknownType(Id),

#[error("Invalid target for type-method call")]
InvalidTypeMethodTarget(Span),
```

---

## 9. Prelude Migration

The prelude functions (`array_len`, `array_push`, etc.) are replaced by dot methods.
During transition, both can coexist: the prelude keeps the old names, and the new dot
methods run the same logic internally.

Once dot methods are stable, remove the prelude entries and update `examples/test.lox`.

Migration map:

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

`print`, `assert`, `from_json`, `to_json` stay in the prelude — they have no natural
receiver type.

---

## 10. Implementation Phases

Work bottom-up from the lowest risk, highest value change.

### Phase 1 — `.` method calls (`.` arm in Pratt)

**Files:** `token.rs` (no change), `ast.rs` (+`MethodCallNode`, +`ClauseNode::MethodCall`),
`parser.rs` (`parse_pratt_infix` Dot arm), `interpreter.rs` (`dispatch_method` +
`ArrayMethod` + `MapMethod`), `error.rs` (+new variants).

**Does not break:** identifier parsing, module resolution, subscriptions, anything else.
`parse_identifier_node` still eats dots for now — fix in Phase 2.

**Test:** `arr.push(1); arr.length()` etc. work without touching existing tests.

### Phase 2 — Strip `prefix_ids` from `IdentifierNode`

**Files:** `ast.rs` (simplify struct), `parser.rs` (`parse_identifier_node` no longer
eats dots), `environment.rs` (simplify `get_variable_all_scope` → `get_variable`),
`interpreter.rs` (all callers of `get_variable_all_scope`).

**Breaking:** any code using `node.prefix_ids`, `node.prefixes`, `node.is_simple()`,
`node.create_name()`. Audit all call sites.

**After this phase:** `a.b` where `a` is not a receiver of a method call is a parse
error (dot always requires `()`). The broken module hack is gone.

### Phase 3 — `::` paths and module hierarchy

**Files:** `token.rs` (+`DoubleColon`), `lex.rs` (`::`→`DoubleColon`), `ast.rs`
(+`PathNode`, +`ClauseNode::Path`), `parser.rs` (`parse_primary` path branch),
`environment.rs` (+`get_module_export`), `interpreter.rs` (`ClauseNode::Path` arm),
`error.rs` (+`ModuleExportNotFound`, `ModulePathTooDeep`).

**After this phase:** `import "std/math" as std::math; std::math::sin(x)` works.
`import "foo" as math; math.some_fn()` is now an error (no `.` on module names —
they are paths, not values).

**Future:** nested module maps for `std::math::sin` (three segments) — the module
system would need to be hierarchical. For now, restrict to two segments
(`module::export`) and return `ModulePathTooDeep` for deeper paths.

### Phase 4 — `:` type-associated calls

**Files:** `ast.rs` (+`TypeMethodCallNode`, +`ClauseNode::TypeMethodCall`),
`parser.rs` (`get_binding_power` Colon, `parse_pratt_infix` Colon arm),
`interpreter.rs` (`dispatch_type_method`), `error.rs` (+`UnknownType`,
`InvalidTypeMethodTarget`).

**After this phase:** `Array:new()`, `Map:new()` work. Useful primarily when
struct/class support is added and constructors become `MyType:new(args)`.

---

## 11. Open Questions

- **`.` without `()`** — should `map.key` be field access (sugar for `map["key"]`)
  or always a parse error? For now: always require `()`, revisit when structs arrive.
- **Chaining mutable methods** — `arr.push(1).push(2)` requires `push` to return
  `arr` (the receiver). Currently `array_push_fn` returns `Value::make_nil()`. Change
  mutable array/map methods to return `receiver` for ergonomic chaining?
- **UFCS fallback** — should `arr.my_fn()` fall through to `my_fn(arr)` if no
  built-in method named `my_fn` exists? Enables user-defined "methods" but adds
  ambiguity and spooky action at a distance. Recommendation: no UFCS for now.
- **`:` vs `::` for type methods** — using `Car:new()` risks confusion with
  `mod::export` using `::`. Alternatively, make type methods also use `::` and
  distinguish by whether the left side resolves to a module or a type. Decision
  deferred to when structs are designed.
