# Plan: Struct System

**Date:** 2026-04  
**Status:** Draft — not yet implemented  
**Depends on:** `plan-resolution-syntax.md` (`.` method call and `:` type-associated call)

## Design decisions (locked)

1. **`FnDeclNode` gets typed param slots now**, even though all `type_ann` / `return_type` are `None` at runtime. This avoids a full rewrite when type annotations are added.
2. **`self` is a keyword** inside `impl` blocks. Instance vs static is determined at parse time. `self` outside an impl block is a parse error.
3. **`FieldAccess` and `Subscription` are distinct** in the AST and in `ChainingReassignTargetNode`. Never conflated.
4. **`StructInstance` fields are `Vec<(Id, Value)>`**, ordered by declaration. NOT `HashMap` — preserves field order, distinct from `Map`.
5. **Construction validates** that all declared fields are present and no unknown fields are given.
6. **Type registry is split**: `ImplDef` (static, for type checker) and `ImplTable` (runtime, for interpreter).

---

## 1. New Syntax

```
-- struct declaration (top-level only)
struct_decl     = "struct" IDENTIFIER "{" struct_fields "}"
struct_fields   = (IDENTIFIER ",")*

-- impl block (top-level only)
impl_decl       = "impl" IDENTIFIER "{" impl_method* "}"
impl_method     = IDENTIFIER "=" fn_decl

-- updated function declaration (type annotations are optional)
fn_decl         = "fn" "(" param_list ")" ("->" type_expr)? (block | expr)
param_list      = (param ("," param)*)?
param           = "self" | IDENTIFIER (":" type_expr)?
type_expr       = IDENTIFIER                    -- just named types for now

-- struct literal (in expression position)
struct_literal  = IDENTIFIER "{" struct_literal_fields "}"
struct_literal_fields = (struct_literal_field ",")*
struct_literal_field  = IDENTIFIER ("=" clause)?     -- shorthand if no "= clause"

-- struct destructure (in var declaration)
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

## 2. Token Changes

**File: `src/token.rs`**

Add three keywords:

```rust
Struct,   // struct
Impl,     // impl
SelfKw,   // self  (named SelfKw to avoid conflict with Rust's self)
```

**File: `src/parse/lex.rs`**

In the identifier-to-keyword mapping:

```rust
"struct" => Token::Struct,
"impl"   => Token::Impl,
"self"   => Token::SelfKw,
```

`self` is a contextual keyword — the lexer always emits `Token::SelfKw` when it sees `self`.
The parser enforces that it only appears as a parameter inside an impl block.

---

## 3. AST Changes

**File: `src/ast.rs`**

### 3a. `TypeExpr` — stub for future type annotations

```rust
/// Placeholder for type annotations. Currently only named types exist.
/// Will expand to: generics, fn types, union types, etc.
#[derive(Debug, Clone)]
pub enum TypeExpr {
    Named(Span, Id),   // e.g. "Vec3", "f64", "bool"
}
```

### 3b. `ParamNode` — replaces bare `IdentifierNode` in function params

```rust
#[derive(Debug, Clone)]
pub struct ParamNode {
    pub name: Span,
    pub name_id: Id,
    pub is_self: bool,              // true if token was `self`
    pub type_ann: Option<TypeExpr>, // None until type annotations are used
}
```

### 3c. `FnDeclNode` — add param types and return type

```rust
pub struct FnDeclNode {
    pub params: Vec<ParamNode>,           // was arg_names: Vec<IdentifierNode>
    pub return_type: Option<TypeExpr>,    // None = untyped; RTArrow token already exists
    pub body: BlockNode,
}
```

The interpreter reads `params[i].name_id` for binding; ignores `type_ann` and `return_type`.
The type checker (future) reads everything.

### 3d. `StructDeclNode`

```rust
#[derive(Debug, Clone)]
pub struct StructDeclNode {
    pub name: Span,
    pub name_id: Id,
    pub fields: Vec<(Span, Id)>,   // ordered field names
}
```

### 3e. `ImplMethodNode` and `ImplDeclNode`

```rust
#[derive(Debug, Clone)]
pub struct ImplMethodNode {
    pub name: Span,
    pub name_id: Id,
    pub is_instance: bool,   // true if first param is_self
    pub func: FnDeclNode,
}

#[derive(Debug, Clone)]
pub struct ImplDeclNode {
    pub type_name: Span,
    pub type_id: Id,
    pub methods: Vec<ImplMethodNode>,
}
```

### 3f. `StructLiteralNode`

```rust
#[derive(Debug, Clone)]
pub enum StructLiteralField {
    Shorthand(Span, Id),            // `x`   — field and local var share the same name
    Explicit(Span, Id, ClauseNode), // `x = expr`
}

#[derive(Debug, Clone)]
pub struct StructLiteralNode {
    pub type_name: Span,
    pub type_id: Id,
    pub fields: Vec<StructLiteralField>,
}
```

### 3g. `FieldAccessNode`

```rust
#[derive(Debug, Clone)]
pub struct FieldAccessNode {
    pub object: Box<ClauseNode>,
    pub field: Span,
    pub field_id: Id,
}
```

### 3h. Extend `ClauseNode`

```rust
pub enum ClauseNode {
    // existing variants unchanged ...
    Unary(..), Binary(..), RawValue(..), Group(..), Identifier(..),
    Subscription(..), FnCall(..),

    // from plan-resolution-syntax.md:
    Path(PathNode),
    MethodCall(MethodCallNode),
    TypeMethodCall(TypeMethodCallNode),

    // new for structs:
    StructLiteral(StructLiteralNode),
    FieldAccess(FieldAccessNode),
}
```

### 3i. `FollowNode` — typed chain steps for reassignment

```rust
#[derive(Debug, Clone)]
pub enum FollowNode {
    Subscription(ClauseNode),   // [expr]
    FieldAccess(Id),             // .field_name — the Id is resolved at parse time
}
```

### 3j. Update `ChainingReassignTargetNode`

```rust
pub struct ChainingReassignTargetNode {
    pub base: IdentifierNode,
    pub follows: Vec<FollowNode>,   // was Vec<ClauseNode>
}
```

### 3k. `DeclareTarget` — handle struct destructuring in `var`

```rust
#[derive(Debug, Clone)]
pub enum StructDestructureField {
    Shorthand(Span, Id),              // `x`        — binds local `x` from field `x`
    Rename(Span, Id, Span, Id),       // `x = my_x` — binds local `my_x` from field `x`
}

#[derive(Debug, Clone)]
pub struct StructDestructureNode {
    pub type_name: Span,
    pub type_id: Id,
    pub fields: Vec<StructDestructureField>,
}

#[derive(Debug, Clone)]
pub enum DeclareTarget {
    Ident(IdentifierNode),
    StructDestructure(StructDestructureNode),
}
```

### 3l. Update `Statement::Declare`

```rust
pub enum Statement {
    Declare(DeclareTarget, Expression),   // was (IdentifierNode, Expression)
    ReassignIden(ChainingReassignTargetNode, Expression),
    Expr(Expression),
    StructDecl(StructDeclNode),           // new
    ImplDecl(ImplDeclNode),               // new
}
```

---

## 4. Parser Changes

**File: `src/parse/parser.rs`**

### 4a. `parse_function_decl` — typed params and optional return type

```rust
fn parse_function_decl(state: &mut Context) -> Result<FnDeclNode, ParseError> {
    state.consume_token(Token::Fn)?;
    let params = parse_comma_list(state, Token::LRoundParen, Token::RRoundParen, parse_param_node)?;

    let return_type = if state.peek(&[Token::RTArrow]) {
        state.consume_token(Token::RTArrow)?;
        Some(parse_type_expr(state)?)
    } else {
        None
    };

    let body = if state.peek(&[Token::LPointParen]) {
        state.is_in_fn = true;
        let b = parse_block_node(state)?;
        state.is_in_fn = false;
        b
    } else {
        let expr = parse_expr(state)?;
        BlockNode { stmts: vec![], last_expr: Some(Box::new(expr)) }
    };

    Ok(FnDeclNode { params, return_type, body })
}
```

### 4b. `parse_param_node` — handles `self` keyword and optional type annotation

```rust
fn parse_param_node(state: &mut Context) -> Result<ParamNode, ParseError> {
    if state.peek(&[Token::SelfKw]) {
        let li = state.consume_token(Token::SelfKw)?;
        // optional type annotation on self: `self: Vec3`
        let type_ann = if state.peek(&[Token::Colon]) {
            state.consume_token(Token::Colon)?;
            Some(parse_type_expr(state)?)
        } else {
            None
        };
        return Ok(ParamNode {
            name: li.span,
            name_id: Id::new("self"),
            is_self: true,
            type_ann,
        });
    }

    let li = state.consume_token(Token::Identifier)?;
    let type_ann = if state.peek(&[Token::Colon]) {
        state.consume_token(Token::Colon)?;
        Some(parse_type_expr(state)?)
    } else {
        None
    };

    Ok(ParamNode {
        name: li.span,
        name_id: Id::new(li.span.str_from_source(state.get_input())),
        is_self: false,
        type_ann,
    })
}
```

`parse_type_expr` for now: just consume an `Identifier` and return `TypeExpr::Named(span, id)`.

### 4c. `parse_struct_decl`

Top-level, called from `parse_stmt` when `Token::Struct` is seen:

```rust
fn parse_struct_decl(state: &mut Context) -> Result<Statement, ParseError> {
    state.consume_token(Token::Struct)?;
    let name_li = state.consume_token(Token::Identifier)?;
    let name_id = Id::new(name_li.span.str_from_source(state.get_input()));

    let field_lis = parse_comma_list(
        state, Token::LPointParen, Token::RPointParen,
        |s| s.consume_token(Token::Identifier),
    )?;
    let fields = field_lis
        .into_iter()
        .map(|li| (li.span, Id::new(li.span.str_from_source(state.get_input()))))
        .collect();

    Ok(Statement::StructDecl(StructDeclNode { name: name_li.span, name_id, fields }))
}
```

### 4d. `parse_impl_decl`

```rust
fn parse_impl_decl(state: &mut Context) -> Result<Statement, ParseError> {
    state.consume_token(Token::Impl)?;
    let name_li = state.consume_token(Token::Identifier)?;
    let type_id = Id::new(name_li.span.str_from_source(state.get_input()));

    state.consume_token(Token::LPointParen)?;  // {

    let mut methods = vec![];
    while !state.peek(&[Token::RPointParen]) {
        let method_li = state.consume_token(Token::Identifier)?;
        let method_id = Id::new(method_li.span.str_from_source(state.get_input()));
        state.consume_token(Token::Equal)?;

        // mark that we're inside an impl so `self` is valid
        state.is_in_impl = true;
        let func = parse_function_decl(state)?;
        state.is_in_impl = false;

        let is_instance = func.params.first().map(|p| p.is_self).unwrap_or(false);

        methods.push(ImplMethodNode {
            name: method_li.span,
            name_id: method_id,
            is_instance,
            func,
        });
    }

    state.consume_token(Token::RPointParen)?;  // }

    Ok(Statement::ImplDecl(ImplDeclNode {
        type_name: name_li.span,
        type_id,
        methods,
    }))
}
```

`Context` needs a new field `is_in_impl: bool`. `parse_param_node` checks `state.is_in_impl`
when it sees `Token::SelfKw` — if false, return `ParseError::SelfOutsideImpl(span)`.

### 4e. `parse_primary` — detect struct literals

After consuming an `Identifier`, peek for `{` (`LPointParen`):

```rust
fn parse_primary(state: &mut Context) -> Result<ClauseNode, ParseError> {
    if state.peek(&[Token::Identifier]) {
        let li = state.consume_token(Token::Identifier)?;
        let name_id = Id::new(li.span.str_from_source(state.get_input()));

        if state.peek(&[Token::DoubleColon]) {
            // path: a::b::c  (from plan-resolution-syntax.md)
            return parse_path_from(state, li);
        }

        if state.peek(&[Token::LPointParen]) {
            // struct literal: Vec3 { x, y = expr, z }
            return parse_struct_literal(state, li.span, name_id);
        }

        return Ok(ClauseNode::Identifier(IdentifierNode::new(li.span, state.get_input())));
    }
    // ... group, raw_value unchanged
}
```

`parse_struct_literal` consumes `{`, then a comma list of `parse_struct_literal_field`, then `}`.

```rust
fn parse_struct_literal_field(state: &mut Context) -> Result<StructLiteralField, ParseError> {
    let field_li = state.consume_token(Token::Identifier)?;
    let field_id = Id::new(field_li.span.str_from_source(state.get_input()));

    if state.peek(&[Token::Equal]) {
        state.consume_token(Token::Equal)?;
        let expr = parse_clause_node(state)?;
        Ok(StructLiteralField::Explicit(field_li.span, field_id, expr))
    } else {
        Ok(StructLiteralField::Shorthand(field_li.span, field_id))
    }
}
```

### 4f. `parse_pratt_infix` — `Token::Dot`: field access vs method call

```rust
Token::Dot => {
    state.consume_token(Token::Dot)?;
    let name_li = state.consume_token(Token::Identifier)?;
    let name_id = Id::new(name_li.span.str_from_source(state.get_input()));

    if state.peek(&[Token::LRoundParen]) {
        // method call: receiver.method(args)
        let args = parse_comma_list(state, Token::LRoundParen, Token::RRoundParen, parse_expr)?;
        Ok(ClauseNode::MethodCall(MethodCallNode {
            receiver: Box::new(lhs),
            method_name: name_li.span,
            method_id: name_id,
            args,
        }))
    } else {
        // field access: receiver.field  (no parens)
        Ok(ClauseNode::FieldAccess(FieldAccessNode {
            object: Box::new(lhs),
            field: name_li.span,
            field_id: name_id,
        }))
    }
}
```

### 4g. `convert_chaining` — add `FieldAccess` to `FollowNode`

```rust
fn convert_chaining(node: Expression) -> Result<ChainingReassignTargetNode, ParseError> {
    // ...
    loop {
        match clause {
            ClauseNode::Subscription(SubscriptionNode { indexer, indexee }) => {
                index_chain.push(FollowNode::Subscription(*indexee));
                clause = *indexer;
            }
            ClauseNode::FieldAccess(FieldAccessNode { object, field_id, .. }) => {
                index_chain.push(FollowNode::FieldAccess(field_id));
                clause = *object;
            }
            v => {
                // base case: must be an Identifier
                // push as marker, handled below
                index_chain.push(FollowNode::Subscription(v)); // reuse to carry the base
                break;
            }
        }
    }
    // ... pop base, verify it's an Identifier, reverse chain
}
```

(The base case handling is a bit awkward with the unified stack; the actual implementation
can keep a separate `base` accumulator to avoid this.)

### 4h. `parse_declaration` — struct destructuring in `var`

```rust
fn parse_declaration(state: &mut Context) -> Result<Statement, ParseError> {
    state.consume_token(Token::Var)?;

    let target = if state.peek_ahead_is_struct_destructure() {
        // var Vec3 { x, y = my_y } = expr
        let name_li = state.consume_token(Token::Identifier)?;
        let type_id = Id::new(...);
        let fields = parse_comma_list(state, Token::LPointParen, Token::RPointParen,
            parse_struct_destr_field)?;
        DeclareTarget::StructDestructure(StructDestructureNode { type_name: name_li.span, type_id, fields })
    } else {
        let id_li = state.consume_token(Token::Identifier)?;
        DeclareTarget::Ident(IdentifierNode::new(id_li.span, state.get_input()))
    };

    state.consume_token(Token::Equal)?;
    let expr = parse_expr(state)?;
    state.consume_token(Token::Semicolon)?;
    Ok(Statement::Declare(target, expr))
}
```

`peek_ahead_is_struct_destructure`: peek two tokens ahead — if `Identifier` then `{`, it's
a struct destructure. This lookahead is unambiguous because `var { ... }` (no name before
`{`) is invalid otherwise, and `var x = ...` starts with `Identifier` then `=` not `{`.

```rust
fn parse_struct_destr_field(state: &mut Context) -> Result<StructDestructureField, ParseError> {
    let field_li = state.consume_token(Token::Identifier)?;
    let field_id = Id::new(...);
    if state.peek(&[Token::Equal]) {
        state.consume_token(Token::Equal)?;
        let local_li = state.consume_token(Token::Identifier)?;
        let local_id = Id::new(...);
        Ok(StructDestructureField::Rename(field_li.span, field_id, local_li.span, local_id))
    } else {
        Ok(StructDestructureField::Shorthand(field_li.span, field_id))
    }
}
```

---

## 5. Runtime Representation

**File: `src/interpret/heap.rs`**

### 5a. `GcKind` and `GcObject`

```rust
pub enum GcKind {
    Array,
    Map,
    Function,
    Struct,   // new
}

pub enum GcObject {
    Array(Array),
    Map(Map),
    Function(Function),
    Struct(StructInstance),  // new
}
```

### 5b. `StructInstance`

```rust
pub struct StructInstance {
    pub type_id: Id,
    pub fields: Vec<(Id, Value)>,   // ordered by struct declaration
}

impl StructInstance {
    pub fn get_field(&self, field_id: Id) -> Option<Value> {
        self.fields.iter().find(|(id, _)| *id == field_id).map(|(_, v)| *v)
    }

    pub fn replace_field(&mut self, field_id: Id, new_value: Value) -> Option<Value> {
        self.fields.iter_mut()
            .find(|(id, _)| *id == field_id)
            .map(|(_, v)| std::mem::replace(v, new_value))
    }
}
```

**File: `src/interpret/values/value.rs`**

### 5c. `Value` — add `Struct` variant

```rust
pub enum Value {
    // existing ...
    Struct(GcHandle),   // new
}
```

Update `get_handle` and `replace_handle` to include `Value::Struct`:

```rust
pub fn get_handle(self) -> Option<GcHandle> {
    match self {
        Value::Array(h) | Value::Map(h) | Value::Function(h) | Value::Struct(h) => Some(h),
        _ => None,
    }
}
```

`deep_eq` for structs: field-by-field comparison using `type_id` first (must be same type),
then compare each field in declaration order.

---

## 6. Type Registry

**File: `src/interpret/type_registry.rs`** (new file)

```rust
use std::collections::HashMap;
use crate::{ast::TypeExpr, id::Id, interpret::values::Value};

pub struct StructDef {
    pub name: Id,
    pub fields: Vec<Id>,              // ordered field names
}

pub struct MethodDef {
    pub is_instance: bool,
    pub param_types: Vec<Option<TypeExpr>>,  // None = untyped
    pub return_type: Option<TypeExpr>,
}

/// Static layer — for the future type checker.
pub struct ImplDef {
    pub methods: HashMap<Id, MethodDef>,
}

/// Runtime layer — for the interpreter.
pub struct ImplTable {
    pub methods: HashMap<Id, Value>,         // Value::Function or Value::BuiltinFunction
}

pub struct TypeRegistry {
    pub struct_defs: HashMap<Id, StructDef>,
    pub impl_defs: HashMap<Id, ImplDef>,     // future type checker reads this
    pub impl_tables: HashMap<Id, ImplTable>, // interpreter reads this
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self {
            struct_defs: HashMap::new(),
            impl_defs: HashMap::new(),
            impl_tables: HashMap::new(),
        }
    }

    pub fn register_struct(&mut self, def: StructDef) {
        self.struct_defs.insert(def.name, def);
    }

    pub fn register_impl(&mut self, type_id: Id, method_name: Id,
                         method_def: MethodDef, fn_value: Value) {
        self.impl_defs.entry(type_id).or_insert_with(|| ImplDef {
            methods: HashMap::new()
        }).methods.insert(method_name, method_def);

        self.impl_tables.entry(type_id).or_insert_with(|| ImplTable {
            methods: HashMap::new()
        }).methods.insert(method_name, fn_value);
    }

    pub fn get_method(&self, type_id: Id, method_id: Id) -> Option<Value> {
        self.impl_tables.get(&type_id)?.methods.get(&method_id).copied()
    }

    pub fn get_struct_def(&self, type_id: Id) -> Option<&StructDef> {
        self.struct_defs.get(&type_id)
    }
}
```

Add to `Environment`:

```rust
pub struct Environment {
    // existing ...
    pub(super) type_registry: TypeRegistry,
}
```

---

## 7. Interpreter Changes

**File: `src/interpret/interpreter.rs`**

### 7a. `interpret_stmt` — handle new statement variants

```rust
Statement::StructDecl(node) => self.interpret_struct_decl(node),
Statement::ImplDecl(node)   => self.interpret_impl_decl(node),
Statement::Declare(target, expr) => self.interpret_declare_stmt_with_target(target, expr),
```

### 7b. `interpret_struct_decl`

```rust
fn interpret_struct_decl(&mut self, node: &StructDeclNode) -> Result<ValueReturn, Error> {
    if self.environment.type_registry.get_struct_def(node.name_id).is_some() {
        return Err(Error::StructAlreadyDefined(node.name_id));
    }
    self.environment.type_registry.register_struct(StructDef {
        name: node.name_id,
        fields: node.fields.iter().map(|(_, id)| *id).collect(),
    });
    Ok(ValueReturn::none())
}
```

### 7c. `interpret_impl_decl`

```rust
fn interpret_impl_decl(&mut self, node: &ImplDeclNode) -> Result<ValueReturn, Error> {
    if self.environment.type_registry.get_struct_def(node.type_id).is_none() {
        return Err(Error::ImplForUnknownStruct(node.type_id));
    }
    for method in &node.methods {
        let fn_value = self.environment.insert_function_variable(Function {
            arg_ids: method.func.params.iter().map(|p| p.name_id).collect(),
            body: method.func.body.clone(),
        });
        let method_def = MethodDef {
            is_instance: method.is_instance,
            param_types: method.func.params.iter()
                .map(|p| p.type_ann.clone())
                .collect(),
            return_type: method.func.return_type.clone(),
        };
        self.environment.type_registry.register_impl(
            node.type_id, method.name_id, method_def, fn_value,
        );
    }
    Ok(ValueReturn::none())
}
```

### 7d. `interpret_struct_literal`

```rust
fn interpret_struct_literal(&mut self, node: &StructLiteralNode) -> Result<Value, Error> {
    let type_id = node.type_id;
    let struct_def = self.environment.type_registry
        .get_struct_def(type_id)
        .ok_or(Error::UnknownStructType(type_id))?
        .clone();

    let mut field_map: HashMap<Id, Value> = HashMap::new();
    for field in &node.fields {
        let (field_id, value) = match field {
            StructLiteralField::Shorthand(_, id) => {
                let val = self.environment.get_variable(*id)
                    .ok_or(Error::NotFoundVariable(...))?
                    .0;
                (*id, val)
            }
            StructLiteralField::Explicit(_, id, expr) => {
                let val = self.interpret_clause_expr(expr)?;
                (*id, val)
            }
        };
        if !struct_def.fields.contains(&field_id) {
            return Err(Error::StructUnknownField(type_id, field_id));
        }
        if field_map.contains_key(&field_id) {
            return Err(Error::StructDuplicateField(type_id, field_id));
        }
        field_map.insert(field_id, value);
    }

    // Validate: all fields present
    for declared_field in &struct_def.fields {
        if !field_map.contains_key(declared_field) {
            return Err(Error::StructMissingField(type_id, *declared_field));
        }
    }

    // Build in declaration order
    let fields: Vec<(Id, Value)> = struct_def.fields.iter()
        .map(|id| (*id, field_map[id]))
        .collect();

    // Increment ref counts before inserting
    for (_, v) in &fields {
        self.environment.heap.shallow_copy_value(*v);
    }

    Ok(self.environment.insert_struct_variable(StructInstance { type_id, fields }))
}
```

### 7e. `interpret_field_access`

```rust
fn interpret_field_access(&mut self, node: &FieldAccessNode) -> Result<Value, Error> {
    let object = self.interpret_clause_expr(&node.object)?;
    match object {
        Value::Struct(handle) => {
            let inst = self.environment.get_struct(handle)?;
            inst.get_field(node.field_id)
                .ok_or(Error::StructFieldNotFound(inst.type_id, node.field_id))
        }
        other => Err(Error::ValueNotAStruct(other, node.field_id)),
    }
}
```

### 7f. `interpret_reassign_chaining` — handle `FollowNode::FieldAccess`

The existing `deep_copy_reassign_object` in `heap.rs` works by calling `get_at_index` /
`replace_at_index` on `GcObject`. For struct field access, add:

```rust
impl GcObject {
    pub fn get_at_field(&self, field_id: Id) -> Result<Value, Error> {
        match self {
            GcObject::Struct(inst) => inst.get_field(field_id)
                .ok_or(Error::StructFieldNotFound(inst.type_id, field_id)),
            _ => Err(Error::GcObjectNotAStruct(self.get_kind())),
        }
    }

    pub fn replace_at_field(&mut self, field_id: Id, new_value: Value) -> Result<Value, Error> {
        match self {
            GcObject::Struct(inst) => inst.replace_field(field_id, new_value)
                .ok_or(Error::StructFieldNotFound(inst.type_id, field_id)),
            _ => Err(Error::GcObjectNotAStruct(self.get_kind())),
        }
    }
}
```

Extend `deep_copy_reassign_object` to accept `FollowNode` instead of raw `Value` for the
chain steps. Each step dispatches on `FollowNode`:
- `FollowNode::Subscription(indexee_value)` → existing `get_at_index` / `replace_at_index`
- `FollowNode::FieldAccess(field_id)` → `get_at_field` / `replace_at_field`

### 7g. `interpret_method_call` — structs

Extend `dispatch_method` from `plan-resolution-syntax.md`:

```rust
Value::Struct(handle) => {
    let inst = self.environment.heap.get_object(handle)
        .and_then(|o| if let GcObject::Struct(s) = o { Some(s) } else { None })
        .ok_or(Error::GcObjectNotFound(handle))?;
    let type_id = inst.type_id;
    let fn_value = self.environment.type_registry
        .get_method(type_id, method_id)
        .ok_or(Error::MethodNotFound(type_id, method_id))?;
    // prepend receiver as first argument (self binding)
    let mut full_args = vec![Value::Struct(handle)];
    full_args.extend(args);
    self.call_function(fn_value, full_args)
}
```

### 7h. `interpret_type_method_call` — struct static methods

```rust
let fn_value = self.environment.type_registry
    .get_method(node.type_id, node.method_id)
    .ok_or(Error::MethodNotFound(node.type_id, node.method_id))?;
// no receiver prepended — static method
self.call_function(fn_value, evaluated_args)
```

### 7i. `interpret_declare` — struct destructuring

```rust
DeclareTarget::StructDestructure(destr_node) => {
    let value = self.interpret_expr(expr)?.get_or_error()?;
    let Value::Struct(handle) = value else {
        return Err(Error::DestructureTargetNotAStruct(value));
    };
    let inst = self.environment.get_struct(handle)?.clone();
    if inst.type_id != destr_node.type_id {
        return Err(Error::DestructureTypeMismatch(destr_node.type_id, inst.type_id));
    }
    for field in &destr_node.fields {
        let (field_id, local_id) = match field {
            StructDestructureField::Shorthand(_, id) => (*id, *id),
            StructDestructureField::Rename(_, field_id, _, local_id) => (*field_id, *local_id),
        };
        let field_value = inst.get_field(field_id)
            .ok_or(Error::StructFieldNotFound(inst.type_id, field_id))?;
        self.environment.insert_variable_current_scope(local_id, field_value);
    }
}
```

---

## 8. GC Changes

**File: `src/interpret/heap.rs`**

### 8a. `mark` — walk struct fields

In the `match &entry.object` block of the mark traversal:

```rust
GcObject::Struct(inst) => {
    inst.fields.iter()
        .filter_map(|(_, v)| v.get_str_id())
        .for_each(|id| self.string_interner.mark_to_keep(id));
    trace_list.extend(inst.fields.iter().filter_map(|(_, v)| v.get_handle()));
}
```

### 8b. `shallow_dispose_value` — dispose struct fields

```rust
GcObject::Struct(inst) => {
    for (_, value) in inst.fields.clone() {
        self.shallow_dispose_value(value);
    }
}
```

### 8c. `insert_struct_variable` in `Environment`

Use the `decl_gc_type_methods!` macro or add manually:

```rust
pub fn insert_struct_variable(&mut self, data: StructInstance) -> Value {
    let handle = self.heap.insert_object(GcObject::Struct(data));
    Value::Struct(handle)
}

pub fn get_struct(&self, handle: GcHandle) -> Result<&StructInstance, Error> {
    match self.heap.get_object(handle) {
        Some(GcObject::Struct(s)) => Ok(s),
        Some(obj) => Err(Error::GcObjectWrongType(handle, obj.get_kind(), GcKind::Struct)),
        None => Err(Error::GcObjectNotFound(handle)),
    }
}
```

---

## 9. Error Variants

**File: `src/interpret/error.rs`**

```rust
#[error("Struct `{0:?}` is already defined")]
StructAlreadyDefined(Id),

#[error("`impl` for unknown struct `{0:?}`")]
ImplForUnknownStruct(Id),

#[error("Struct `{0:?}` is missing field `{1:?}` in construction")]
StructMissingField(Id, Id),

#[error("Struct `{0:?}` has no field `{1:?}`")]
StructUnknownField(Id, Id),

#[error("Struct `{0:?}` field `{1:?}` provided more than once")]
StructDuplicateField(Id, Id),

#[error("Field access `.{1:?}` on non-struct value `{0:?}`")]
ValueNotAStruct(Value, Id),

#[error("Struct `{0:?}` has no field `{1:?}`")]
StructFieldNotFound(Id, Id),

#[error("Method `{1:?}` not found on type `{0:?}`")]
MethodNotFound(Id, Id),

#[error("Unknown struct type `{0:?}`")]
UnknownStructType(Id),

#[error("`self` used outside an impl block")]
SelfOutsideImpl(Span),

#[error("Destructure target is not a struct: `{0:?}`")]
DestructureTargetNotAStruct(Value),

#[error("Destructure expected `{0:?}` but got `{1:?}`")]
DestructureTypeMismatch(Id, Id),

#[error("GC object is not a struct (got {0:?})")]
GcObjectNotAStruct(GcKind),
```

**File: `src/parse/error.rs`**

```rust
#[error("`self` used outside an impl block")]
SelfOutsideImpl(Span),

#[error("Expected identifier as reassignment base")]
ReassignRootIsNotAnIdentifier,  // already exists, keep
```

---

## 10. Display / Debug

**File: `src/interpret/values/display_writer.rs`** (or `value.rs`)

Add `Value::Struct` to `write_display`:

```rust
Value::Struct(handle) => {
    let inst = env.get_struct(handle)?;
    let type_name = inst.type_id;  // TODO: recover name string from type registry
    write!(w, "{}{{", /* type name */)?;
    for (i, (field_id, field_val)) in inst.fields.iter().enumerate() {
        if i > 0 { write!(w, ", ")?; }
        write!(w, "{:?}=", field_id)?;
        field_val.write_display(env, w)?;
    }
    write!(w, "}}")?;
    Ok(())
}
```

Note: `Id` is a hash — recovering the name string for display requires the type registry.
Pass `env` (which contains `type_registry`) or store the name string in `StructDef`
and look it up. Recommended: store `name_str: String` in `StructDef` alongside `name: Id`.

---

## 11. Implementation Phases

### Phase 1 — `FnDeclNode` refactor (no new features, pure rename)

**Files:** `ast.rs` (`ParamNode`, `TypeExpr`, update `FnDeclNode`), `parser.rs`
(`parse_function_decl`, `parse_param_node`, `parse_type_expr`), `interpreter.rs`
(all callers of `arg_ids` on `Function` — these use `Id` already so minimal change).

**Risk:** touches every function call. Run the full test suite after.

### Phase 2 — Struct and impl declarations

**Files:** `token.rs` (+`Struct`, `Impl`, `SelfKw`), `lex.rs` (keyword mapping),
`ast.rs` (`StructDeclNode`, `ImplDeclNode`, `ImplMethodNode`, `Statement` variants),
`parse/context.rs` (+`is_in_impl`), `parser.rs` (`parse_struct_decl`, `parse_impl_decl`,
`parse_param_node` self-check), `type_registry.rs` (new file), `environment.rs`
(+`type_registry` field), `interpreter.rs` (`interpret_struct_decl`, `interpret_impl_decl`),
`error.rs` (+variants).

**Does not break:** no new expression forms yet. Struct and impl are statement-level only.

### Phase 3 — Struct construction and field access

**Files:** `ast.rs` (`StructLiteralNode`, `FieldAccessNode`, `ClauseNode` variants),
`parser.rs` (`parse_primary` struct-literal branch, `parse_struct_literal`,
`parse_struct_literal_field`, Dot arm in `parse_pratt_infix`), `heap.rs`
(`GcObject::Struct`, `StructInstance`, `get_at_field`, `replace_at_field`, mark/dispose),
`values/value.rs` (`Value::Struct`, `get_handle`, `replace_handle`, `deep_eq`, `write_display`),
`environment.rs` (`insert_struct_variable`, `get_struct`), `interpreter.rs`
(`interpret_struct_literal`, `interpret_field_access`, `ClauseNode::StructLiteral` and
`ClauseNode::FieldAccess` arms).

**Test:** `var v = Vec3:new(1, 2, 3); print(v.x);` should work end-to-end.

### Phase 4 — Field assignment and method dispatch

**Files:** `ast.rs` (`FollowNode`, update `ChainingReassignTargetNode`), `parser.rs`
(`convert_chaining` field-access arm), `heap.rs` (update `deep_copy_reassign_object` to
take `&[FollowNode]` instead of `&[Value]`), `interpreter.rs` (`dispatch_method` struct arm,
`dispatch_type_method` struct arm, updated `interpret_reassign_chaining`).

**Test:** `v.x = 5.0;` and `v1.dot(v2)` and `Vec3:new(...)` all work.

### Phase 5 — Destructuring

**Files:** `ast.rs` (`StructDestructureNode`, `StructDestructureField`, `DeclareTarget`),
`parser.rs` (`parse_declaration` + `peek_ahead_is_struct_destructure` +
`parse_struct_destr_field`), `interpreter.rs` (`interpret_declare` with `DeclareTarget`).

**Test:** `var Vec3 { x, y, z } = v1;` binds `x`, `y`, `z` correctly.

---

## 12. Open Questions (deferred)

- **Struct equality**: field-by-field (like Array) or reference-only? Recommendation:
  field-by-field with same-type check first. Can be overridden later via a trait.
- **Multiple `impl` blocks**: allowed (merge methods) or error? Recommendation: merge,
  error on duplicate method name.
- **`self` type annotation requirement**: should `fn(self: Vec3, ...)` eventually be
  required? Leave optional for now; type checker can enforce later.
- **`.` on maps**: `map.key` (no parens) as sugar for `map["key"]`? Not in scope for this
  plan — keep `map["key"]` as the only map access syntax for now to avoid ambiguity.
- **Trait/impl-for-trait syntax**: `impl Vec3: Printable { ... }` — deferred until
  traits are designed.
- **`Id` to name string for display**: store `name_str: String` in `StructDef`.
  `Id::debug_name()` or similar could be added globally later.
