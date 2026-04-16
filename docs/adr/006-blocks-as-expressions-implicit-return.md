# ADR 006: Blocks Are Expressions with Implicit Return

**Date:** 2026-01 (marked complete in `docs/todos.md`)  
**Status:** Accepted

## Context

In the original Lox, functions require an explicit `return` statement to produce a value. Control flow constructs (`if`, `while`) are statements with no value. This forces verbose patterns like:

```lox
var result;
if (cond) { result = 1; } else { result = 2; }
```

Rust-style languages treat blocks as expressions: the last expression in a block, if not followed by a semicolon, is the value of the block. This allows:

```rust
let result = if cond { 1 } else { 2 };
```

## Decision

Blocks (`{ ... }`) are expressions that evaluate to the value of their last un-semicoloned statement. This is represented in the AST via `BlockNode` (`src/ast.rs:26`):

```rust
pub struct BlockNode {
    pub stmts: StatementList,          // all but (possibly) the last
    pub last_expr: Option<Box<Expression>>, // the implicit return value, if any
}
```

**Parser logic** (`src/parse/parser.rs`): when parsing a block, if the final item is an `Expression` statement with no trailing semicolon, it is pulled out and placed into `last_expr` rather than `stmts`.

**Interpreter**: `interpret_block_node()` pushes a `Block` scope, runs all `stmts` via `interpret_block_node_inner()`, evaluates `last_expr` if present (still inside the scope so variables are live), then pops the scope before returning the value. See ADR 007 for the scoping design.

**Function shorthand**: `fn(args) = expr` is syntactic sugar that wraps `expr` as the sole `last_expr` of the function body — no braces or `return` needed.

**`return` keyword** still works and short-circuits the block, propagated via a `ValueReturn { value, should_bubble: true }` wrapper that unwinds through nested calls.

## Consequences

**Positive:**
- Functions can omit `return` for single-expression bodies: `var double = fn(x) = x * 2`.
- `if`/`when`/`while` blocks naturally produce values, enabling assignment from control flow.
- Consistent with the rest of the language's expression-oriented design.

**Negative:**
- The parser must distinguish "expression statement with semicolon" from "expression that is the block's value" — subtle and easy to get wrong.
- The parser must carefully separate `stmts` from `last_expr` — a missing semicolon changes a statement into the block's return value.
