# ADR 001: Tree-Walking AST Interpreter

**Date:** 2026-01  
**Status:** Accepted

## Context

Lox interpreters can be implemented at various levels of sophistication:

- **Tree-walk interpreter** — directly executes the AST node by node
- **Bytecode VM** — compiles the AST to a flat instruction stream, then runs a stack machine

Early in development, a VM-based approach was explored. It was removed in favour of a simpler model. The project's goals are learning, experimentation, and language feature exploration — not raw execution performance.

## Decision

Use a tree-walking interpreter. The `Interpreter<'cl, 'sl>` struct in `src/interpret/interpreter.rs` holds a borrowed reference to the `Environment` and the source string (`&'sl str`), and walks the AST recursively via `interpret_stmt()` → `interpret_expr()` → `interpret_clause_expr()`.

No intermediate representation or compilation step exists. The AST produced by `parse()` is executed directly.

## Consequences

**Positive:**
- Significantly simpler implementation — AST semantics map 1:1 to interpreter behaviour.
- Easier to debug: a crash or wrong result points directly to an AST node.
- Faster iteration on new language features (add an AST variant, add a match arm).
- Source spans are always available for error messages without extra bookkeeping.

**Negative:**
- Slower execution than a VM or JIT — every expression evaluation traverses pointer-chased tree nodes.
- No opportunity for compile-time optimisations (constant folding, dead code elimination).

**Future:** Experimenting with a bytecode VM is listed in `docs/todos.md` as a future goal. When that work begins, the tree-walk interpreter can serve as the reference implementation for correctness testing.
