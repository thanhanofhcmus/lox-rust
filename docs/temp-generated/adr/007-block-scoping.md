# ADR 007: Block-Level Scoping with Readonly-Scope Boundaries

**Date:** 2026-01 (initial: no block scope) → 2026-04 (revised: block scope + ScopeKind) → 2026-04 (revised again: ScopeKind replaced with is_readonly)
**Status:** Accepted (revised)

## Context

The design has gone through two revisions:

**Revision 1 (no block scope):** `push_scope()`/`pop_scope()` were only called at function boundaries. Variables leaked out of `if`/`while` blocks, and loop bodies hit `ReDeclareVariable` on the second iteration.

**Revision 2 (ScopeKind):** Introduced `ScopeKind { Block, Function, Global }`. Block nodes pushed a `Block` scope; function calls pushed a `Function` scope. `replace_variable_function_scope()` walked outward through `Block` scopes but stopped at a `Function` boundary — preventing functions from mutating variables in their caller's frame.

**Revision 3 (is_readonly, current):** `ScopeKind` was removed in favour of a single `is_readonly: bool` flag on each scope. The motivation was to support readonly loop variables (`for x in arr`) and readonly function arguments without adding more enum variants. As a deliberate consequence, the hard `Function` stop in assignment search was also removed — functions can now mutate variables from outer scopes, enabling a closure-like programming style.

## Decision

### Scope structure

Each `Scope` (`src/interpret/environment.rs`) carries a boolean:

```rust
struct Scope {
    variables: HashMap<Id, Value>,
    is_readonly: bool,
}
```

`push_scope(false)` — normal writable scope (block bodies, global).  
`push_scope(true)` — readonly scope (for-loop variable, function arguments).

### Block interpretation

`interpret_block_node()` pushes a writable scope, runs the block, pops:

```rust
fn interpret_block_node(&mut self, node: &BlockNode) -> Result<ValueReturn, Error> {
    self.environment.push_scope(false);
    let result = self.interpret_block_node_inner(node); // no ? — must always reach pop
    self.environment.pop_scope();
    result
}
```

`last_expr` is evaluated inside `interpret_block_node_inner`, before `pop_scope()`, so variables declared in `stmts` are still live when the final expression is computed.

### Function calls — two scopes

A function call pushes **two** scopes:

```
push_scope(true)        ← readonly args scope
  insert arg variables
  interpret_block_node  ← push_scope(false) / pop_scope inside
pop_scope()             ← pops args scope
```

The args scope is readonly, so function arguments cannot be reassigned from inside the function body. The body scope is writable for new local variables.

### For loops — two scopes per iteration

`interpret_for_expr()` uses the same pattern:

```
push_scope(true)                           ← readonly, holds loop variable
  insert_variable_current_scope(iden, v)
  interpret_block_node(body)               ← writable body scope inside
pop_scope()                                ← pops loop var scope
```

The loop variable is in a readonly scope, so assigning to it inside the body (`x = new_value`) is a `VariableReadOnly` error.

### Variable lookup — `get_variable_all_scope`

Reading a variable (`get_variable_all_scope`) returns `(Value, bool /* is_readonly */)`. The readonly flag is `true` for:

- Variables found in a readonly scope (loop vars, function args)
- Prelude built-ins
- Module-exported variables

### Variable assignment — `replace_variable_function_scope`

Assignment checks readonly status first via `get_variable_all_scope`. If the found variable is readonly, a `VariableReadOnly` error is raised before `replace_variable_function_scope` is even called.

`replace_variable_function_scope` itself skips readonly scopes (via `continue`) and searches all writable scopes with no hard stop:

```rust
pub fn replace_variable_function_scope(&mut self, id: Id, value: Value) -> bool {
    for scope in self.scope_stack.iter_outward_mut() {
        if scope.is_readonly {
            continue; // skip readonly scopes (loop vars, args)
        }
        if let Some(old_value) = scope.get_variable(id) {
            self.heap.shallow_copy_value(value);
            self.heap.shallow_dispose_value(old_value);
            scope.insert_variable(id, value);
            return true;
        }
    }
    false
}
```

**This means a function can mutate variables from its enclosing scope or the global scope.** This is intentional — it enables closures-like patterns without requiring a formal closure implementation:

```lox
var counter = 0;
var increment = fn() { counter = counter + 1; };
increment();
# counter is now 1
```

## Consequences

**Positive:**
- Loop variable (`for x in arr`) is readonly inside the body — prevents accidental mutation.
- Function arguments are implicitly readonly — encourages immutable-arg style.
- Prelude and module variables are always readonly — prevents overwriting built-ins.
- Functions can read and mutate outer-scope variables — enables stateful closures without explicit closure syntax.
- `var iden = ...` inside a for body or a function body creates a shadow in the inner scope (no `ReDeclareVariable` error), keeping the outer readonly variable intact.

**Negative:**
- No hard function-call boundary for assignments — a deeply nested function can accidentally mutate a distant global. This requires discipline from the programmer.
- If true closures (capturing by value at definition time) are added later, the current mutable-outer-scope model will conflict and need revisiting.
- The `is_readonly` flag is a single bit with two meanings conflated: "this scope's variables cannot be targeted by assignment search" (structural) and "this particular variable is immutable" (semantic). A future design might separate these.
