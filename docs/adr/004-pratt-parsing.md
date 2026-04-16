# ADR 004: Pratt Parsing for Expression Precedence

**Date:** 2026-02 (commits `4be9bb9`, `ec40475`)  
**Status:** Accepted

## Context

Expression parsing requires handling operator precedence and associativity. Common approaches:

- **Recursive descent with grammar rules** — one function per precedence level (e.g. `parse_term`, `parse_factor`). Simple to understand but verbose and hard to extend.
- **Pratt / precedence climbing** — a single loop driven by numeric or enumerated binding powers. More compact and easier to add new operators.

The original parser used recursive descent with hardcoded precedence. Two refactoring commits migrated binary operators, then function calls and subscriptions, to Pratt parsing.

## Decision

Use Pratt parsing for all infix and postfix expressions. The implementation lives in `src/parse/parser.rs`.

**`BindingPower` enum** (`parser.rs:228`) defines 13 ordered levels:

```
None < LogicalLeft < LogicalRight < EqualityLeft < EqualityRight
    < ComparisonLeft < ComparisonRight < TermLeft < TermRight
    < FactorLeft < FactorRight < ChainLeft < ChainRight
```

The enum's `Ord` derive means binding power comparisons are plain comparisons — no magic numbers.

**`get_binding_power(token)`** (`parser.rs:245`) maps tokens to `(left_bp, right_bp)` pairs:

| Tokens | Left | Right |
|---|---|---|
| `and`, `or` | LogicalLeft | LogicalRight |
| `==`, `!=` | EqualityLeft | EqualityRight |
| `<`, `<=`, `>`, `>=` | ComparisonLeft | ComparisonRight |
| `+`, `-` | TermLeft | TermRight |
| `*`, `/`, `%` | FactorLeft | FactorRight |
| `[`, `(`, `.` | ChainLeft | ChainRight |

**`parse_pratt(state, min_bp)`** (`parser.rs:262`) parses a prefix node, then loops consuming infix operators whose left binding power meets the minimum threshold.

**Postfix operators** (subscriptions `[]`, calls `()`, member access `.`) use `ChainLeft`/`ChainRight` — the highest binding power. Because `ChainRight > ChainLeft`, they are right-associative, which naturally handles chaining: `arr[0](arg)[1]` parses as `((arr[0])(arg))[1]`.

Note: `Token::Dot` is handled as a chain operator in `get_binding_power` but has `unimplemented!()` in `parse_pratt_infix` — identifier chaining (`module.var`) is handled earlier at the identifier parsing level instead.

## Consequences

**Positive:**
- Adding a new operator requires one line in `get_binding_power` and one match arm in `parse_pratt_infix`.
- Precedence is visible in a single table rather than spread across recursive function nesting.
- The first refactor alone removed ~55 lines while making precedence more explicit.

**Negative:**
- Pratt parsing is less familiar to developers accustomed to grammar-rule-based parsers.
- The mix of prefix (`parse_pratt_prefix`) and infix (`parse_pratt_infix`) dispatch requires understanding the algorithm's two phases.
