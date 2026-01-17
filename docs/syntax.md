Lox syntax

Syntax point

- `stmt` is the top most level
- `expr` is anything that produces value
- `clause` is a simple expression
- `primary` is a primitive value

```
stmt          = reassignment | declaration | expr | if | while | return | import
import        = "import" UNIX_PATH_STRING "as" IDENTIFIER
if            = "if" clause block ("else" block)?
declaration   = "var" IDENTIFIER "=" expr
while         = "while" clause block
return        = "return" (expr)?
block         = "{" (stmt ";")* "}"
reassignment  = IDENTIFIER "=" expr
expr          = clause | ternary | when
ternary       = "cond" clause "then" clause "else" clause
when          = "when" "{" ( "case" clause "->" expr "," ... )* "}"
clause        = logical
logical       = equality (( "and" | "or" ) equality)*
equality      = comparison (("==" | "!=") comparison)*
comparison    = term (("<" | "<" | "<=" | ">=") term)*
term          = factor (("+" | "-") factor)*
factor        = modulo (("*" | "/") modulo)*
modulo        = unary ("%" unary)?
unary         = ("!" | "-")* unary | chaining
chaining      = chaining_base (chaining_part)*
# TODO: review group position here, we put it here to have a higher precedence than unary
chaining_base = identifier | group | primary  
chaining_part = "." identifier | "[" expr "]" | "(" (expr "," ... ) ")"
primary       = STRING | NUMBER | "true" | "false" | "nil" | array_literal | map_literal | function_decl
identifier    = (IDENTIFIER "." ...)*
function_decl = "fn" "(" ( IDENTIFIER "," ... )* ")" (block | expr)
array_literal = "[" (clause, "," ... )* "]" | "[" ":" clause ":" clause "]"
map_literal   = "%{" (primary "=>" expr , ...)* "}"
group         = "(" clause ")"

```
