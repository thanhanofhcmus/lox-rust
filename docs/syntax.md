Lox syntax

Syntax point

- `stmt` is the top most level
- `expr` is anything that produces value
- `clause` is a simple expression
- `primary` is a primitive value

```
stmt          = import | while | if | block_stmt | return | declaration | reassignment |  expr
import        = "import" UNIX_PATH_STRING "as" IDENTIFIER ";"
while         = "while" clause block_stmt
if            = "if" clause block_stmt ("else" block_stmt)?
block_stmt    = "{" stmt* "}"
return        = "return" (expr)? ";"
declaration   = "var" IDENTIFIER "=" expr ";"
reassignment  = IDENTIFIER "=" expr ";"
expr          = ternary | when | block_expr | clause
ternary       = "cond" clause "then" clause "else" clause
when          = "when" "{" ( clause "->" expr "," ... )* "}"
block_expr    = "{" (stmt ";")* "yield" expr; "}"
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
function_decl = "fn" "(" ( IDENTIFIER "," ... )* ")" (block_stmt | expr)
array_literal = "[" (clause, "," ... )* "]" | "[" ":" clause ":" clause "]"
map_literal   = "%{" (primary "=>" expr , ...)* "}" 
group         = "(" clause ")"
```
