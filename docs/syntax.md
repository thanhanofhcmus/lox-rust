Lox syntax

Syntax point

- `stmt` is the top most level
- `expr` is anything that produces value
- `clause` is a simple expression
- `primary` is a primitive value

```
global_block      = import* stmt*
stmt              = while | if | stmt_block | declaration | reassignment | expr
stmt_block        = "{" stmt* "}"
expr_block        = "{" (yield | stmt)* yield "}"
function_block    = "{" (return | stmt)* "}"
return            = "return" expr? ";"
yield             = "yield" expr ";"
import            = "import" UNIX_PATH_STRING "as" IDENTIFIER ";"
while             = "while" clause stmt_block
if                = "if" clause stmt_block ("else" stmt_block)?
declaration       = "var" IDENTIFIER "=" expr ";"
reassignment      = IDENTIFIER "=" expr ";"
expr              = ternary | when | expr_block | clause
ternary           = "cond" clause "then" clause "else" clause
when              = "when" "{" ( clause "->" expr "," ... )* "}"
clause            = logical
logical           = equality (( "and" | "or" ) equality)*
equality          = comparison (("==" | "!=") comparison)*
comparison        = term (("<" | "<" | "<=" | ">=") term)*
term              = factor (("+" | "-") factor)*
factor            = modulo (("*" | "/") modulo)*
modulo            = unary ("%" unary)?
unary             = ("!" | "-")* unary | chaining
chaining          = chaining_base (chaining_part)*
# TODO: review group position here, we put it here to have a higher precedence than unary
chaining_base     = identifier | group | primary  
chaining_part     = "." identifier | "[" expr "]" | "(" (expr "," ... ) ")"
primary           = STRING | NUMBER | "true" | "false" | "nil" | array_literal | map_literal | function_decl
identifier        = (IDENTIFIER "." ...)*
function_decl     = "fn" "(" ( IDENTIFIER "," ... )* ")" (function_block | expr)
array_literal     = "[" (clause, "," ... )* "]" | "[" ":" clause ":" clause "]"
map_literal       = "%{" (primary "=>" expr , ...)* "}" 
group             = "(" clause ")"
```
