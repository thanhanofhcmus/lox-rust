Lox syntax

There are 3 syntax recursive point that repeat alot

- `stmt` is the top most level
- `expr` is anything that produces value
- `clause` is a simple expression

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
unary         = ("!" | "-")* unary | primary | chaining
chaining      = identifier | call | index 
call          = chaining "(" (clause "," ...)* ")"
index         = (chaining | array_literal) ("[" clause "]")
primary       = STRING | NUMBER | "true" | "false" | "nil" | group | array_literal | map_literal | function_decl
identifier    = (IDENTIFIER "." ...)*
function_decl = "fn" "(" ( IDENTIFIER "," ... )* ")" (block | expr)
array_literal = "[" (clause, "," ... )* "]" | "[" ":" clause ":" clause "]"
map_literal   = "%{" (primary "=>" expr , ...)* "}"
group         = "(" clause ")"

```
