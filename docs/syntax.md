Lox syntax

Syntax point

- `stmt` is the top most level
- `expr` is anything that produces value
- `clause` is a simple expression
- `primary` is a primitive value

```
global_block      = import* stmt*
stmt              = declaration | reassignment | expr
block             = "{" stmt* "}"
function_block    = "{" (return | stmt)* "}"
return            = "return" expr? ";"
import            = "import" UNIX_PATH_STRING "as" IDENTIFIER ";"
declaration       = "var" IDENTIFIER "=" expr ";"
reassignment      = IDENTIFIER "=" expr ";"
expr              = if | while | when | clause
when              = "when" "{" ( clause "->" expr "," ... )* "}"
if                = "if" clause block ("else" if | block)?
while             = "while" clause block
clause            = binary | unary | call | subscription
binary            = clause ( "and" | "or" )          clause
                  | clause ("==" | "!=")             clause
                  | clause ("<" | "<=" | ">" | ">=") clause
                  | clause ("+" | "-")               clause
                  | clause ("*" | "/" | "%")         clause
unary             = ("not" | "-")* unary | primary
call              = clause "(" (clause, "," ...)* ")"
subscription      = clause "[" clause "]"
primary           = IDENTIFIER | group | raw_value
raw_value         = scalar | array_literal | map_literal | function_decl
scalar            = STRING | NUMBER | "true" | "false" | "nil" 
function_decl     = "fn" "(" ( IDENTIFIER "," ... )* ")" (function_block | expr)
array_literal     = "[" (clause, "," ... )* "]" | "[" ":" clause ":" clause "]"
map_literal       = "%{" (primary "=>" clause , ...)* "}" 
group             = "(" clause ")"
```
