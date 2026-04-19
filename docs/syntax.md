Lox syntax

Syntax point

- `stmt` is the top most level
- `expr` is anything that produces value
- `clause` is a simple expression
- `primary` is a primitive value

```
global_block      = import* stmt*
stmt              = declaration | reassignment | struct_decl | expr
struct_decl       = "struct" "{" (struct_field_decl "," ...)* "}"
struct_field_decl = IDENTIFIER (":"  TYPE_IDENTIFIER)?
block             = "{" stmt* "}"
function_block    = "{" (return | stmt)* "}"
return            = "return" expr? ";"
import            = "import" UNIX_PATH_STRING "as" IDENTIFIER ";"
declaration       = "var" IDENTIFIER (":" type) "=" expr ";"
reassignment      = IDENTIFIER "=" expr ";"
expr              = while | when | clause
when              = "when" "{" ( clause "->" expr "," ... )* "}"
while             = "while" clause block
clause            = binary | unary | call | subscription | if
if                = "if" clause block ("else" if | block)?
binary            = clause ( "and" | "or" )          clause
                  | clause ("==" | "!=")             clause
                  | clause ("<" | "<=" | ">" | ">=") clause
                  | clause ("+" | "-")               clause
                  | clause ("*" | "/" | "%")         clause
unary             = ("not" | "-")* unary | primary
call              = clause "(" (clause, "," ...)* ")"
subscription      = clause "[" clause "]"
primary           = IDENTIFIER | group | raw_value
raw_value         = scalar | array_literal | map_literal | struct_decl | fn_decl | struct_literal
scalar            = STRING | NUMBER | "true" | "false" | "nil" 
fn_decl           = "fn" "(" (fn_param "," ...)* ")" ("->" type)? (function_block | expr)
fn_param          = IDENTIFIER (":" type)?
struct_literal    = TYPE_IDENTIFIER "{" (IDENTIFIER "=" clause "," ... )* "}"
array_literal     = "[" (clause, "," ... )* "]" | "[" ":" clause ":" clause "]" | "[" "for" IDENTIFIER "in" clause ("if" clause)? ":" clause "]"
map_literal       = "%{" (primary "=>" clause , ...)* "}"
group             = "(" clause ")"
type              = "any" | "bool" | "number" | "str" | TYPE_IDENTIFIER
```
