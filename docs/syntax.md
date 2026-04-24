Lox syntax

Syntax point

- `stmt` is the top most level
- `expr` is anything that produces value
- `clause` is a simple expression
- `primary` is a primitive value

```
global_block      = import* stmt*
stmt              = declaration | struct_decl | reassignment | expr
struct_decl       = "struct" "{" (struct_field_decl "," ...)* "}"
struct_field_decl = IDENTIFIER (":" type_iden )?
block             = "{" stmt* "}"
function_block    = "{" (return | stmt)* "}"
return            = "return" expr? ";"
import            = "import" UNIX_PATH_STRING "as" IDENTIFIER ";"
declaration       = "var" binding (":" type_iden)? "=" expr ";"
binding           = IDENTIFIER | "%(" IDENTIFIER "," ... ")"
reassignment      = IDENTIFIER "=" expr ";"
expr              = if | while | when | for | clause
when              = "when" "{" ( clause "->" expr "," ... )* "}"
while             = "while" clause block
for               = "for" binding "in" clause block
clause            = binary | unary | fn_call | subscription | member_access
if                = "if" clause block ("else" if | block)?
binary            = clause ( "and" | "or" )          clause
                  | clause ("==" | "!=")             clause
                  | clause ("<" | "<=" | ">" | ">=") clause
                  | clause ("+" | "-")               clause
                  | clause ("*" | "/" | "%")         clause
unary             = ("not" | "-")* unary | primary
fn_call           = clause "(" (clause, "," ...)* ")"
subscription      = clause "[" clause "]"
member_access     = clause "." (IDENTIFIER | WHOLE_NUMBER)
primary           = IDENTIFIER | group | raw_value
raw_value         = scalar | array_literal | map_literal | tuple_literal | struct_decl | fn_decl | struct_literal
scalar            = STRING | NUMBER | "true" | "false" | "nil"
fn_decl           = "fn" "(" (fn_param "," ...)* ")" ("->" type_iden)? (function_block | expr)
fn_param          = IDENTIFIER (":" type_iden)?
struct_literal    = TYPE_IDENTIFIER "{" (IDENTIFIER "=" clause "," ... )* "}"
array_literal     = "[" (clause, "," ... )* "]" | "[" ":" clause ":" clause "]" | "[" "for" IDENTIFIER "in" clause ("if" clause)? ":" clause "]"
map_literal       = "%{" (primary "=>" clause , ...)* "}"
tuple_literal     = "%(" (clause "," ...)* ")"
group             = "(" clause ")"
type_iden         = "any" | "bool" | "number" | "str"
                  | "[" type_iden "]"
                  | "%{" type_iden "=>" type_iden "}"
                  | "%(" type_iden "," ... ")"
                  | TYPE_IDENTIFIER
```
