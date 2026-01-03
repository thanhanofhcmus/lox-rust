Lox syntax

```
stmt         = reassignment | declaration | print | expr | if | while | return | module
module       = "module" block
print        = "print" (clause "," ...)*
if           = "if" clause block ("else" block)?
declaration  = "var" IDENTIFIER "=" expr
while        = "while" clause block
return       = "return" (expr)?
block        = "{" (stmt ";")* "}"
reassignment = lvalue "=" expr
lvalue       = IDENTIFIER | index
expr         = clause | ternary | when
ternary      = "cond" clause "then" clause "else" clause
when         = "when" "{" ( "case" clause "->" expr "," ... )* "}"
clause       = logical
logical      = equality (( "and" | "or" ) equality)*
equality     = comparison (("==" | "!=") comparison)*
comparison   = term (("<" | "<" | "<=" | ">=") term)*
term         = factor (("+" | "-") factor)*
factor       = modulo (("*" | "/") modulo)*
modulo       = unary ("%" unary)?
unary        = ("!" | "-")* unary | call
call         = index "(" (clause "," ...)* ")"
index        = primary ("[" clause "]")?
primary      = STRING | NUMBER | identifier | atom | group | array | function
atom         = "true" | "false" | "nil"
identifier   = (IDENTIFIER "." ...)*
function     = "fn" "(" ( IDENTIFIER "," ... )* ")" (block | expr)
array        = "[" (clause, "," ... )* | (clause ":" clause) "]"
group        = "(" clause ")"
```
