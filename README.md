# lox-rust

An implementation of the lox programming language in `rust` with author flavor on it.

Original tutorial in Java and C [link](https://craftinginterpreters.com/)

## Running

Build once, then pick a mode:

```bash
cargo build --release

# 1. REPL — read / eval / print loop
./target/release/lox-rust -i

# 2. Prompt — run a single statement and exit
./target/release/lox-rust -p 'print("hello");'

# 3. File — run a source file
./target/release/lox-rust -f path/to/program.lox
```

The `-i` REPL also accepts an optional seed statement as the next argument
(e.g. `-i 'var x = 1;'`) which is executed before the prompt starts.

## Language tour

### Comments

```
# `#` for single-line comments
```

### Variables

Declarations use `var` and end with `;`. Variables are mutable by default.

```
var x = 1;
print(x);

# reassignment doesn't repeat `var`
x = "a string";
print(x);
```

### Types

lox-rust has a gradual type system — you can annotate, leave bare, or use `any`.

```
# built-in type names: any, bool, number, str
var n: number = 42;
var s: str    = "hello";
var b: bool   = true;
var a: any    = "anything goes";   # `any` accepts any value
var inferred  = 3.14;              # no annotation → inferred as `number`
```

Scalar types:

```
var z = nil;           # the nil value
var t = true;          # bool
var f = false;
var i = 42;            # integer number
var d = 1.5432;        # floating number
```

Integer-vs-float arithmetic is hybrid: `2 / 2` stays an integer (`1`), but
`2 / 3` promotes to float. Mixing always promotes to float.

### Strings

```
var greet = "hello\nworld";         # standard escapes: \n \t \r \" \\
var raw   = r"C:\path\no\escape";   # raw string, no escape processing
var joined = "foo" + "bar";         # `+` concatenates strings
```

### Arithmetic and logic

```
var y = 1 + 0.6;
var z = y / 2;
var w = z * 10;
var m = w % 2;           # modulo

var b = true and false;
b = b or (2 == 10 / 5);
var n = not b;           # unary `not`

# comparisons yield bool
var lt = 1 < 2;
var eq = "a" == "a";
```

### Control flow — expression-oriented

Blocks are expressions. The last expression without `;` is the block's value.

```
var x = {
  var a = 1;
  var b = 2;
  a + b           # no `;` → this is what the block evaluates to
};                # statement still needs its trailing `;`
```

`if` / `else if` / `else` is an expression, too:

```
var label = if n % 2 == 0 {
  "even"
} else if n == 0 {
  "zero"
} else {
  "odd"
};
```

`while` loops iterate until the condition is false:

```
var t = 100;
while t > 0 {
  print(t);
  t = t - 1;
}
```

`for ... in` walks arrays (and anything array-shaped at runtime):

```
for x in [1, 2, 3] {
  print(x);
}
```

`when` is a condition table — the first truthy arm wins. Use `true` as the
last arm for a default case.

```
var next = when {
  n == 0 -> 1,
  n == 1 -> 2,
  n == 2 -> 3,
  true   -> 0,
};
```

### Arrays

```
var xs = [true, 1, "mixed types are fine"];
print(xs[0]);                 # index with `[ ]`
xs[1] = 99;                   # element reassignment works

# repeat syntax: value, then count
var zeros = [:0:5];           # [0, 0, 0, 0, 0]

# list comprehension, with optional `if` filter
var evens = [for x in xs if x % 2 == 0: x * 10];
```

Array builtins: `array_length`, `array_push`, `array_pop`, `array_insert`.

### Maps

Map literals use `%{ key => value, ... }`. Keys must be scalars or strings.

```
var scores = %{
  "alice" => 10,
  "bob"   => 7,
};
print(scores["alice"]);
scores["carol"] = 12;         # subscript assignment works on maps too
```

Map builtins: `map_length`, `map_keys`, `map_values`, `map_insert`, `map_remove`.

### Functions

Functions are first-class values. Two body forms:

```
# block body — statements inside, explicit `return`
var add = fn(a, b) { return a + b; };

# expression body — single expression, implicit return
var inc = fn(x) x + 1;

# optional parameter and return-type annotations
var sub: fn = fn(a: number, b: number) -> number { return a - b; };

print(add(2, 3));
print(inc(10));
```

### Structs

Declare a struct, then build instances with a literal. Field order in the
literal doesn't matter, but every field must be present.

```
struct Point { x: number, y: number }

var p = Point { x = 1, y = 2 };
print(p.x);            # field read with `.` works
print(p.y);

# field write works too, including chained and mixed chains
p.x = 99;
struct Box { center: any }
var b = Box { center = Point { x = 10, y = 20 } };
b.center.x = 42;       # chained write: struct.field.field

struct Bag { items: any }
var bag = Bag { items = [10, 20, 30] };
print(bag.items[1]);   # mixed chain read: struct field -> array index
bag.items[0] = 99;     # mixed chain write
```

> Known limitation:
> - User-defined struct names are **not yet valid in type annotations**. Built-in
>   types (`any`, `bool`, `number`, `str`) are; for struct-typed fields, use
>   `any` as a placeholder (e.g. `struct Box { center: any }`).

Struct equality (`==`) is nominal and recursive: two values of the same
struct type are equal iff every field is equal (recursing through nested
structs, arrays, and maps). Values of different struct types are never
equal, even with identical field names and values.

### Imports

Modules are loaded from the filesystem. Imports must be at the top of the file.

```
import "./math.lox" as math;
# Planned syntax for referring to an exported name is `math::add(...)`
# using `::` — NOT `math.add(...)`. Dot (`.`) is reserved for struct
# field access and (future) method calls on values, while `::` is
# the module-path operator. Neither form is implemented yet.
```

### Built-in debug helpers

Handy when poking around the REPL:

```
_dbg_state();         # dump scopes, modules, and heap
_dbg_heap_stats();    # live/dead GC object counts
_dbg_gc_mark_sweep(); # force a GC cycle
```

### JSON

```
var s = to_json(%{"a" => 1, "b" => [2, 3]});
var v = from_json(s);
```
