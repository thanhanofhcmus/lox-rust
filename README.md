# lox-rust

A implementation of the lox programming language in `rust` with author flavor on it.

Original tutorial in Java and C [link](https://craftinginterpreters.com/)

## Features

```bash
# `hash` for comment

# create a new variable with `var`
var x = 1

# print it with `print`
print(x)

# variable is dynamic, you can set it to another type
x = "a string"
print(x)

# these are types supported in this lox

# boolean
x = true
x = false

# number, with or without decimal
x = 42
x = 1.5432

# and array
x = [true, 1, "yes, mixed type in array value"]

# finally, null type
x = nil

# Here are some simple arithmetic operations
# adding, subtracting, multiply, division and modulo
x = 1
var y = x + 0.6 # a new variable, yay
var z = y / 2
var w = z * 10
var alpha = w % 2

# you can also do boolean arithmentics
var b = true and false
b = b or (2 == 10 / 5)

# if statement to do branching
if n % 2 == 0 {
  print("n is even")
} else {
  print("n is odd")
}

# while loop is also supported
var t = 100
while t > 100 {
  print(t)
  t = t - 1
}

# for loop is not supported, if you want to do for loop
# desugar it into a while loop

# but if you have multiple switch condition, you can use `when` - `case` expression

var next_n_loop_in_four = when {
  case n == 0 -> 1,
  case n == 1 -> 2,
  case n == 2 -> 3,
  case n == 4 -> 0,
}

# or you can put any condition expression in it, it will check form the top condition down
var t = when {
  case x == y and y == z -> "all equal",
  case x == y -> "only x = y",
  case y == z -> "only z = y",
  case x == z -> "only x = z",
  true -> "nothing equal anything",
}

# ternary expression is created with `cond` - `then` - `else`
var ter = cond n == 2 then "n is 2" else "n is not 2"

# functions can be created the and assign the same way variable are created

var add = fn(a, b) { return a + b }
var print_times_two = fn(n) = { print(n * 2) }

```
