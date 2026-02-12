

["var" "import" "as" "fn"] @keyword
["true" "false" "nil"] @constant.builtin
["and" "or" "not"] @operator

(fn_call (identifier) @function.builtin (#match? @function.builtin "^(assert|print|_dbg_.*)$"))
(fn_call (identifier) @function (#not-match? @function "^(assert|print|_dbg_.*)$"))

(parameter_list (identifier) @variable.parameter)

(number) @number
(string) @string
(unix_path_string) @string.special
(comment) @comment
