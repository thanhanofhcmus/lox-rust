

["var" "import" "as" "fn" "when"] @keyword
["true" "false" "nil"] @constant.builtin
["and" "or" "not"] @operator

(fn_call (identifier) @function.builtin (#match? @function.builtin "^(assert|print|to_json|from_json|_dbg_.*)$"))
(fn_call (identifier) @function (#not-match? @function "^(assert|print|to_json|from_json|_dbg_.*)$"))

(map_literal "%{" @punctuation.special "}" @punctuation.special)
(array_literal "[" @punctuation.bracket "]" @punctuation.bracket)

(parameter_list (identifier) @variable.parameter)

(number) @number
(string) @string
(unix_path_string) @string.special
(comment) @comment
