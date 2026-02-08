/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "lox",

  extras: ($) => [/\s/, $.comment],

  rules: {
    source_file: ($) => seq(repeat($.stmt)),

    stmt: ($) => choice($.declaration, $.expr),

    declaration: ($) => seq("var", $.identifier, "=", $.expr, ";"),

    expr: ($) => choice($.clause),

    clause: ($) => choice($.nil, $.bool, $.number, $.string),

    nil: ($) => "null",
    bool: ($) => choice("true", "false"),
    string: ($) => /"[^"]*"/,
    number: ($) => /\d+(\.\d+)?/,

    identifier: ($) => sep_by(".", /[a-zA-Z_][a-zA-Z0-9_]*/),

    unix_path_string: ($) => /"[^"]+"/,

    comment: ($) => token(seq("#", /.*/)),
  },
});

// @ts-ignore
function sep_by(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)));
}
