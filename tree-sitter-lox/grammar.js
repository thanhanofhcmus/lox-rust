/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PRECEDENCES = {
  Logical: 1,
  Equality: 2,
  Comparison: 3,
  Term: 4,
  Factor: 5,
  Unary: 6,
  Chaining: 7,
};

module.exports = grammar({
  name: "lox",

  extras: ($) => [/\s/, $.comment],

  rules: {
    source_file: ($) => seq(repeat($.import), repeat($.stmt)),

    import: ($) => seq("import", $.unix_path_string, "as", $.identifier),

    stmt: ($) => choice($.declaration, $.expr),

    declaration: ($) => seq("var", $.identifier, "=", $.expr, ";"),

    expr: ($) => choice($.clause),

    clause: ($) => $._clause,

    _clause: ($) => choice($.binary, $.unary, $.chaining),

    binary: ($) =>
      choice(
        prec.left(
          PRECEDENCES.Logical,
          seq(
            field("left", $._clause),
            field("op", choice("and", "or")),
            field("right", $._clause),
          ),
        ),
        prec.left(
          PRECEDENCES.Equality,
          seq(
            field("left", $._clause),
            field("op", choice("==", "!=")),
            field("right", $._clause),
          ),
        ),
        prec.left(
          PRECEDENCES.Comparison,
          seq(
            field("left", $._clause),
            field("op", choice("<", "<=", "=>", ">")),
            field("right", $._clause),
          ),
        ),
        prec.left(
          PRECEDENCES.Term,
          seq(
            field("left", $._clause),
            field("op", choice("+", "-")),
            field("right", $._clause),
          ),
        ),
        prec.left(
          PRECEDENCES.Factor,
          seq(
            field("left", $._clause),
            field("op", choice("*", "/", "%")),
            field("right", $._clause),
          ),
        ),
      ),

    unary: ($) =>
      prec.left(PRECEDENCES.Unary, seq(choice("-", "not"), $._clause)),

    chaining: ($) =>
      prec.left(
        PRECEDENCES.Chaining,
        seq($.chaining_base, optional(repeat($.chaining_follow))),
      ),

    chaining_base: ($) => choice($.identifier, $.primary),

    chaining_follow: ($) =>
      choice(
        seq(".", $.identifier),
        seq("[", $.expr, "]"), // indexing
        seq("(", sep_by_optional(",", $.expr), ")"), // function call
      ),

    primary: ($) => choice($.scalar, $.array_literal, $.map_literal),

    scalar: ($) => choice("true", "false", "nil", $.number, $.string),

    string: ($) => /"[^"]*"/,

    number: ($) => /\d+(\.\d+)?/,

    array_literal: ($) =>
      choice(
        seq("[", sep_by_optional(",", $.clause), "]"),
        seq("[", ":", $.clause, ":", $.clause, "]"),
      ),

    map_literal: ($) =>
      seq("%{", sep_by_optional(",", seq($.scalar, "=>", $.expr)), "}"),

    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    unix_path_string: ($) => /"[^"]+"/,

    comment: ($) => token(seq("#", /.*/)),
  },
});

// @ts-ignore
function sep_by(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)));
}

// @ts-ignore
function sep_by_optional(sep, rule) {
  return optional(sep_by(sep, rule));
}
