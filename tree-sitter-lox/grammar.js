/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PRECEDENCES = {
  Logical: 1,
  Equality: 2,
  Comparison: 3,
  Term: 4,
  Factor: 5,
  Unary: 6,
  Chain: 7,
  Primay: 8,
};

module.exports = grammar({
  name: "lox",

  extras: ($) => [/\s/, $.comment],

  rules: {
    source_file: ($) => seq(repeat($.import), repeat($.stmt)),

    import: ($) => seq("import", $.unix_path_string, "as", $.identifier),

    stmt: ($) => choice($.declaration, seq($.expr, optional(";"))),

    block: ($) => seq("{", repeat($.stmt), "}"),

    declaration: ($) => seq("var", $.identifier, "=", $.expr, ";"),

    expr: ($) => choice($.clause),

    clause: ($) => $._clause,

    _clause: ($) =>
      choice($.binary, $.unary, $.fn_call, $.subscription, $.primary),

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

    fn_call: ($) =>
      prec.left(
        PRECEDENCES.Chain,
        seq($.identifier, "(", sep_by_optional(",", $._clause), ")"),
      ),

    subscription: ($) => prec.left(PRECEDENCES.Chain, seq("[", $._clause, "]")),

    primary: ($) =>
      prec.left(PRECEDENCES.Primay, choice($.identifier, $._raw_value)),

    _raw_value: ($) =>
      choice($.scalar, $.array_literal, $.map_literal, $.fn_decl),

    scalar: ($) => choice("true", "false", "nil", $.number, $.string),

    string: ($) => /"[^"]*"/,

    number: ($) => /\d+(\.\d+)?/,

    array_literal: ($) =>
      choice(
        seq("[", sep_by_optional(",", $._clause), "]"),
        seq("[", ":", $._clause, ":", $._clause, "]"),
      ),

    map_literal: ($) =>
      seq("%{", sep_by_optional(",", seq($.scalar, "=>", $.expr)), "}"),

    fn_decl: ($) =>
      seq(
        "fn",
        "(",
        field("parameters", optional($.parameter_list)),
        ")",
        choice($.expr, $.block),
      ),

    parameter_list: ($) => sep_by(",", $.identifier),

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
