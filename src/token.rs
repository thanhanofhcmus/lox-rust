use derive_more::Display;

#[derive(Clone, Copy, Debug, Display, PartialEq)]
pub enum Token {
    #[display("%{{")]
    PercentLPointParent,
    #[display("%(")]
    PercentLRoundParen,

    #[display("(")]
    LRoundParen,
    #[display(")")]
    RRoundParen,
    #[display("[")]
    LSquareParen,
    #[display("]")]
    RSquareParen,
    #[display("{{")]
    LPointParen,
    #[display("}}")]
    RPointParen,

    #[display("=")]
    Equal,

    #[display("==")]
    EqualEqual,
    #[display("!=")]
    BangEqual,

    #[display("<")]
    Less,
    #[display("<=")]
    LessEqual,
    #[display(">")]
    Greater,
    #[display(">=")]
    GreaterEqual,

    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[display("*")]
    Star,
    #[display("/")]
    Slash,
    #[display("%")]
    Percentage,

    #[display("and")]
    And,
    #[display("or")]
    Or,
    #[display("not")]
    Not,

    #[display("->")]
    RThinArrow,
    #[display("=>")]
    RFatArrow,

    #[display("true")]
    True,
    #[display("false")]
    False,
    #[display("nil")]
    Nil,

    #[display("var")]
    Var,
    #[display("if")]
    If,
    #[display("else")]
    Else,
    #[display("while")]
    While,
    #[display("for")]
    For,
    #[display("fn")]
    Fn,
    #[display("return")]
    Return,
    #[display("when")]
    When,
    #[display("import")]
    Import,
    #[display("as")]
    As,
    #[display("in")]
    In,
    #[display("struct")]
    Struct,

    #[display("any")]
    TypeAny,
    #[display("bool")]
    TypeBool,
    #[display("number")]
    TypeNumber,
    #[display("str")]
    TypeStr,

    #[display("identifier")]
    Identifier,
    #[display("number literal")]
    Number,
    #[display("whole number literal")]
    WholeNumber,
    #[display("string literal")]
    String,
    #[display("raw string literal")]
    RawString,

    #[display(".")]
    Dot,
    #[display(",")]
    Comma,
    #[display(":")]
    Colon,
    #[display(";")]
    Semicolon,

    #[display("comment")]
    Comment,
}
