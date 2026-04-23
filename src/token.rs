use derive_more::Display;

#[derive(Clone, Copy, Debug, Display, PartialEq)]
pub enum Token {
    PercentLPointParent, // %{
    PercentLRoundParen,  // %(

    LRoundParen,  // (
    RRoundParen,  // )
    LSquareParen, // [
    RSquareParen, // ]
    LPointParen,  // {
    RPointParen,  // }

    Equal, // =

    EqualEqual, // ==
    BangEqual,  // !=

    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=

    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percentage, // %

    And, // and
    Or,  // or
    Not, // not

    RThinArrow, // ->
    RFatArrow,  // =>

    True,  // true
    False, // false
    Nil,   // nil

    Var,    // var
    If,     // if
    Else,   // else
    While,  // while
    For,    // for
    Fn,     // fn
    Return, // return
    When,   // when
    Import, // import
    As,     // as
    In,     // in
    Struct, // struct

    // Types
    TypeAny,
    TypeBool,
    TypeNumber,
    TypeStr,

    Identifier,
    Number,
    WholeNumber,
    String,
    RawString,

    Dot,       // .
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;

    Comment, // a whole comment until the newline
}
