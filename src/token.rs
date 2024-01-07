use derive_more::Display;

#[derive(Clone, Copy, Debug, Display, PartialEq)]
pub enum Token {
    LRoundParen,
    RRoundParen,
    LSquareParen,
    RSquareParen,
    LPointParen,
    RPointParen,

    Equal,
    Bang,

    EqualEqual,
    BangEqual,

    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    Plus,
    Minus,
    Star,
    Slash,

    And,
    Or,

    True,
    False,
    Nil,

    Var,
    If,
    Else,
    While,
    Fn,
    When,
    Then,

    Identifier,
    Number,
    String,

    Comma,
    Semicolon,

    // TODO: remove when implement function
    Print,
}
