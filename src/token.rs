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
    Percentage,

    And,
    Or,

    RTArrow, // right thin arrow "->"

    True,
    False,
    Nil,

    Var,
    If,
    Else,
    While,
    Fn,
    Return,
    Cond,
    Then,
    When,
    Case,
    Module,

    Identifier,
    Number,
    String,

    Dot,
    Comma,
    Colon,
    Semicolon,

    // TODO: remove when implement functions and modules
    Print,
}
