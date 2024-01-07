use derive_more::Display;

#[derive(Clone, Copy, Debug, Display, PartialEq)]
pub enum Token {
    LeftParen,
    RightParen,

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

    Identifier,
    Number,
    String,
}
