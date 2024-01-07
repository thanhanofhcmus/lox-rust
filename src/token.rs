use derive_more::Display;

#[derive(Clone, Copy, Debug, Display, PartialEq)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,

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

    Identifier,
    Number,
    String,

    Comma,
    Semicolon,
}
