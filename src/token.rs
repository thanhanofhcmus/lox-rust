use derive_more::Display;

#[derive(Clone, Copy, Debug, Display, PartialEq)]
pub enum Token {
    LeftParen,
    RightParen,

    Bang,

    Plus,
    Minus,
    Star,
    Slash,

    And,
    Or,

    True,
    False,

    Identifier,
    Number,
}
