use derive_more::Display;

#[derive(Clone, Copy, Debug, Display, PartialEq)]
pub enum Token {
    LeftParen,
    RightParen,

    Number,

    Plus,
    Minus,
    Star,
    Slash,

    And,
    Or,

    True,
    False,

    Identifier,
}
