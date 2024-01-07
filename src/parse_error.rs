use crate::span::Span;
use crate::token::Token;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected character in position {0}")]
    UnexpectedCharacter(Span),

    #[error("Unexpected token `{0}` at position {1}")]
    UnexpectedToken(Token, Span),

    #[error("String starts at {0} is not closed")]
    UnclosedString(usize),

    #[error("Unable to parse to number at position {0}")]
    ParseToNumber(Span),

    #[error("Unable to parse the next value")]
    Eof,

    #[error("Parse have leftover tokens start with {0} at {1}")]
    Unfinished(Token, Span),
}
