use crate::span::Span;
use crate::token::Token;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected character in position {0}")]
    UnexpectedCharacter(Span),

    #[error("Unexpected token `{0}` at position {1}")]
    UnexpectedToken(Token, Span),

    #[error("unable to parse to number at position {0}")]
    ParseToNumber(Span),

    #[error("unable to parse the next value")]
    Eof,

    #[error("parse have leftover tokens start with {0} at {1}")]
    Unfinished(Token, Span),
}
