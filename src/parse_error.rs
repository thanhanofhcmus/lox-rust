use crate::span::Span;
use crate::token::Token;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected character in position {0}")]
    UnexpectedCharacter(Span),

    #[error("Unexpected token {0} at position {1}")]
    UnexpectedToken(Token, Span),

    #[error("unable to convert to utf8")]
    ConvertFromUtf8(Span),

    #[error("unable to parse to number")]
    ParseToNumber(Span),

    #[error("unable to parse the next value")]
    Eof,
}
