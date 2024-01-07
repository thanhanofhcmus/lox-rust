use crate::span::Span;
use crate::token::Token;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected character in position {0}")]
    UnexpectedCharacter(Span),

    #[error("Unexpected token `{0}` at position {1} {}", diag_expect_token(.2))]
    UnexpectedToken(Token, Span, Option<Token>),

    #[error("String starts at {0} is not closed")]
    UnclosedString(usize),

    #[error("Unable to parse to number at position {0}")]
    ParseToNumber(Span),

    #[error("Unexpected `return` keyword outside of function")]
    UnexpectedReturn(Span),

    #[error("Unable to parse the next value because of EOF")]
    Eof,

    #[error("Parse have leftover tokens start with {0} at {1}")]
    Unfinished(Token, Span),
}

fn diag_expect_token(o: &Option<Token>) -> String {
    match o {
        None => "".to_string(),
        Some(t) => format!(", expected token `{:?}`", t),
    }
}
