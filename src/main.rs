use derive_more::Display;
use thiserror::Error;

#[derive(Debug, Clone, Copy, Display)]
#[display(fmt = "[{}:{}]", start, end)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    pub fn one(start: usize) -> Self {
        Span { start, end: start }
    }

    pub fn two(start: usize) -> Self {
        Span {
            start,
            end: start + 1,
        }
    }

    pub fn extract_from_source<'a>(&self, input: &'a [u8]) -> &'a [u8] {
        &input[self.start..self.end]
    }
}

#[derive(Debug)]
pub enum Token {
    LeftParen,
    RightParen,

    Number,

    Plus,
    Minus,
    Star,
    Slash,
}

#[derive(Debug)]
pub struct LexItem {
    pub span: Span,
    pub token: Token,
}

impl LexItem {
    pub fn new(token: Token, span: Span) -> Self {
        LexItem { token, span }
    }
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token in position {0}")]
    UnexpectedToken(Span),
}

fn is_ascii_number(c: &u8) -> bool {
    (48..=57).contains(c)
}

fn lex(input: &[u8]) -> Result<Vec<LexItem>, ParseError> {
    let mut curr_offset = 0;
    let mut result = vec![];

    while let Some(&c) = input.get(curr_offset) {
        if c == b'(' {
            result.push(LexItem::new(Token::LeftParen, Span::two(curr_offset)));
        } else if c == b')' {
            result.push(LexItem::new(Token::RightParen, Span::two(curr_offset)));
        } else if c == b'+' {
            result.push(LexItem::new(Token::Plus, Span::two(curr_offset)));
        } else if c == b'-' {
            result.push(LexItem::new(Token::Minus, Span::two(curr_offset)));
        } else if c == b'*' {
            result.push(LexItem::new(Token::Star, Span::two(curr_offset)));
        } else if c == b'/' {
            result.push(LexItem::new(Token::Slash, Span::two(curr_offset)));
        } else if c == b' ' || c == b'\t' {
            // skip
        } else if is_ascii_number(&c) {
            result.push(lex_number(input, &mut curr_offset));
        } else {
            return Err(ParseError::UnexpectedToken(Span::one(curr_offset)));
        }
        curr_offset += 1;
    }

    Ok(result)
}

fn lex_number(input: &[u8], offset: &mut usize) -> LexItem {
    let start_offset = *offset;

    while let Some(c) = input.get(*offset) {
        if !is_ascii_number(c) {
            break;
        }
        *offset += 1;
    }

    LexItem::new(Token::Number, Span::new(start_offset, *offset))
}

fn main() {
    let input = "(123+-*/)";

    let tokens = lex(input.as_bytes()).unwrap();

    for token in tokens {
        println!(
            "{} - {:?}: {:?}",
            token.span,
            token.token,
            std::str::from_utf8(token.span.extract_from_source(input.as_bytes())).unwrap(),
        );
    }
}
