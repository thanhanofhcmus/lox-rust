use crate::parse_error::ParseError;

use crate::span::Span;
use crate::token::Token;

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

fn is_ascii_number(c: &u8) -> bool {
    (48..=57).contains(c)
}

pub fn lex(input: &[u8]) -> Result<Vec<LexItem>, ParseError> {
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
            return Err(ParseError::UnexpectedCharacter(Span::one(curr_offset)));
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
