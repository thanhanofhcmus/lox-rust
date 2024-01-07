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

fn is_identifier_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric()
}

pub fn lex(input: &str) -> Result<Vec<LexItem>, ParseError> {
    let mut curr_offset = 0;
    let mut result = vec![];

    while let Some(c) = input.chars().nth(curr_offset) {
        if c == '(' {
            result.push(LexItem::new(Token::LeftParen, Span::one(curr_offset)));
        } else if c == ')' {
            result.push(LexItem::new(Token::RightParen, Span::one(curr_offset)));
        } else if c == '+' {
            result.push(LexItem::new(Token::Plus, Span::one(curr_offset)));
        } else if c == '-' {
            result.push(LexItem::new(Token::Minus, Span::one(curr_offset)));
        } else if c == '*' {
            result.push(LexItem::new(Token::Star, Span::one(curr_offset)));
        } else if c == '/' {
            result.push(LexItem::new(Token::Slash, Span::one(curr_offset)));
        } else if c == ' ' || c == '\t' {
            // skip
        } else if c.is_ascii_digit() {
            result.push(lex_number(input, &mut curr_offset));
        } else {
            return Err(ParseError::UnexpectedCharacter(Span::one(curr_offset)));
        }
        curr_offset += 1;
    }

    Ok(result)
}

fn lex_number(input: &str, offset: &mut usize) -> LexItem {
    let start_offset = *offset;

    while let Some(c) = input.chars().nth(*offset + 1) {
        if !c.is_ascii_digit() {
            break;
        }
        *offset += 1;
    }

    LexItem::new(Token::Number, Span::new(start_offset, *offset))
}

fn lex_keyword_or_identifier(input: &str, offset: &mut usize) -> LexItem {
    let start_offset = *offset;

    while let Some(c) = input.chars().nth(*offset + 1) {
        if !is_identifier_char(c) {
            break;
        }
        *offset += 1;
    }

    todo!()
}
