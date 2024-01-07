use crate::parse_error::ParseError;

use crate::span::Span;
use crate::token::Token;

static KEYWORDS: phf::Map<&'static str, Token> = phf::phf_map!(
    "and" => Token::And,
    "or" => Token::Or,

    "true" => Token::True,
    "false" => Token::False,
);

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
        } else if c == '=' {
            let is_equal_equal = input
                .chars()
                .nth(curr_offset + 1)
                .map(|next| next == '=')
                .unwrap_or(false);
            if is_equal_equal {
                result.push(LexItem::new(Token::EqualEqual, Span::two(curr_offset)));
                curr_offset += 1;
            } else {
                result.push(LexItem::new(Token::Equal, Span::one(curr_offset)));
            }
        } else if c == '!' {
            let is_bang_equal = input
                .chars()
                .nth(curr_offset + 1)
                .map(|next| next == '=')
                .unwrap_or(false);
            if is_bang_equal {
                result.push(LexItem::new(Token::BangEqual, Span::two(curr_offset)));
                curr_offset += 1;
            } else {
                result.push(LexItem::new(Token::Bang, Span::one(curr_offset)));
            }
        } else if c.is_ascii_digit() {
            result.push(lex_number(input, &mut curr_offset));
        } else if is_identifier_char(c) {
            result.push(lex_keyword_or_identifier(input, &mut curr_offset))
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

    let id = input
        .chars()
        .skip(start_offset)
        .take(*offset - start_offset + 1)
        .collect::<String>();

    let span = Span::new(start_offset, *offset);

    match KEYWORDS.get(id.as_str()) {
        Some(&token) => LexItem::new(token, span),
        None => LexItem::new(Token::Identifier, span),
    }
}
