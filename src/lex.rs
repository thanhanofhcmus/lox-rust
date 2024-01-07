use crate::parse_error::ParseError;

use crate::span::Span;
use crate::token::Token;

static KEYWORDS: phf::Map<&'static str, Token> = phf::phf_map!(
    "and" => Token::And,
    "or" => Token::Or,

    "true" => Token::True,
    "false" => Token::False,
    "nil" => Token::Nil,

    "var" => Token::Var,
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

fn is_keyword_or_identifier_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric()
}

fn is_next_equal(input: &str, curr_offset: usize) -> bool {
    input
        .chars()
        .nth(curr_offset + 1)
        .map(|next| next == '=')
        .unwrap_or(false)
}

pub fn lex(input: &str) -> Result<Vec<LexItem>, ParseError> {
    let mut curr_offset = 0;
    let mut result = vec![];

    while let Some(c) = input.chars().nth(curr_offset) {
        let mut push_with_equal = |token_has_equal, token_no_equal| {
            if is_next_equal(input, curr_offset) {
                result.push(LexItem::new(token_has_equal, Span::two(curr_offset)));
                curr_offset += 1;
            } else {
                result.push(LexItem::new(token_no_equal, Span::one(curr_offset)));
            }
        };

        if c == '(' {
            result.push(LexItem::new(Token::LeftParen, Span::one(curr_offset)));
        } else if c == ')' {
            result.push(LexItem::new(Token::RightParen, Span::one(curr_offset)));
        } else if c == ';' {
            result.push(LexItem::new(Token::Semicolon, Span::one(curr_offset)));
        } else if c == '+' {
            result.push(LexItem::new(Token::Plus, Span::one(curr_offset)));
        } else if c == '-' {
            result.push(LexItem::new(Token::Minus, Span::one(curr_offset)));
        } else if c == '*' {
            result.push(LexItem::new(Token::Star, Span::one(curr_offset)));
        } else if c == '"' {
            result.push(lex_string(input, &mut curr_offset)?);
        } else if c == '/' {
            result.push(LexItem::new(Token::Slash, Span::one(curr_offset)));
        } else if c == ' ' || c == '\t' {
            // skip
        } else if c == '=' {
            push_with_equal(Token::EqualEqual, Token::Equal);
        } else if c == '!' {
            push_with_equal(Token::BangEqual, Token::Bang);
        } else if c == '<' {
            push_with_equal(Token::LessEqual, Token::Less);
        } else if c == '>' {
            push_with_equal(Token::GreaterEqual, Token::Greater);
        } else if c.is_ascii_digit() {
            result.push(lex_number(input, &mut curr_offset));
        } else if is_keyword_or_identifier_char(c) {
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

fn lex_string(input: &str, offset: &mut usize) -> Result<LexItem, ParseError> {
    let start_offset = *offset;
    *offset += 1; // consume '"'
    while let Some(c) = input.chars().nth(*offset) {
        if c == '"' {
            break;
        }
        *offset += 1;
    }

    if input.chars().nth(*offset).is_none() {
        return Err(ParseError::UnclosedString(start_offset));
    }

    Ok(LexItem::new(
        Token::String,
        Span::new(start_offset, *offset),
    ))
}

fn lex_keyword_or_identifier(input: &str, offset: &mut usize) -> LexItem {
    let start_offset = *offset;

    while let Some(c) = input.chars().nth(*offset + 1) {
        if !is_keyword_or_identifier_char(c) {
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
