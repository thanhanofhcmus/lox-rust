use crate::span::Span;
use crate::token::Token;

use super::error::ParseError;

static KEYWORDS: phf::Map<&'static [u8], Token> = phf::phf_map!(
    b"and" => Token::And,
    b"or" => Token::Or,
    b"not" => Token::Not,

    b"true" => Token::True,
    b"false" => Token::False,
    b"nil" => Token::Nil,

    b"var" => Token::Var,

    b"if" => Token::If,
    b"else" => Token::Else,
    b"while" => Token::While,
    b"for" => Token::For,
    b"when" => Token::When,
    b"in" => Token::In,

    b"fn" => Token::Fn,
    b"return" => Token::Return,

    b"import" => Token::Import,
    b"as" => Token::As,
);

#[derive(Debug, Clone, Copy)]
pub struct LexItem {
    pub span: Span,
    pub token: Token,
}

impl LexItem {
    pub fn new(token: Token, span: Span) -> Self {
        LexItem { token, span }
    }
}

fn is_keyword_or_identifier_char(c: u8) -> bool {
    let c = c as char;
    c == '_' || c.is_ascii_alphanumeric()
}

fn is_next_char(input: &[u8], curr_offset: usize, expect: u8) -> bool {
    input
        .get(curr_offset + 1)
        .map(|next| *next == expect)
        .unwrap_or(false)
}

pub fn lex(input: &str) -> Result<Vec<LexItem>, ParseError> {
    let mut curr_offset = 0;
    let mut result = vec![];

    let utf8_bytes = input.as_bytes();

    while let Some(&c) = utf8_bytes.get(curr_offset) {
        let const_offset = curr_offset; // we don't want to borrow mutate of curr_offset
        let tok_one = |t| LexItem::new(t, Span::one(const_offset));

        let mut push_with_equal = |token_has_equal, token_no_equal| {
            if is_next_char(utf8_bytes, curr_offset, b'=') {
                result.push(LexItem::new(token_has_equal, Span::two(curr_offset)));
                curr_offset += 1;
            } else {
                result.push(LexItem::new(token_no_equal, Span::one(curr_offset)));
            }
        };

        match c {
            b' ' | b'\t' | b'\r' | b'\n' => { /* skip */ }
            b'(' => result.push(tok_one(Token::LRoundParen)),
            b')' => result.push(tok_one(Token::RRoundParen)),
            b'[' => result.push(tok_one(Token::LSquareParen)),
            b']' => result.push(tok_one(Token::RSquareParen)),
            b'{' => result.push(tok_one(Token::LPointParen)),
            b'}' => result.push(tok_one(Token::RPointParen)),
            b'.' => result.push(tok_one(Token::Dot)),
            b',' => result.push(tok_one(Token::Comma)),
            b':' => result.push(tok_one(Token::Colon)),
            b';' => result.push(tok_one(Token::Semicolon)),
            b'+' => result.push(tok_one(Token::Plus)),
            b'*' => result.push(tok_one(Token::Star)),
            b'/' => result.push(tok_one(Token::Slash)),

            b'%' => {
                if is_next_char(utf8_bytes, curr_offset, b'{') {
                    result.push(LexItem::new(
                        Token::PercentLPointParent,
                        Span::two(curr_offset),
                    ));
                    curr_offset += 1;
                } else {
                    result.push(tok_one(Token::Percentage));
                }
            }

            b'-' => {
                if is_next_char(utf8_bytes, curr_offset, b'>') {
                    result.push(LexItem::new(Token::RTArrow, Span::two(curr_offset)));
                    curr_offset += 1;
                } else {
                    result.push(tok_one(Token::Minus));
                }
            }

            b'=' => {
                if is_next_char(utf8_bytes, curr_offset, b'=') {
                    result.push(LexItem::new(Token::EqualEqual, Span::two(curr_offset)));
                    curr_offset += 1;
                } else if is_next_char(utf8_bytes, curr_offset, b'>') {
                    result.push(LexItem::new(Token::RFArrtow, Span::two(curr_offset)));
                    curr_offset += 1;
                } else {
                    result.push(tok_one(Token::Equal));
                }
            }

            b'!' => {
                if is_next_char(utf8_bytes, curr_offset, b'=') {
                    result.push(LexItem::new(Token::BangEqual, Span::two(curr_offset)));
                    curr_offset += 1;
                } else {
                    return Err(ParseError::UnexpectedCharacter(Span::one(curr_offset)));
                }
            }

            b'<' => push_with_equal(Token::LessEqual, Token::Less),
            b'>' => push_with_equal(Token::GreaterEqual, Token::Greater),
            b'"' => result.push(lex_string(utf8_bytes, &mut curr_offset)?),

            b'#' => result.push(lex_comment(utf8_bytes, &mut curr_offset)),

            // need to be before the is_keyword_or_identifier_char check
            b'r' if is_next_char(utf8_bytes, curr_offset, b'"') => {
                result.push(lex_raw_string(utf8_bytes, &mut curr_offset)?)
            }

            v if v.is_ascii_digit() => result.push(lex_number(utf8_bytes, &mut curr_offset)?),
            v if is_keyword_or_identifier_char(v) => {
                result.push(lex_keyword_or_identifier(utf8_bytes, &mut curr_offset))
            }
            _ => {
                return Err(ParseError::UnexpectedCharacter(Span::one(curr_offset)));
            }
        }

        curr_offset += 1;
    }

    Ok(result)
}

fn lex_number(input: &[u8], offset: &mut usize) -> Result<LexItem, ParseError> {
    let start_offset = *offset;
    let mut has_parsed_dot = false;

    while let Some(&c) = input.get(*offset + 1) {
        if !(c.is_ascii_digit() || c == b'.') {
            break;
        }
        if c == b'.' {
            if has_parsed_dot {
                return Err(ParseError::UnexpectedCharacter(Span::one(*offset)));
            }
            has_parsed_dot = true
        }
        *offset += 1;
    }

    // check if the last thing we parsed is a dot
    let end_offset = *offset;
    if let Some(&c) = input.get(end_offset)
        && c == b'.'
    {
        return Err(ParseError::UnexpectedCharacter(Span::one(end_offset)));
    }

    Ok(LexItem::new(
        Token::Number,
        Span::new(start_offset, end_offset),
    ))
}

fn lex_string(input: &[u8], offset: &mut usize) -> Result<LexItem, ParseError> {
    let start_offset = *offset;

    *offset += 1; // consume '"'
    while let Some(&c) = input.get(*offset) {
        match c {
            // closing '"'
            b'"' => {
                let end_offset = *offset;

                return Ok(LexItem::new(
                    Token::String,
                    Span::new(start_offset, end_offset),
                ));
            }

            // escape char
            b'\\' => {
                // Skip the current char '\'
                *offset += 1;

                if *offset > input.len() {
                    return Err(ParseError::UnclosedString(start_offset));
                }

                // we skip a single byte since we only handle the case of \"
                // TODO: Handle Unicode escape
                *offset += 1;
            }

            // normal char
            _ => {
                *offset += 1;
            }
        };
    }

    // We hit the end with out hitting the closing '"'
    Err(ParseError::UnclosedString(start_offset))
}

fn lex_raw_string(input: &[u8], offset: &mut usize) -> Result<LexItem, ParseError> {
    let start_offset = *offset;

    *offset += 2; // consume 'r' and '"'
    while let Some(&c) = input.get(*offset) {
        match c {
            // closing '"'
            b'"' => {
                let end_offset = *offset;

                return Ok(LexItem::new(
                    Token::RawString,
                    Span::new(start_offset, end_offset),
                ));
            }

            // escape char
            b'\\' if is_next_char(input, *offset, b'"') => {
                // Skip the current char '\' and '"'
                *offset += 2;
            }

            // normal char
            _ => {
                *offset += 1;
            }
        };
    }

    // We hit the end with out hitting the closing '"'
    Err(ParseError::UnclosedString(start_offset))
}

fn lex_keyword_or_identifier(input: &[u8], offset: &mut usize) -> LexItem {
    let start_offset = *offset;

    while let Some(&c) = input.get(*offset + 1) {
        if !is_keyword_or_identifier_char(c) {
            break;
        }
        *offset += 1;
    }

    let id = &input[start_offset..(*offset + 1)];
    let span = Span::new(start_offset, *offset);

    match KEYWORDS.get(id) {
        Some(&token) => LexItem::new(token, span),
        None => LexItem::new(Token::Identifier, span),
    }
}

fn lex_comment(input: &[u8], offset: &mut usize) -> LexItem {
    let start_offset = *offset;

    while let Some(&c) = input.get(*offset) {
        // skip until we get a new line
        if c == b'\n' {
            break;
        }
        *offset += 1;
    }

    LexItem::new(Token::Comment, Span::new(start_offset, *offset))
}
