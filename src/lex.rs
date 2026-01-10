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
    "if" => Token::If,
    "else" => Token::Else,
    "while" => Token::While,
    "fn" => Token::Fn,
    "return" => Token::Return,
    "cond" => Token::Cond,
    "then" => Token::Then,
    "when" => Token::When,
    "case" => Token::Case,
    "module" => Token::Module,
    "import" => Token::Import,
    "as" => Token::As,

    "print" => Token::Print,
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

fn is_keyword_or_identifier_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric()
}

fn is_next_char(input: &str, curr_offset: usize, expect: char) -> bool {
    input
        .chars()
        .nth(curr_offset + 1)
        .map(|next| next == expect)
        .unwrap_or(false)
}

pub fn lex(input: &str) -> Result<Vec<LexItem>, ParseError> {
    let mut curr_offset = 0;
    let mut result = vec![];

    while let Some(c) = input.chars().nth(curr_offset) {
        let const_offset = curr_offset; // we don't want to borrow mutate of curr_offset
        let tok_one = |t| LexItem::new(t, Span::one(const_offset));

        let mut push_with_equal = |token_has_equal, token_no_equal| {
            if is_next_char(input, curr_offset, '=') {
                result.push(LexItem::new(token_has_equal, Span::two(curr_offset)));
                curr_offset += 1;
            } else {
                result.push(LexItem::new(token_no_equal, Span::one(curr_offset)));
            }
        };

        match c {
            ' ' | '\t' | '\r' | '\n' => { /* skip */ }
            '(' => result.push(tok_one(Token::LRoundParen)),
            ')' => result.push(tok_one(Token::RRoundParen)),
            '[' => result.push(tok_one(Token::LSquareParen)),
            ']' => result.push(tok_one(Token::RSquareParen)),
            '{' => result.push(tok_one(Token::LPointParen)),
            '}' => result.push(tok_one(Token::RPointParen)),
            '.' => result.push(tok_one(Token::Dot)),
            ',' => result.push(tok_one(Token::Comma)),
            ':' => result.push(tok_one(Token::Colon)),
            ';' => result.push(tok_one(Token::Semicolon)),
            '+' => result.push(tok_one(Token::Plus)),
            '*' => result.push(tok_one(Token::Star)),
            '/' => result.push(tok_one(Token::Slash)),
            '%' => result.push(tok_one(Token::Percentage)),

            '-' => {
                if is_next_char(input, curr_offset, '>') {
                    result.push(LexItem::new(Token::RTArrow, Span::two(curr_offset)));
                    curr_offset += 1;
                } else {
                    result.push(tok_one(Token::Minus));
                }
            }

            '=' => push_with_equal(Token::EqualEqual, Token::Equal),
            '!' => push_with_equal(Token::BangEqual, Token::Bang),
            '<' => push_with_equal(Token::LessEqual, Token::Less),
            '>' => push_with_equal(Token::GreaterEqual, Token::Greater),
            '"' => result.push(lex_string(input, &mut curr_offset)?),

            v if v.is_ascii_digit() => result.push(lex_number(input, &mut curr_offset)?),
            v if is_keyword_or_identifier_char(v) => {
                result.push(lex_keyword_or_identifier(input, &mut curr_offset))
            }
            _ => {
                return Err(ParseError::UnexpectedCharacter(Span::one(curr_offset)));
            }
        }
        curr_offset += 1;
    }

    Ok(result)
}

fn lex_number(input: &str, offset: &mut usize) -> Result<LexItem, ParseError> {
    let start_offset = *offset;
    let mut has_parsed_dot = false;

    while let Some(c) = input.chars().nth(*offset + 1) {
        if !(c.is_ascii_digit() || c == '.') {
            break;
        }
        if c == '.' {
            if has_parsed_dot {
                return Err(ParseError::UnexpectedCharacter(Span::one(*offset)));
            }
            has_parsed_dot = true
        }
        *offset += 1;
    }

    Ok(LexItem::new(
        Token::Number,
        Span::new(start_offset, *offset),
    ))
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

    let id = &input[start_offset..(*offset + 1)];
    let span = Span::new(start_offset, *offset);

    match KEYWORDS.get(id) {
        Some(&token) => LexItem::new(token, span),
        None => LexItem::new(Token::Identifier, span),
    }
}
