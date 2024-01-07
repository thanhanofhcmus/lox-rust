use crate::lex::LexItem;
use crate::parse_error::ParseError;
use crate::token::Token;
use std::str::from_utf8;

#[derive(Debug)]
pub enum Expression {
    NoOp,
    Number(f64),
    BinaryOp(Box<Expression>, Token, Box<Expression>),
}

pub fn parse(input: &[u8], items: &[LexItem]) -> Result<Expression, ParseError> {
    let mut curr_pos = 0;
    parse_expr(input, items, &mut curr_pos)
}

fn parse_expr(
    input: &[u8],
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    parse_term(input, items, curr_pos)
}

fn parse_term(
    input: &[u8],
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    let lhs = parse_primary(input, items, curr_pos)?;
    // TODO: check for rhs

    Ok(lhs)
}

fn parse_primary(
    input: &[u8],
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    let Some(li) = items.get(*curr_pos) else {
            return Err(ParseError::Eof);
    };
    match li.token {
        Token::Number => parse_number(input, items, curr_pos),
        Token::LeftParen => parse_group(input, items, curr_pos),
        _ => Err(ParseError::UnexpectedToken(li.token, li.span)),
    }
}

fn parse_group(
    input: &[u8],
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    *curr_pos += 1; // consume '('
    let expr = parse_expr(input, items, curr_pos)?;
    *curr_pos += 1;

    if items.get(*curr_pos).is_none() {
        return Err(ParseError::Eof);
    }
    Ok(expr)
}

fn parse_number(
    input: &[u8],
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    let Some(li) = items.get(*curr_pos) else {
            return Err(ParseError::Eof);
    };
    // TODO: consider where to increase curr_pos
    *curr_pos += 1;

    if li.token != Token::Number {
        return Err(ParseError::UnexpectedToken(li.token, li.span));
    }

    let source = li.span.extract_from_source(input);
    let Ok(s) = from_utf8(source) else {
        return Err(ParseError::ConvertFromUtf8(li.span))
    };
    match s.parse::<f64>() {
        Err(_) => Err(ParseError::ParseToNumber(li.span)),
        Ok(num) => Ok(Expression::Number(num)),
    }
}
