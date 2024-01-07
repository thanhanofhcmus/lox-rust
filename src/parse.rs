use crate::expr::Expression;
use crate::lex::LexItem;
use crate::parse_error::ParseError;
use crate::span::Span;
use crate::token::Token;

pub fn parse(input: &str, items: &[LexItem]) -> Result<Expression, ParseError> {
    let mut curr_pos = 0;
    let result = parse_expr(input, items, &mut curr_pos)?;
    match items.get(curr_pos) {
        None => Ok(result),
        Some(li) => Err(ParseError::Unfinished(li.token, li.span)),
    }
}

fn parse_expr(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    parse_logical(input, items, curr_pos)
}

fn parse_logical(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    parse_recursive_binary(
        input,
        items,
        curr_pos,
        &[Token::And, Token::Or],
        parse_equality,
    )
}

fn parse_equality(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    parse_recursive_binary(
        input,
        items,
        curr_pos,
        &[Token::EqualEqual, Token::BangEqual],
        parse_comparision,
    )
}

fn parse_comparision(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    parse_recursive_binary(
        input,
        items,
        curr_pos,
        &[
            Token::Less,
            Token::LessEqual,
            Token::Greater,
            Token::GreaterEqual,
        ],
        parse_term,
    )
}

fn parse_term(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    parse_recursive_binary(
        input,
        items,
        curr_pos,
        &[Token::Plus, Token::Minus],
        parse_factor,
    )
}

fn parse_factor(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    parse_recursive_binary(
        input,
        items,
        curr_pos,
        &[Token::Star, Token::Slash],
        parse_unary,
    )
}

fn parse_recursive_binary<F>(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
    match_tokens: &'static [Token],
    lower_fn: F,
) -> Result<Expression, ParseError>
where
    F: Fn(&str, &[LexItem], &mut usize) -> Result<Expression, ParseError>,
{
    let mut lhs = lower_fn(input, items, curr_pos)?;

    while let Some(op) = items.get(*curr_pos) {
        if !match_tokens.contains(&op.token) {
            break;
        }
        *curr_pos += 1;
        let rhs = lower_fn(input, items, curr_pos)?;
        lhs = Expression::BinaryOp(Box::new(lhs), op.token, Box::new(rhs));
    }

    Ok(lhs)
}

fn parse_unary(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    let Some(li) = items.get(*curr_pos) else {
            return Err(ParseError::Eof);
    };
    match li.token {
        Token::Bang => parse_recursive_unary(input, items, Token::Bang, curr_pos),
        Token::Minus => parse_recursive_unary(input, items, Token::Minus, curr_pos),
        _ => parse_primary(input, items, curr_pos),
    }
}

fn parse_recursive_unary(
    input: &str,
    items: &[LexItem],
    match_token: Token,
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    let Some(li) = items.get(*curr_pos) else {
            return Err(ParseError::Eof);
    };

    if li.token == match_token {
        *curr_pos += 1;
        Ok(Expression::UnaryOp(
            Box::new(parse_recursive_unary(input, items, match_token, curr_pos)?),
            match_token,
        ))
    } else {
        parse_primary(input, items, curr_pos)
    }
}

fn parse_primary(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    let Some(li) = items.get(*curr_pos) else {
            return Err(ParseError::Eof);
    };

    match li.token {
        Token::Nil => {
            *curr_pos += 1;
            Ok(Expression::Nil)
        }
        Token::True => {
            *curr_pos += 1;
            Ok(Expression::Bool(true))
        }
        Token::False => {
            *curr_pos += 1;
            Ok(Expression::Bool(false))
        }
        Token::String => {
            *curr_pos += 1;
            Ok(Expression::Str(
                // remove start '"' and end '"'
                Span::new(li.span.start + 1, li.span.end - 1)
                    .extract_from_source(input)
                    .to_string(),
            ))
        }
        Token::Number => parse_number(input, items, curr_pos),
        Token::LeftParen => parse_group(input, items, curr_pos),
        _ => Err(ParseError::UnexpectedToken(li.token, li.span)),
    }
}

fn parse_group(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    *curr_pos += 1; // consume '('
    let expr = parse_expr(input, items, curr_pos)?;

    // now curr_pos must be at token ')'
    let Some(li) = items.get(*curr_pos) else {
        return Err(ParseError::Eof);
    };

    if li.token != Token::RightParen {
        return Err(ParseError::UnexpectedToken(li.token, li.span));
    }

    *curr_pos += 1;
    Ok(expr)
}

fn parse_number(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    let Some(li) = items.get(*curr_pos) else {
            return Err(ParseError::Eof);
    };
    *curr_pos += 1;

    if li.token != Token::Number {
        return Err(ParseError::UnexpectedToken(li.token, li.span));
    }

    let source = li.span.extract_from_source(input);
    match source.parse::<f64>() {
        Err(_) => Err(ParseError::ParseToNumber(li.span)),
        Ok(num) => Ok(Expression::Number(num)),
    }
}
