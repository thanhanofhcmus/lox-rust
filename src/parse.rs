use crate::expr::Expression;
use crate::lex::LexItem;
use crate::parse_error::ParseError;
use crate::token::Token;

pub fn parse(input: &str, items: &[LexItem]) -> Result<Expression, ParseError> {
    let mut curr_pos = 0;
    let result = parse_expr(input, items, &mut curr_pos)?;
    match items.get(curr_pos) {
        None => Ok(result),
        Some(_) => Err(ParseError::Unfinished),
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
    let mut lhs = parse_term(input, items, curr_pos)?;

    while let Some(op) = items.get(*curr_pos) {
        if op.token != Token::And && op.token != Token::Or {
            break;
        }
        *curr_pos += 1;
        let rhs = parse_term(input, items, curr_pos)?;
        lhs = Expression::BinaryOp(Box::new(lhs), op.token, Box::new(rhs));
    }

    Ok(lhs)
}

fn parse_term(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    let mut lhs = parse_factor(input, items, curr_pos)?;

    while let Some(op) = items.get(*curr_pos) {
        if op.token != Token::Plus && op.token != Token::Minus {
            break;
        }
        *curr_pos += 1;
        let rhs = parse_factor(input, items, curr_pos)?;
        lhs = Expression::BinaryOp(Box::new(lhs), op.token, Box::new(rhs));
    }

    Ok(lhs)
}

fn parse_factor(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    let mut lhs = parse_unary(input, items, curr_pos)?;

    while let Some(op) = items.get(*curr_pos) {
        if op.token != Token::Star && op.token != Token::Slash {
            break;
        }
        *curr_pos += 1;
        let rhs = parse_unary(input, items, curr_pos)?;
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
        Token::Bang => parse_unary_bang(input, items, curr_pos),
        // Token::Minus
        _ => parse_primary(input, items, curr_pos),
    }
}

fn parse_unary_bang(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    let Some(li) = items.get(*curr_pos) else {
            return Err(ParseError::Eof);
    };

    if li.token == Token::Bang {
        *curr_pos += 1;
        Ok(Expression::UnaryOp(
            Box::new(parse_unary_bang(input, items, curr_pos)?),
            Token::Bang,
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
        Token::True => {
            *curr_pos += 1;
            Ok(Expression::Bool(true))
        }
        Token::False => {
            *curr_pos += 1;
            Ok(Expression::Bool(false))
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
    // TODO: consider where to increase curr_pos
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
