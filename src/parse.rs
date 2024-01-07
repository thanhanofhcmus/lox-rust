use crate::ast::{Expression, Statement, StatementList};
use crate::lex::LexItem;
use crate::parse_error::ParseError;
use crate::span::Span;
use crate::token::Token;

pub fn parse(input: &str, items: &[LexItem]) -> Result<Statement, ParseError> {
    let mut curr_pos = 0;
    let mut stmts = vec![];
    while curr_pos < items.len() {
        let result = parse_stmt(input, items, &mut curr_pos)?;
        stmts.push(result);
        // try to consume ';'
        if let Some(li) = items.get(curr_pos) {
            if li.token == Token::Semicolon {
                curr_pos += 1;
            }
        }
    }
    match items.get(curr_pos) {
        None => Ok(Statement::Global(stmts)),
        Some(li) => Err(ParseError::Unfinished(li.token, li.span)),
    }
}

fn parse_stmt(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Statement, ParseError> {
    let Some(li) = items.get(*curr_pos) else {
        return Err(ParseError::Eof);
    };
    match li.token {
        Token::Print => parse_print(input, items, curr_pos),
        Token::Identifier => parse_reassignment(input, items, curr_pos),
        Token::Var => parse_declaration(input, items, curr_pos),
        Token::If => parse_if(input, items, curr_pos),
        Token::While => parse_while(input, items, curr_pos),
        Token::LPointParen => parse_block(input, items, curr_pos),
        _ => parse_expr(input, items, curr_pos).map(Statement::Expr),
    }
}

fn parse_print(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Statement, ParseError> {
    consume_token(items, Token::Print, curr_pos)?;
    let expr = parse_expr(input, items, curr_pos)?;
    Ok(Statement::Pritnt(expr))
}

fn parse_if(input: &str, items: &[LexItem], curr_pos: &mut usize) -> Result<Statement, ParseError> {
    consume_token(items, Token::If, curr_pos)?;

    let cond = parse_expr(input, items, curr_pos)?;
    let if_stmts = parse_block_statement_list(input, items, curr_pos)?;
    let mut else_stmt = None;

    if peek(items, &[Token::Else], *curr_pos) {
        consume_token(items, Token::Else, curr_pos)?;
        else_stmt = Some(parse_block_statement_list(input, items, curr_pos)?);
    }

    Ok(Statement::If(cond, if_stmts, else_stmt))
}

fn parse_while(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Statement, ParseError> {
    consume_token(items, Token::While, curr_pos)?;
    let cond = parse_expr(input, items, curr_pos)?;
    let stmts = parse_block_statement_list(input, items, curr_pos)?;
    Ok(Statement::While(cond, stmts))
}

fn parse_block(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Statement, ParseError> {
    Ok(Statement::Block(parse_block_statement_list(
        input, items, curr_pos,
    )?))
}

fn parse_block_statement_list(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<StatementList, ParseError> {
    consume_token(items, Token::LPointParen, curr_pos)?;

    let mut stmts = vec![];

    while let Some(li) = items.get(*curr_pos) {
        if li.token == Token::RPointParen {
            break;
        }

        let stmt = parse_stmt(input, items, curr_pos)?;

        // TODO: maybe allow optional ';' at the last stmt
        consume_token(items, Token::Semicolon, curr_pos)?;

        stmts.push(stmt);
    }

    consume_token(items, Token::RPointParen, curr_pos)?;

    Ok(stmts)
}

fn parse_declaration(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Statement, ParseError> {
    consume_token(items, Token::Var, curr_pos)?;

    let id_item = consume_token(items, Token::Identifier, curr_pos)?;

    consume_token(items, Token::Equal, curr_pos)?;

    let expr = parse_expr(input, items, curr_pos)?;

    let name = id_item.span.extract_from_source(input);
    Ok(Statement::Declare(name.to_string(), expr))
}

fn parse_reassignment(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Statement, ParseError> {
    let id_item = consume_token(items, Token::Identifier, curr_pos)?;
    consume_token(items, Token::Equal, curr_pos)?;
    let expr = parse_expr(input, items, curr_pos)?;
    let name = id_item.span.extract_from_source(input);
    Ok(Statement::Reassign(name.to_string(), expr))
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
        parse_comparison,
    )
}

fn parse_comparison(
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
        Token::Identifier => {
            *curr_pos += 1;
            Ok(Expression::Identifier(
                li.span.extract_from_source(input).to_string(),
            ))
        }
        Token::Number => parse_number(input, items, curr_pos),
        Token::LSquareParen => parse_array(input, items, curr_pos),
        Token::LRoundParen => parse_group(input, items, curr_pos),
        _ => Err(ParseError::UnexpectedToken(li.token, li.span, None)),
    }
}

fn parse_group(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    consume_token(items, Token::LRoundParen, curr_pos)?;
    let expr = parse_expr(input, items, curr_pos)?;
    consume_token(items, Token::RRoundParen, curr_pos)?;
    Ok(expr)
}

fn parse_array(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    consume_token(items, Token::LSquareParen, curr_pos)?;

    let mut exprs = vec![];
    let mut has_consumed_comma = true;

    while let Some(li) = items.get(*curr_pos) {
        if li.token == Token::RSquareParen {
            break;
        }

        if !has_consumed_comma {
            return Err(ParseError::UnexpectedToken(
                li.token,
                li.span,
                Some(Token::Comma),
            ));
        }

        let expr = parse_expr(input, items, curr_pos)?;
        exprs.push(expr);
        has_consumed_comma = false;

        if let Some(next) = items.get(*curr_pos) {
            if next.token == Token::Comma {
                *curr_pos += 1;
                has_consumed_comma = true;
            }
        };
    }

    consume_token(items, Token::RSquareParen, curr_pos)?;

    Ok(Expression::Array(exprs))
}

fn parse_number(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    let li = consume_token(items, Token::Number, curr_pos)?;
    let source = li.span.extract_from_source(input);
    match source.parse::<f64>() {
        Err(_) => Err(ParseError::ParseToNumber(li.span)),
        Ok(num) => Ok(Expression::Number(num)),
    }
}

fn consume_token<'a>(
    items: &'a [LexItem],
    token: Token,
    curr_pos: &mut usize,
) -> Result<&'a LexItem, ParseError> {
    let Some(li) = items.get(*curr_pos) else {
        return Err(ParseError::Eof);
    };
    if li.token != token {
        return Err(ParseError::UnexpectedToken(li.token, li.span, Some(token)));
    }
    *curr_pos += 1;
    Ok(li)
}

fn peek(items: &[LexItem], match_tokens: &'static [Token], curr_poss: usize) -> bool {
    items
        .get(curr_poss)
        .map(|li| match_tokens.contains(&li.token))
        .unwrap_or(false)
}
