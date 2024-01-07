use crate::ast::{
    BinaryOpNode, Expression, FnCallNode, FnDeclNode, IfNode, Statement, StatementList, WhileNode,
};
use crate::lex::LexItem;
use crate::parse_error::ParseError;
use crate::span::Span;
use crate::token::Token;
use log::trace;

/*
stmt         = (reassignment | declaration | print | expr | if | while )
print        = "print" expr
reassignment = IDENTIFIER "=" expr
declaration  = "var" IDENTIFIER "=" expr
while        = "while" expr block
block        = "{" (stmt ";")* "}"
expr         = logical
logical      = equality (( "and" | "or" ) equality)*
equality     = comparison (("==" | "!=") comparison)*
comparison   = term (("<" | "<" | "<=" | ">=") term)*
term         = factor (("+" | "-") factor)*
factor       = unary (("*" | "/") unary)*
unary        = ("!" | "-")* unary | call
call         = primary "(" (expr "," ...)* ")"
primary      = STRING | NUMBER | IDENTIFIER | "true" | "false" | "nil" | group | array | function | if
if           = "if" expr block ("else" block)?
function     = "fun" "(" ( FUNC_ARG "," ... )* ")" block
array        = "[" (expr, ",")* "]"
group        = "(" expr ")"
*/

pub fn parse(input: &str, items: &[LexItem]) -> Result<Statement, ParseError> {
    trace!("parse");
    let mut curr_pos = 0;
    let mut stmts = vec![];
    while curr_pos < items.len() {
        let result = parse_stmt(input, items, &mut curr_pos)?;
        stmts.push(result);
        // try to consume ';'
        if peek(items, &[Token::Semicolon], curr_pos) {
            curr_pos += 1;
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
    trace!("parse_stmt");
    let Some(li) = items.get(*curr_pos) else {
        return Err(ParseError::Eof);
    };
    match li.token {
        Token::Print => parse_print(input, items, curr_pos),
        Token::Identifier => parse_reassignment_or_expr(input, items, curr_pos),
        Token::Var => parse_declaration(input, items, curr_pos),
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
    trace!("parse_stmt");
    consume_token(items, Token::Print, curr_pos)?;
    let expr = parse_expr(input, items, curr_pos)?;
    Ok(Statement::Print(expr))
}

fn parse_while(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Statement, ParseError> {
    trace!("parse_while");
    consume_token(items, Token::While, curr_pos)?;
    let cond = parse_expr(input, items, curr_pos)?;
    let body = parse_block_statement_list(input, items, curr_pos)?;
    Ok(Statement::While(WhileNode { cond, body }))
}

fn parse_block(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Statement, ParseError> {
    trace!("parse_block");
    Ok(Statement::Block(parse_block_statement_list(
        input, items, curr_pos,
    )?))
}

fn parse_block_statement_list(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<StatementList, ParseError> {
    trace!("parse_block_statement_list");
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
    trace!("parse_declaration");
    consume_token(items, Token::Var, curr_pos)?;

    let id_item = consume_token(items, Token::Identifier, curr_pos)?;

    consume_token(items, Token::Equal, curr_pos)?;

    let expr = parse_expr(input, items, curr_pos)?;

    let name = id_item.span.str_from_source(input);
    Ok(Statement::Declare(name.to_string(), expr))
}

fn parse_reassignment(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Statement, ParseError> {
    trace!("parse_reassignment");
    let id_item = consume_token(items, Token::Identifier, curr_pos)?;
    consume_token(items, Token::Equal, curr_pos)?;
    let expr = parse_expr(input, items, curr_pos)?;
    let name = id_item.span.str_from_source(input);
    Ok(Statement::Reassign(name.to_string(), expr))
}

fn parse_expr(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    trace!("parse_expr");
    parse_logical(input, items, curr_pos)
}

fn parse_reassignment_or_expr(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Statement, ParseError> {
    trace!("parse_reassignment_or_expr");
    if peek_2_token(items, &[Token::Equal], *curr_pos) {
        parse_reassignment(input, items, curr_pos)
    } else {
        parse_expr(input, items, curr_pos).map(Statement::Expr)
    }
}

fn parse_logical(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    trace!("parse_logical");
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
    trace!("parse_equality");
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
    trace!("parse_comparison");
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
    trace!("parse_term");
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
    trace!("parse_factor");
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
    trace!("parse_recursive_binary");
    let mut lhs = lower_fn(input, items, curr_pos)?;

    while let Some(op) = items.get(*curr_pos) {
        if !match_tokens.contains(&op.token) {
            break;
        }
        *curr_pos += 1;
        let rhs = lower_fn(input, items, curr_pos)?;
        lhs = Expression::BinaryOp(BinaryOpNode {
            lhs: Box::new(lhs),
            op: op.token,
            rhs: Box::new(rhs),
        });
    }

    Ok(lhs)
}

fn parse_unary(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    trace!("parse_unary");
    let Some(li) = items.get(*curr_pos) else {
            return Err(ParseError::Eof);
    };
    match li.token {
        Token::Bang => parse_recursive_unary(input, items, Token::Bang, curr_pos),
        Token::Minus => parse_recursive_unary(input, items, Token::Minus, curr_pos),
        _ => parse_call(input, items, curr_pos),
    }
}

fn parse_recursive_unary(
    input: &str,
    items: &[LexItem],
    match_token: Token,
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    trace!("parse_recursive_unary");
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
        parse_call(input, items, curr_pos)
    }
}

fn parse_call(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    trace!("parse_call");
    let prim = parse_primary(input, items, curr_pos)?;
    if let Expression::Identifier(name) = &prim {
        if peek(items, &[Token::LRoundParen], *curr_pos) {
            let args = parse_comma_list(
                input,
                items,
                curr_pos,
                Token::LRoundParen,
                Token::RRoundParen,
                parse_expr,
            )?;
            return Ok(Expression::FnCall(FnCallNode {
                name: name.to_string(),
                args,
            }));
        }
    }
    Ok(prim)
}

fn parse_primary(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    trace!("parse_primary");
    let Some(li) = items.get(*curr_pos) else {
            return Err(ParseError::Eof);
    };

    let mut next = |expr| {
        *curr_pos += 1;
        Ok(expr)
    };

    match li.token {
        Token::Nil => next(Expression::Nil),
        Token::True => next(Expression::Bool(true)),
        Token::False => next(Expression::Bool(false)),
        Token::String => {
            next(Expression::Str(
                // remove start '"' and end '"'
                Span::new(li.span.start + 1, li.span.end - 1)
                    .str_from_source(input)
                    .to_string(),
            ))
        }
        Token::Identifier => next(Expression::Identifier(
            li.span.str_from_source(input).to_string(),
        )),
        Token::If => parse_if(input, items, curr_pos),
        Token::Number => parse_number(input, items, curr_pos),
        Token::LSquareParen => parse_array(input, items, curr_pos),
        Token::LRoundParen => parse_group(input, items, curr_pos),
        Token::Fn => parse_function(input, items, curr_pos),
        _ => Err(ParseError::UnexpectedToken(li.token, li.span, None)),
    }
}

fn parse_group(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    trace!("parse_group");
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
    trace!("parse_array");
    let exprs = parse_comma_list(
        input,
        items,
        curr_pos,
        Token::LSquareParen,
        Token::RSquareParen,
        parse_expr,
    )?;
    Ok(Expression::Array(exprs))
}

fn parse_comma_list<F, T>(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
    left_paren: Token,
    right_paren: Token,
    lower_fn: F,
) -> Result<Vec<T>, ParseError>
where
    F: Fn(&str, &[LexItem], &mut usize) -> Result<T, ParseError>,
{
    trace!("parse_comma_list");
    consume_token(items, left_paren, curr_pos)?;

    let mut result = Vec::new();
    let mut has_consumed_comma = true;

    while let Some(li) = items.get(*curr_pos) {
        if li.token == right_paren {
            break;
        }

        if !has_consumed_comma {
            return Err(ParseError::UnexpectedToken(
                li.token,
                li.span,
                Some(Token::Comma),
            ));
        }

        let expr = lower_fn(input, items, curr_pos)?;
        result.push(expr);
        has_consumed_comma = false;

        if let Some(next) = items.get(*curr_pos) {
            if next.token == Token::Comma {
                *curr_pos += 1;
                has_consumed_comma = true;
            }
        };
    }

    consume_token(items, right_paren, curr_pos)?;

    Ok(result)
}

fn parse_number(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    trace!("parse_number");
    let li = consume_token(items, Token::Number, curr_pos)?;
    let source = li.span.str_from_source(input);
    match source.parse::<f64>() {
        Err(_) => Err(ParseError::ParseToNumber(li.span)),
        Ok(num) => Ok(Expression::Number(num)),
    }
}

fn get_identifier(_: &str, items: &[LexItem], curr_pos: &mut usize) -> Result<LexItem, ParseError> {
    let li = consume_token(items, Token::Identifier, curr_pos)?;
    Ok(li.to_owned())
}

fn parse_if(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    trace!("parse_if");
    consume_token(items, Token::If, curr_pos)?;

    let cond = parse_expr(input, items, curr_pos)?;
    let if_stmts = parse_block_statement_list(input, items, curr_pos)?;
    let mut else_stmts = None;

    if peek(items, &[Token::Else], *curr_pos) {
        consume_token(items, Token::Else, curr_pos)?;
        else_stmts = Some(parse_block_statement_list(input, items, curr_pos)?);
    }

    Ok(Expression::If(IfNode {
        cond: Box::new(cond),
        if_stmts,
        else_stmts,
    }))
}
fn parse_function(
    input: &str,
    items: &[LexItem],
    curr_pos: &mut usize,
) -> Result<Expression, ParseError> {
    trace!("parse_identifier");
    consume_token(items, Token::Fn, curr_pos)?;
    let arg_names = parse_comma_list(
        input,
        items,
        curr_pos,
        Token::LRoundParen,
        Token::RRoundParen,
        get_identifier,
    )?
    .into_iter()
    .map(|li| li.span.str_from_source(input).to_string())
    .collect();

    let body = parse_block_statement_list(input, items, curr_pos)?;

    Ok(Expression::FnDecl(FnDeclNode { arg_names, body }))
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

fn peek(items: &[LexItem], match_tokens: &'static [Token], curr_pos: usize) -> bool {
    items
        .get(curr_pos)
        .map(|li| match_tokens.contains(&li.token))
        .unwrap_or(false)
}

fn peek_2_token(items: &[LexItem], match_tokens: &'static [Token], curr_pos: usize) -> bool {
    items
        .get(curr_pos + 1)
        .map(|li| match_tokens.contains(&li.token))
        .unwrap_or(false)
}
