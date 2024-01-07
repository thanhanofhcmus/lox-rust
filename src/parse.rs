use crate::ast::{
    BinaryOpNode, Expression, FnCallNode, FnDeclNode, IfStmtNode, Statement, StatementList,
    TernaryExprNode, WhileNode,
};
use crate::lex::LexItem;
use crate::parse_error::ParseError;
use crate::span::Span;
use crate::token::Token;
use log::trace;

/*
stmt         = reassignment | declaration | print | expr | if | while | return
print        = "print" expr
if           = "if" expr block ("else" block)?
declaration  = "var" IDENTIFIER "=" expr
while        = "while" expr block
return       = "return" (expr)?
block        = "{" (stmt ";")* "}"
reassignment = IDENTIFIER "=" expr
expr         = logical | ternary
ternary      = "when" logical "then" logical "else" logical
logical      = equality (( "and" | "or" ) equality)*
equality     = comparison (("==" | "!=") comparison)*
comparison   = term (("<" | "<" | "<=" | ">=") term)*
term         = factor (("+" | "-") factor)*
factor       = unary (("*" | "/") unary)*
unary        = ("!" | "-")* unary | call
call         = primary "(" (expr "," ...)* ")"
primary      = STRING | NUMBER | IDENTIFIER | "true" | "false" | "nil" | group | array | function
function     = "fn" "(" ( FUNC_ARG "," ... )* ")" block
array        = "[" (expr, ",")* "]"
group        = "(" expr ")"
*/

struct ParseState<'a> {
    input: &'a str,
    items: &'a [LexItem],
    curr_pos: usize,
    is_in_fn: bool,
}

impl<'a> ParseState<'a> {
    fn new(input: &'a str, items: &'a [LexItem]) -> Self {
        Self {
            input,
            items,
            curr_pos: 0,
            is_in_fn: false,
        }
    }
}

pub fn parse(input: &str, items: &[LexItem]) -> Result<Statement, ParseError> {
    trace!("parse");
    let mut stmts = vec![];
    let mut state = ParseState::new(input, items);
    while state.curr_pos < items.len() {
        let result = parse_stmt(&mut state)?;
        stmts.push(result);
        // try to consume ';'
        if peek(&state, &[Token::Semicolon]) {
            state.curr_pos += 1;
        }
    }
    match items.get(state.curr_pos) {
        None => Ok(Statement::Global(stmts)),
        Some(li) => Err(ParseError::Unfinished(li.token, li.span)),
    }
}

fn parse_stmt(state: &mut ParseState) -> Result<Statement, ParseError> {
    trace!("parse_stmt");
    let li = get_curr(state)?;
    match li.token {
        Token::Print => parse_print(state),
        Token::Identifier => parse_reassignment_or_expr(state),
        Token::Var => parse_declaration(state),
        Token::While => parse_while(state),
        Token::If => parse_if(state),
        Token::Return => parse_return(state),
        Token::LPointParen => parse_block(state),
        _ => parse_expr(state).map(Statement::Expr),
    }
}

fn parse_print(state: &mut ParseState) -> Result<Statement, ParseError> {
    trace!("parse_stmt");
    consume_token(state, Token::Print)?;
    let expr = parse_expr(state)?;
    Ok(Statement::Print(expr))
}

fn parse_if(state: &mut ParseState) -> Result<Statement, ParseError> {
    trace!("parse_if");
    consume_token(state, Token::If)?;

    let cond = parse_expr(state)?;
    let if_stmts = parse_block_statement_list(state)?;
    let mut else_stmts = None;

    if peek(state, &[Token::Else]) {
        consume_token(state, Token::Else)?;
        else_stmts = Some(parse_block_statement_list(state)?);
    }

    Ok(Statement::If(IfStmtNode {
        cond,
        if_stmts,
        else_stmts,
    }))
}
fn parse_while(state: &mut ParseState) -> Result<Statement, ParseError> {
    trace!("parse_while");
    consume_token(state, Token::While)?;
    let cond = parse_expr(state)?;
    let body = parse_block_statement_list(state)?;
    Ok(Statement::While(WhileNode { cond, body }))
}

fn parse_return(state: &mut ParseState) -> Result<Statement, ParseError> {
    consume_token(state, Token::Return)?;
    if !state.is_in_fn {
        return Err(ParseError::UnexpectedReturn(Span::one(state.curr_pos)));
    }
    let expr = parse_expr(state)?;
    Ok(Statement::Return(expr))
}

fn parse_block(state: &mut ParseState) -> Result<Statement, ParseError> {
    trace!("parse_block");
    Ok(Statement::Block(parse_block_statement_list(state)?))
}

fn parse_block_statement_list(state: &mut ParseState) -> Result<StatementList, ParseError> {
    trace!("parse_block_statement_list");
    consume_token(state, Token::LPointParen)?;

    let mut stmts = vec![];

    while let Some(li) = state.items.get(state.curr_pos) {
        if li.token == Token::RPointParen {
            break;
        }

        let stmt = parse_stmt(state)?;

        // TODO: maybe allow optional ';' at the last stmt
        if !peek(state, &[Token::RPointParen]) {
            consume_token(state, Token::Semicolon)?;
        }

        stmts.push(stmt);
    }

    consume_token(state, Token::RPointParen)?;

    Ok(stmts)
}

fn parse_declaration(state: &mut ParseState) -> Result<Statement, ParseError> {
    trace!("parse_declaration");
    consume_token(state, Token::Var)?;
    let id_item = consume_token(state, Token::Identifier)?;
    consume_token(state, Token::Equal)?;
    let expr = parse_expr(state)?;
    let name = id_item.span.str_from_source(state.input);
    Ok(Statement::Declare(name.to_string(), expr))
}

fn parse_reassignment(state: &mut ParseState) -> Result<Statement, ParseError> {
    trace!("parse_reassignment");
    let id_item = consume_token(state, Token::Identifier)?;
    consume_token(state, Token::Equal)?;
    let expr = parse_expr(state)?;
    let name = id_item.span.str_from_source(state.input);
    Ok(Statement::Reassign(name.to_string(), expr))
}

fn parse_reassignment_or_expr(state: &mut ParseState) -> Result<Statement, ParseError> {
    trace!("parse_reassignment_or_expr");
    if peek_2_token(state, &[Token::Equal]) {
        parse_reassignment(state)
    } else {
        parse_expr(state).map(Statement::Expr)
    }
}

fn parse_expr(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_expr");
    if peek(state, &[Token::When]) {
        parse_ternary(state)
    } else {
        parse_logical(state)
    }
}

fn parse_ternary(state: &mut ParseState) -> Result<Expression, ParseError> {
    consume_token(state, Token::When)?;
    let cond = parse_logical(state)?;
    consume_token(state, Token::Then)?;
    let true_expr = parse_logical(state)?;
    consume_token(state, Token::Else)?;
    let false_expr = parse_logical(state)?;
    Ok(Expression::Ternary(TernaryExprNode {
        cond: Box::new(cond),
        true_expr: Box::new(true_expr),
        false_expr: Box::new(false_expr),
    }))
}

fn parse_logical(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_logical");
    parse_recursive_binary(state, &[Token::And, Token::Or], parse_equality)
}

fn parse_equality(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_equality");
    parse_recursive_binary(
        state,
        &[Token::EqualEqual, Token::BangEqual],
        parse_comparison,
    )
}

fn parse_comparison(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_comparison");
    parse_recursive_binary(
        state,
        &[
            Token::Less,
            Token::LessEqual,
            Token::Greater,
            Token::GreaterEqual,
        ],
        parse_term,
    )
}

fn parse_term(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_term");
    parse_recursive_binary(state, &[Token::Plus, Token::Minus], parse_factor)
}

fn parse_factor(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_factor");
    parse_recursive_binary(state, &[Token::Star, Token::Slash], parse_unary)
}

fn parse_recursive_binary<F>(
    state: &mut ParseState,
    match_tokens: &'static [Token],
    lower_fn: F,
) -> Result<Expression, ParseError>
where
    F: Fn(&mut ParseState) -> Result<Expression, ParseError>,
{
    trace!("parse_recursive_binary");
    let mut lhs = lower_fn(state)?;

    while let Some(op) = state.items.get(state.curr_pos) {
        if !match_tokens.contains(&op.token) {
            break;
        }
        state.curr_pos += 1;
        let rhs = lower_fn(state)?;
        lhs = Expression::BinaryOp(BinaryOpNode {
            lhs: Box::new(lhs),
            op: op.token,
            rhs: Box::new(rhs),
        });
    }

    Ok(lhs)
}

fn parse_unary(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_unary");
    let li = get_curr(state)?;
    match li.token {
        Token::Bang => parse_recursive_unary(state, Token::Bang),
        Token::Minus => parse_recursive_unary(state, Token::Minus),
        _ => parse_call(state),
    }
}

fn parse_recursive_unary(
    state: &mut ParseState,
    match_token: Token,
) -> Result<Expression, ParseError> {
    trace!("parse_recursive_unary");
    let li = get_curr(state)?;
    if li.token == match_token {
        state.curr_pos += 1;
        Ok(Expression::UnaryOp(
            Box::new(parse_recursive_unary(state, match_token)?),
            match_token,
        ))
    } else {
        parse_call(state)
    }
}

fn parse_call(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_call");
    let prim = parse_primary(state)?;
    if let Expression::Identifier(name) = &prim {
        if peek(state, &[Token::LRoundParen]) {
            let args = parse_comma_list(state, Token::LRoundParen, Token::RRoundParen, parse_expr)?;
            return Ok(Expression::FnCall(FnCallNode {
                name: name.to_string(),
                args,
            }));
        }
    }
    Ok(prim)
}

fn parse_primary(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_primary");
    let li = *get_curr(state)?;
    let mut next = |expr| {
        state.curr_pos += 1;
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
                    .str_from_source(state.input)
                    .to_string(),
            ))
        }
        Token::Identifier => next(Expression::Identifier(
            li.span.str_from_source(state.input).to_string(),
        )),
        Token::Number => parse_number(state),
        Token::LSquareParen => parse_array(state),
        Token::LRoundParen => parse_group(state),
        Token::Fn => parse_function(state),
        _ => Err(ParseError::UnexpectedToken(li.token, li.span, None)),
    }
}

fn parse_group(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_group");
    consume_token(state, Token::LRoundParen)?;
    let expr = parse_expr(state)?;
    consume_token(state, Token::RRoundParen)?;
    Ok(expr)
}

fn parse_array(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_array");
    let exprs = parse_comma_list(state, Token::LSquareParen, Token::RSquareParen, parse_expr)?;
    Ok(Expression::Array(exprs))
}

fn parse_function(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_identifier");
    consume_token(state, Token::Fn)?;
    let arg_names = parse_comma_list(
        state,
        Token::LRoundParen,
        Token::RRoundParen,
        get_identifier,
    )?
    .into_iter()
    .map(|li| li.span.str_from_source(state.input).to_string())
    .collect();

    state.is_in_fn = true;
    let body = parse_block_statement_list(state)?;
    state.is_in_fn = false;

    Ok(Expression::FnDecl(FnDeclNode { arg_names, body }))
}

fn parse_number(state: &mut ParseState) -> Result<Expression, ParseError> {
    trace!("parse_number");
    let li = consume_token(state, Token::Number)?;
    let source = li.span.str_from_source(state.input);
    match source.parse::<f64>() {
        Err(_) => Err(ParseError::ParseToNumber(li.span)),
        Ok(num) => Ok(Expression::Number(num)),
    }
}

fn get_identifier(state: &mut ParseState) -> Result<LexItem, ParseError> {
    let li = consume_token(state, Token::Identifier)?;
    Ok(li.to_owned())
}

fn parse_comma_list<F, T>(
    state: &mut ParseState,
    left_paren: Token,
    right_paren: Token,
    lower_fn: F,
) -> Result<Vec<T>, ParseError>
where
    F: Fn(&mut ParseState) -> Result<T, ParseError>,
{
    trace!("parse_comma_list");
    consume_token(state, left_paren)?;

    let mut result = Vec::new();
    let mut has_consumed_comma = true;

    while let Some(li) = state.items.get(state.curr_pos) {
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

        let expr = lower_fn(state)?;
        result.push(expr);
        has_consumed_comma = false;

        if let Some(next) = state.items.get(state.curr_pos) {
            if next.token == Token::Comma {
                state.curr_pos += 1;
                has_consumed_comma = true;
            }
        };
    }

    consume_token(state, right_paren)?;

    Ok(result)
}

fn consume_token(state: &mut ParseState, token: Token) -> Result<LexItem, ParseError> {
    let li = *get_curr(state)?;
    if li.token != token {
        return Err(ParseError::UnexpectedToken(li.token, li.span, Some(token)));
    }
    state.curr_pos += 1;
    Ok(li)
}

fn peek(state: &ParseState, match_tokens: &'static [Token]) -> bool {
    state
        .items
        .get(state.curr_pos)
        .map(|li| match_tokens.contains(&li.token))
        .unwrap_or(false)
}

fn peek_2_token(state: &ParseState, match_tokens: &'static [Token]) -> bool {
    state
        .items
        .get(state.curr_pos + 1)
        .map(|li| match_tokens.contains(&li.token))
        .unwrap_or(false)
}

fn get_curr<'a>(state: &'a ParseState) -> Result<&'a LexItem, ParseError> {
    match state.items.get(state.curr_pos) {
        Some(li) => Ok(li),
        None => Err(ParseError::Eof),
    }
}
