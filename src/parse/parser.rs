use crate::ast::*;

use super::context::Context;
use super::error::ParseError;
use super::lex::LexItem;

use crate::span::Span;
use crate::token::Token;

pub fn parse(input: &str, items: &[LexItem]) -> Result<AST, ParseError> {
    let mut state = Context::new(input, items);
    let mut imports = vec![];
    let mut global_stmts = vec![];
    let mut is_parsing_import = true;
    while !state.is_at_end() {
        if state.peek(&[Token::Import]) {
            if !is_parsing_import {
                let li = state.get_curr()?;
                return Err(ParseError::ImportNotAtTheTop(li.span));
            }
            let import = parse_import(&mut state)?;
            imports.push(import);
            continue;
        }

        is_parsing_import = false;

        // parse import then parse stmts
        state.prepare_next();
        let result = parse_stmt(&mut state)?;
        global_stmts.push(result);
    }
    match state.get_curr() {
        Err(_) => Ok(AST {
            imports,
            global_stmts,
        }),
        Ok(li) => Err(ParseError::Unfinished(li.token, li.span)),
    }
}

fn parse_import(state: &mut Context) -> Result<ImportNode, ParseError> {
    state.consume_token(Token::Import)?;
    let path_li = state.consume_token(Token::String)?;
    state.consume_token(Token::As)?;
    let iden_li = state.consume_token(Token::Identifier)?;
    state.consume_token(Token::Semicolon)?;
    Ok(ImportNode {
        path: Span::new(path_li.span.start + 1, path_li.span.end - 1),
        iden: IdentifierNode::new_from_name(iden_li.span, state.get_input()),
    })
}

fn parse_stmt(state: &mut Context) -> Result<Statement, ParseError> {
    let li = state.get_curr()?;
    match li.token {
        Token::Var => parse_declaration(state),
        Token::Identifier => parse_reassignment_or_expr(state),
        _ => parse_expr_stmt(state),
    }
}

fn parse_expr_stmt(state: &mut Context) -> Result<Statement, ParseError> {
    let expr = parse_expr(state)?;
    Ok(Statement::Expr(expr))
}

fn parse_block_node(state: &mut Context) -> Result<BlockNode, ParseError> {
    state.consume_token(Token::LPointParen)?;

    let mut stmts = vec![];
    let mut last_expr = None;
    while !state.peek(&[Token::RPointParen]) {
        let result = parse_stmt(state)?;
        if let Statement::Expr(expr) = result {
            if state.peek(&[Token::RPointParen]) {
                // the expression is the last statement in the chain
                last_expr = Some(Box::new(expr));
                break;
            } else {
                // normal expr statement with semicolon
                state.consume_token(Token::Semicolon)?;
                stmts.push(Statement::Expr(expr));
            }
        } else {
            stmts.push(result);
        }
    }

    state.consume_token(Token::RPointParen)?;
    Ok(BlockNode { stmts, last_expr })
}

fn parse_declaration(state: &mut Context) -> Result<Statement, ParseError> {
    state.consume_token(Token::Var)?;
    let id_item = state.consume_token(Token::Identifier)?;
    state.consume_token(Token::Equal)?;
    let expr = parse_expr(state)?;
    state.consume_token(Token::Semicolon)?;
    Ok(Statement::Declare(
        IdentifierNode::new_from_name(id_item.span, state.get_input()),
        expr,
    ))
}

fn parse_reassignment_or_expr(state: &mut Context) -> Result<Statement, ParseError> {
    if !state.peek_2_token(&[Token::Equal]) {
        let expr = parse_expr_stmt(state)?;
        if state.peek(&[Token::Semicolon]) {
            state.consume_token(Token::Semicolon)?;
        }
        return Ok(expr);
    }
    let id_item = state.consume_token(Token::Identifier)?;
    state.consume_token(Token::Equal)?;
    let expr = parse_expr(state)?;
    state.consume_token(Token::Semicolon)?;
    Ok(Statement::ReassignIden(
        IdentifierNode::new_from_name(id_item.span, state.get_input()),
        expr,
    ))
}

fn parse_expr(state: &mut Context) -> Result<Expression, ParseError> {
    match state.get_curr()?.token {
        Token::Return => parse_return(state),
        Token::When => parse_when(state),
        Token::While => parse_while(state),
        Token::If => parse_if(state),
        Token::LPointParen => parse_block_node(state).map(Expression::Block),
        _ => parse_clause_node(state).map(Expression::Clause),
    }
}

fn parse_return(state: &mut Context) -> Result<Expression, ParseError> {
    let return_li = state.consume_token(Token::Return)?;
    if !state.is_in_fn {
        return Err(ParseError::UnexpectedReturn(return_li.span));
    }
    let return_expr = if !state.peek(&[Token::Semicolon]) {
        let expr = parse_expr(state)?;
        Some(Box::new(expr))
    } else {
        None
    };
    state.consume_token(Token::Semicolon)?;
    Ok(Expression::Return(return_expr))
}

fn parse_while(state: &mut Context) -> Result<Expression, ParseError> {
    state.consume_token(Token::While)?;
    let cond = parse_clause_node(state)?;
    let body = parse_block_node(state)?;
    Ok(Expression::While(WhileNode { cond, body }))
}

fn parse_if(state: &mut Context) -> Result<Expression, ParseError> {
    state.consume_token(Token::If)?;

    let cond = parse_clause_node(state)?;
    let if_stmts = parse_block_node(state)?;

    let mut else_if_nodes = vec![];
    let mut else_stmts = None;

    while state.peek(&[Token::Else]) {
        state.consume_token(Token::Else)?;
        if state.peek(&[Token::If]) {
            state.consume_token(Token::If)?;
            let cond = parse_clause_node(state)?;
            let if_stmts = parse_block_node(state)?;
            else_if_nodes.push(ElseIfNode {
                cond,
                stmts: if_stmts,
            });
        } else {
            else_stmts = Some(parse_block_node(state)?);
            break;
        };
    }
    let chains = IfChainNode {
        if_node: ElseIfNode {
            cond,
            stmts: if_stmts,
        },
        else_if_nodes,
        else_stmts,
    };

    Ok(Expression::IfChain(chains))
}

fn parse_when(state: &mut Context) -> Result<Expression, ParseError> {
    state.consume_token(Token::When)?;
    let case_nodes = parse_comma_list(
        state,
        Token::LPointParen,
        Token::RPointParen,
        parse_when_arm,
    )?;
    Ok(Expression::When(case_nodes))
}

fn parse_when_arm(state: &mut Context) -> Result<WhenArmNode, ParseError> {
    let cond = parse_clause_node(state)?;
    state.consume_token(Token::RTArrow)?;
    let expr = parse_clause_node(state)?;
    Ok(WhenArmNode { cond, expr })
}

fn parse_clause_node(state: &mut Context) -> Result<ClauseNode, ParseError> {
    parse_logical(state)
}

fn parse_logical(state: &mut Context) -> Result<ClauseNode, ParseError> {
    parse_recursive_binary(state, &[Token::And, Token::Or], parse_equality)
}

fn parse_equality(state: &mut Context) -> Result<ClauseNode, ParseError> {
    parse_recursive_binary(
        state,
        &[Token::EqualEqual, Token::BangEqual],
        parse_comparison,
    )
}

fn parse_comparison(state: &mut Context) -> Result<ClauseNode, ParseError> {
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

fn parse_term(state: &mut Context) -> Result<ClauseNode, ParseError> {
    parse_recursive_binary(state, &[Token::Plus, Token::Minus], parse_factor)
}

fn parse_factor(state: &mut Context) -> Result<ClauseNode, ParseError> {
    parse_recursive_binary(state, &[Token::Star, Token::Slash], parse_modulo)
}

fn parse_modulo(state: &mut Context) -> Result<ClauseNode, ParseError> {
    parse_recursive_binary(state, &[Token::Percentage], parse_unary)
}

fn parse_recursive_binary<F>(
    state: &mut Context,
    match_tokens: &'static [Token],
    lower_fn: F,
) -> Result<ClauseNode, ParseError>
where
    F: Fn(&mut Context) -> Result<ClauseNode, ParseError>,
{
    let mut lhs = lower_fn(state)?;

    while let Ok(op) = state.get_curr().cloned() {
        if !match_tokens.contains(&op.token) {
            break;
        }
        state.advance();
        let rhs = lower_fn(state)?;
        lhs = ClauseNode::BinaryOp(BinaryOpNode {
            lhs: Box::new(lhs),
            op: op.token,
            rhs: Box::new(rhs),
        });
    }

    Ok(lhs)
}

fn parse_unary(state: &mut Context) -> Result<ClauseNode, ParseError> {
    let li = state.get_curr()?;
    match li.token {
        Token::Not => parse_recursive_unary(state, Token::Not),
        Token::Minus => parse_recursive_unary(state, Token::Minus),
        _ => parse_chaining(state),
    }
}

fn parse_recursive_unary(
    state: &mut Context,
    match_token: Token,
) -> Result<ClauseNode, ParseError> {
    let li = state.get_curr()?;
    if li.token == match_token {
        state.advance();
        Ok(ClauseNode::UnaryOp(
            Box::new(parse_recursive_unary(state, match_token)?),
            match_token,
        ))
    } else {
        parse_chaining(state)
    }
}

fn parse_chaining(state: &mut Context) -> Result<ClauseNode, ParseError> {
    let base = if state.peek(&[Token::Identifier]) {
        let node = parse_identifier_node(state)?;
        ChainingBase::Identifier(node)
    } else if state.peek(&[Token::LRoundParen]) {
        let node = parse_group(state)?;
        ChainingBase::Group(Box::new(node))
    } else {
        let node = parse_primary_node(state)?;
        ChainingBase::Primary(node)
    };

    let mut follows = vec![];
    loop {
        if state.peek(&[Token::Identifier]) {
            let id = parse_identifier_node(state)?;
            follows.push(ChainingFollow::Identifier(id));
        } else if state.peek(&[Token::LSquareParen]) {
            state.consume_token(Token::LSquareParen)?;
            let indexee = parse_expr(state)?;
            state.consume_token(Token::RSquareParen)?;
            follows.push(ChainingFollow::Index(Box::new(indexee)));
        } else if state.peek(&[Token::LRoundParen]) {
            // parse function
            // TODO: make start position span the whole chain
            let start_position = state.get_current_position();
            let args = parse_comma_list(state, Token::LRoundParen, Token::RRoundParen, parse_expr)?;
            let end_position = state.get_current_position();
            follows.push(ChainingFollow::FnCall(FnCallNode {
                iden: IdentifierNode::new_from_name(
                    Span::new(start_position, end_position),
                    state.get_input(),
                ),
                args,
            }));
        } else {
            break;
        }
    }

    Ok(ClauseNode::Chaining(ChainingNode { base, follows }))
}

fn parse_primary_node(state: &mut Context) -> Result<PrimaryNode, ParseError> {
    let li = *state.get_curr()?;
    let mut next = |expr| {
        state.advance();
        Ok(expr)
    };

    match li.token {
        Token::Nil => next(PrimaryNode::Nil),
        Token::True => next(PrimaryNode::Bool(true)),
        Token::False => next(PrimaryNode::Bool(false)),
        Token::String => {
            // Don't use the `next` closure here, the borrow checker will complain
            state.advance();
            Ok(PrimaryNode::Str(
                // remove start '"' and end '"'
                Span::new(li.span.start + 1, li.span.end - 1),
            ))
        }
        Token::Number => parse_number(state),
        Token::LSquareParen => Ok(PrimaryNode::ArrayLiteral(parse_array_literal_node(state)?)),
        Token::PercentLPointParent => Ok(PrimaryNode::MapLiteral(parse_map_literal_node(state)?)),
        Token::Fn => parse_function_decl(state),
        _ => Err(ParseError::UnexpectedToken(li.token, li.span, None)),
    }
}

fn parse_identifier_node(state: &mut Context) -> Result<IdentifierNode, ParseError> {
    let lex_items = parse_repeated_with_separator(
        state,
        Token::Dot,
        |s| s.consume_token(Token::Identifier),
        |t| t != Token::Identifier,
    )?;
    Ok(IdentifierNode::new_from_vec(
        lex_items.into_iter().map(|li| li.span).collect(),
        state.get_input(),
    ))
}

fn parse_group(state: &mut Context) -> Result<Expression, ParseError> {
    state.consume_token(Token::LRoundParen)?;
    let expr = parse_expr(state)?;
    state.consume_token(Token::RRoundParen)?;
    Ok(expr)
}

fn parse_array_literal_node(state: &mut Context) -> Result<ArrayLiteralNode, ParseError> {
    if state.peek_2_token(&[Token::Colon]) {
        state.consume_token(Token::LSquareParen)?;
        state.consume_token(Token::Colon)?;
        let value = parse_clause_node(state)?;
        state.consume_token(Token::Colon)?;
        let repeat = parse_clause_node(state)?;
        state.consume_token(Token::RSquareParen)?;
        return Ok(ArrayLiteralNode::Repeat(Box::new(ArrayRepeatNode {
            repeat,
            value,
        })));
    }
    let exprs = parse_comma_list(
        state,
        Token::LSquareParen,
        Token::RSquareParen,
        parse_clause_node,
    )?;
    Ok(ArrayLiteralNode::List(exprs))
}

fn parse_map_literal_node(state: &mut Context) -> Result<MapLiteralNode, ParseError> {
    let nodes = parse_comma_list(
        state,
        Token::PercentLPointParent,
        Token::RPointParen,
        parse_map_literal_element_node,
    )?;
    Ok(MapLiteralNode { nodes })
}

fn parse_map_literal_element_node(
    state: &mut Context,
) -> Result<MapLiteralElementNode, ParseError> {
    let key = parse_primary_node(state)?;
    state.consume_token(Token::RFArrtow)?;
    let value = parse_expr(state)?;
    Ok(MapLiteralElementNode { key, value })
}

fn parse_function_decl(state: &mut Context) -> Result<PrimaryNode, ParseError> {
    state.consume_token(Token::Fn)?;
    let arg_names = parse_comma_list(
        state,
        Token::LRoundParen,
        Token::RRoundParen,
        get_identifier,
    )?
    .into_iter()
    .map(|li| IdentifierNode::new_from_name(li.span, state.get_input()))
    .collect();

    let body;

    if state.peek(&[Token::LPointParen]) {
        state.is_in_fn = true;
        body = parse_block_node(state)?;
        state.is_in_fn = false;
    } else {
        let expr = parse_expr(state)?;
        body = BlockNode {
            stmts: vec![],
            last_expr: Some(Box::new(expr)),
        };
    }

    Ok(PrimaryNode::FnDecl(FnDeclNode { arg_names, body }))
}

fn parse_number(state: &mut Context) -> Result<PrimaryNode, ParseError> {
    let li = state.consume_token(Token::Number)?;
    let source = li.span.str_from_source(state.get_input());

    if let Ok(num) = source.parse::<i64>() {
        return Ok(PrimaryNode::Integer(num));
    }

    match source.parse::<f64>() {
        Err(_) => Err(ParseError::ParseToNumber(li.span)),
        Ok(num) => Ok(PrimaryNode::Floating(num)),
    }
}

fn get_identifier(state: &mut Context) -> Result<LexItem, ParseError> {
    let li = state.consume_token(Token::Identifier)?;
    Ok(li.to_owned())
}

fn parse_repeated_with_separator<F, T>(
    state: &mut Context,
    separator: Token,
    lower_fn: F,
    break_check_fn: impl Fn(Token) -> bool,
) -> Result<Vec<T>, ParseError>
where
    F: Fn(&mut Context) -> Result<T, ParseError>,
{
    let mut result = Vec::new();
    let mut has_consumed_separator = true;

    while let Ok(li) = state.get_curr() {
        if break_check_fn(li.token) {
            break;
        }

        if !has_consumed_separator {
            return Err(ParseError::UnexpectedToken(
                li.token,
                li.span,
                Some(separator),
            ));
        }

        let expr = lower_fn(state)?;
        result.push(expr);
        has_consumed_separator = false;

        if let Ok(next) = state.get_curr() {
            if next.token == separator {
                state.advance();
                has_consumed_separator = true;
            }
        };
    }

    Ok(result)
}

fn parse_comma_list<F, T>(
    state: &mut Context,
    left_paren: Token,
    right_paren: Token,
    lower_fn: F,
) -> Result<Vec<T>, ParseError>
where
    F: Fn(&mut Context) -> Result<T, ParseError>,
{
    state.consume_token(left_paren)?;
    let result =
        parse_repeated_with_separator(state, Token::Comma, lower_fn, |t| t == right_paren)?;
    state.consume_token(right_paren)?;
    Ok(result)
}
