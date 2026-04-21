use crate::types::TypeId;
use crate::{ast::*, string_utils};

use super::context::Context;
use super::error::ParseError;
use super::lex::LexItem;

use crate::span::Span;
use crate::token::Token;

pub fn parse(
    input: &str,
    items: &[LexItem],
    should_eval_string: bool,
) -> Result<AST<()>, ParseError> {
    let mut state = Context::new(input, items, should_eval_string);
    let mut imports = vec![];
    let mut global_stmts = vec![];
    let mut is_parsing_import = true;
    while !state.is_at_end() {
        // parse import then parse stmts
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

        state.prepare_next();
        let stmt = parse_stmt(&mut state)?;
        if let Statement::Expr(expr) = &stmt {
            match expr.case {
                ExprCase::Return(_) | ExprCase::Clause(_) => {
                    state.consume_token(Token::Semicolon)?;
                }
                _ => {}
            }
        }
        global_stmts.push(stmt);
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

fn parse_stmt(state: &mut Context) -> Result<Statement<()>, ParseError> {
    let li = state.get_curr()?;
    match li.token {
        Token::Var => parse_declaration(state),
        Token::Struct => parse_struct_decl(state).map(Statement::StructDecl),
        Token::Identifier => parse_reassignment_or_expr(state),
        _ => parse_expr_stmt(state),
    }
}

fn parse_type_node(state: &mut Context) -> Result<TypeNode, ParseError> {
    let type_item = *state.get_curr()?;
    state.advance();
    // for a simple case we we can work out the type right a way
    let type_id = match type_item.token {
        Token::TypeAny => TypeId::ANY,
        Token::TypeBool => TypeId::BOOL,
        Token::TypeNumber => TypeId::NUMBER,
        Token::TypeStr => TypeId::STR,
        // Token::Identifier => Typed::Name(...)
        _ => {
            return Err(ParseError::UnexpectedToken(
                type_item.token,
                type_item.span,
                None,
            ));
        }
    };
    Ok(TypeNode::BuiltIn(type_id))
}

fn parse_struct_field_node(state: &mut Context) -> Result<StructFieldNode, ParseError> {
    let li = get_identifier(state)?;
    let explicit_type = if state.peek(&[Token::Colon]) {
        state.consume_token(Token::Colon)?;
        Some(parse_type_node(state)?)
    } else {
        None
    };
    Ok(StructFieldNode {
        iden: IdentifierNode::new_from_name(li.span, state.get_input()),
        explicit_type,
    })
}

fn parse_struct_decl(state: &mut Context) -> Result<StructDeclNode, ParseError> {
    state.consume_token(Token::Struct)?;
    let li = get_identifier(state)?;
    let fields = parse_comma_list(
        state,
        Token::LPointParen,
        Token::RPointParen,
        parse_struct_field_node,
    )?;
    Ok(StructDeclNode {
        iden: IdentifierNode::new_from_name(li.span, state.get_input()),
        fields,
    })
}

fn parse_block_node(state: &mut Context) -> Result<BlockNode<()>, ParseError> {
    state.consume_token(Token::LPointParen)?;

    let mut stmts = vec![];
    let mut last_expr = None;
    while !state.peek(&[Token::RPointParen]) {
        let result = parse_stmt(state)?;

        // we are facing the end of this block,
        if state.peek(&[Token::RPointParen]) {
            // last parsed statement might be an expression with no semicolon
            if let Statement::Expr(expr) = result {
                // the last parsed statement is an expression, make it a return expr
                last_expr = Some(Box::new(expr));
                break;
            } else {
                // the last parsed statement is just a normal statement
                stmts.push(result);
            }
        } else {
            // not the last statement, make sure to consume semicolon from non-block expression
            if let Statement::Expr(expr) = &result {
                match expr.case {
                    ExprCase::Return(_) | ExprCase::Clause(_) => {
                        state.consume_token(Token::Semicolon)?;
                    }
                    _ => {}
                }
            }
            stmts.push(result);
        }
    }

    state.consume_token(Token::RPointParen)?;
    Ok(BlockNode::new(stmts, last_expr))
}

fn parse_expr_stmt(state: &mut Context) -> Result<Statement<()>, ParseError> {
    let expr = parse_expr(state)?;
    Ok(Statement::Expr(expr))
}

fn parse_declaration(state: &mut Context) -> Result<Statement<()>, ParseError> {
    state.consume_token(Token::Var)?;
    let id_item = state.consume_token(Token::Identifier)?;
    let explicit_type = if state.peek(&[Token::Colon]) {
        state.consume_token(Token::Colon)?;
        Some(parse_type_node(state)?)
    } else {
        None
    };
    state.consume_token(Token::Equal)?;
    let expr = parse_expr(state)?;
    state.consume_token(Token::Semicolon)?;
    Ok(Statement::Declare(DeclareStatementNode {
        iden: IdentifierNode::new_from_name(id_item.span, state.get_input()),
        explicit_type,
        expr,
    }))
}

fn parse_reassignment_or_expr(state: &mut Context) -> Result<Statement<()>, ParseError> {
    let expr = parse_expr(state)?;

    if !state.peek(&[Token::Equal]) {
        return Ok(Statement::Expr(expr));
    }

    state.consume_token(Token::Equal)?;

    let node = convert_chaining(expr)?;

    let expr = parse_expr(state)?;
    state.consume_token(Token::Semicolon)?;
    Ok(Statement::ReassignIden(node, expr))
}

fn parse_expr(state: &mut Context) -> Result<Expression<()>, ParseError> {
    match state.get_curr()?.token {
        Token::Return => parse_return(state),
        Token::When => parse_when(state),
        Token::While => parse_while(state),
        Token::If => parse_if(state),
        Token::For => parse_for(state),
        Token::LPointParen => parse_block_node(state).map(|v| Expression::new(ExprCase::Block(v))),
        _ => parse_clause_node(state).map(|v| Expression::new(ExprCase::Clause(v))),
    }
}

fn parse_return(state: &mut Context) -> Result<Expression<()>, ParseError> {
    let li = state.consume_token(Token::Return)?;
    if state.fn_depth == 0 {
        return Err(ParseError::UnexpectedReturn(li.span));
    }
    let return_expr = if !state.peek(&[Token::Semicolon]) {
        let expr = parse_expr(state)?;
        Some(Box::new(expr))
    } else {
        None
    };
    state.consume_token(Token::Semicolon)?;
    Ok(Expression::new(ExprCase::Return(return_expr)))
}

fn parse_while(state: &mut Context) -> Result<Expression<()>, ParseError> {
    state.consume_token(Token::While)?;
    state.allow_struct_literal = false;
    let cond = parse_clause_node(state)?;
    state.allow_struct_literal = true;
    let body = parse_block_node(state)?;
    Ok(Expression::new(ExprCase::While(WhileNode { cond, body })))
}

fn parse_for(state: &mut Context) -> Result<Expression<()>, ParseError> {
    state.consume_token(Token::For)?;
    let iden_li = state.consume_token(Token::Identifier)?;
    state.consume_token(Token::In)?;
    state.allow_struct_literal = false;
    let collection = parse_clause_node(state)?;
    state.allow_struct_literal = true;
    let body = parse_block_node(state)?;
    Ok(Expression::new(ExprCase::For(ForNode {
        iden: IdentifierNode::new_from_name(iden_li.span, state.get_input()),
        collection,
        body,
    })))
}

fn parse_if(state: &mut Context) -> Result<Expression<()>, ParseError> {
    state.consume_token(Token::If)?;

    state.allow_struct_literal = false;
    let cond = parse_clause_node(state)?;
    state.allow_struct_literal = true;
    let if_stmts = parse_block_node(state)?;

    let mut else_if_nodes = vec![];
    let mut else_stmts = None;

    while state.peek(&[Token::Else]) {
        state.consume_token(Token::Else)?;
        if state.peek(&[Token::If]) {
            state.consume_token(Token::If)?;
            state.allow_struct_literal = false;
            let cond = parse_clause_node(state)?;
            state.allow_struct_literal = true;
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

    Ok(Expression::new(ExprCase::IfChain(IfChainNode {
        if_node: ElseIfNode {
            cond,
            stmts: if_stmts,
        },
        else_if_nodes,
        else_stmts,
    })))
}

fn parse_when(state: &mut Context) -> Result<Expression<()>, ParseError> {
    state.consume_token(Token::When)?;
    let arms = parse_comma_list(
        state,
        Token::LPointParen,
        Token::RPointParen,
        parse_when_arm,
    )?;
    Ok(Expression::new(ExprCase::When(WhenNode { arms })))
}

fn parse_when_arm(state: &mut Context) -> Result<WhenArmNode<()>, ParseError> {
    let cond = parse_clause_node(state)?;
    state.consume_token(Token::RThinArrow)?;
    let expr = parse_clause_node(state)?;
    Ok(WhenArmNode { cond, expr })
}

fn parse_clause_node(state: &mut Context) -> Result<ClauseNode<()>, ParseError> {
    parse_pratt(state, BindingPower::None)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum BindingPower {
    None,
    LogicalLeft,
    LogicalRight,
    EqualityLeft,
    EqualityRight,
    ComparisionLeft,
    ComparisionRight,
    TermLeft,
    TermRight,
    FactorLeft,
    FactorRight,
    // all three chain node (member access, subscription and function all have right association)
    ChainRight,
    ChainLeft,
}

fn get_binding_power(token: Token) -> Option<(BindingPower, BindingPower)> {
    use BindingPower::*;
    use Token::*;
    let bp = match token {
        And | Or => (LogicalLeft, LogicalRight),
        EqualEqual | BangEqual => (EqualityLeft, EqualityRight),
        Less | LessEqual | Greater | GreaterEqual => (ComparisionLeft, ComparisionRight),
        Plus | Minus => (TermLeft, TermRight),
        Star | Slash | Percentage => (FactorLeft, FactorRight),

        // postfix operators, we will only extract the left part
        LSquareParen | LRoundParen | Dot => (ChainLeft, ChainRight),
        _ => return Option::None,
    };
    Some(bp)
}

fn parse_pratt(state: &mut Context, min_bp: BindingPower) -> Result<ClauseNode<()>, ParseError> {
    let mut lhs = parse_pratt_prefix(state)?;

    loop {
        if state.is_at_end() {
            break;
        }
        let li = state.get_curr().copied()?;
        let Some((left_bp, right_bp)) = get_binding_power(li.token) else {
            break;
        };
        if left_bp < min_bp {
            break;
        }
        lhs = parse_pratt_infix(state, lhs, right_bp)?;
    }

    Ok(lhs)
}

fn parse_pratt_infix(
    state: &mut Context,
    lhs: ClauseNode<()>,
    min_bp: BindingPower,
) -> Result<ClauseNode<()>, ParseError> {
    let li = state.get_curr().copied()?;
    match li.token {
        // TODO: support member/module access via dot notation; requires a dot-access AST node
        Token::Dot => unimplemented!(),

        Token::LSquareParen => {
            state.consume_token(li.token)?;
            let indexee = parse_clause_node(state)?;
            state.consume_token(Token::RSquareParen)?;
            Ok(ClauseNode::new(ClauseCase::Subscription(
                SubscriptionNode {
                    indexer: Box::new(lhs),
                    indexee: Box::new(indexee),
                },
            )))
        }
        Token::LRoundParen => {
            let args = parse_comma_list(state, Token::LRoundParen, Token::RRoundParen, parse_expr)?;
            Ok(ClauseNode::new(ClauseCase::FnCall(FnCallNode {
                caller: Box::new(lhs),
                args,
            })))
        }
        token if is_binary_op(token) => {
            state.consume_token(li.token)?;
            let rhs = parse_pratt(state, min_bp)?;
            Ok(ClauseNode::new(ClauseCase::Binary(BinaryOpNode {
                lhs: Box::new(lhs),
                op: li.token,
                rhs: Box::new(rhs),
            })))
        }
        _ => Err(ParseError::UnexpectedToken(li.token, li.span, None)),
    }
}

fn parse_pratt_prefix(state: &mut Context) -> Result<ClauseNode<()>, ParseError> {
    parse_unary(state)
}

fn is_binary_op(token: Token) -> bool {
    use Token::*;
    matches!(
        token,
        And | Or
            | EqualEqual
            | BangEqual
            | Less
            | LessEqual
            | Greater
            | GreaterEqual
            | Plus
            | Minus
            | Star
            | Slash
            | Percentage
    )
}

fn parse_unary(state: &mut Context) -> Result<ClauseNode<()>, ParseError> {
    let li = state.get_curr()?;
    match li.token {
        Token::Not => parse_recursive_unary(state, Token::Not),
        Token::Minus => parse_recursive_unary(state, Token::Minus),
        _ => parse_primary(state),
    }
}

fn parse_recursive_unary(
    state: &mut Context,
    match_token: Token,
) -> Result<ClauseNode<()>, ParseError> {
    let li = state.get_curr()?;
    if li.token == match_token {
        state.advance();
        let expr = parse_recursive_unary(state, match_token)?;
        Ok(ClauseNode::new(ClauseCase::Unary(
            Box::new(expr),
            match_token,
        )))
    } else {
        parse_primary(state)
    }
}

fn parse_primary(state: &mut Context) -> Result<ClauseNode<()>, ParseError> {
    let base = if state.peek(&[Token::Identifier]) {
        // Struct literal: `Name { field = value }`.
        // Only attempted when the context allows it (disabled in if/while/for conditions
        // to avoid ambiguity with block expressions — same restriction as Rust).
        let is_struct_literal =
            state.allow_struct_literal && state.peek_2_token(&[Token::LPointParen]);
        if is_struct_literal {
            let node = parse_struct_literal_node(state)?;
            ClauseCase::RawValue(RawValueNode::StructLiteral(node))
        } else {
            let node = parse_identifier_node(state)?;
            ClauseCase::Identifier(node)
        }
    } else if state.peek(&[Token::LRoundParen]) {
        let node = parse_group(state)?;
        ClauseCase::Group(Box::new(node))
    } else {
        let node = parse_raw_value_node(state)?;
        ClauseCase::RawValue(node)
    };
    Ok(ClauseNode::new(base))
}

fn parse_raw_value_node(state: &mut Context) -> Result<RawValueNode<()>, ParseError> {
    let li = *state.get_curr()?;
    match li.token {
        Token::Nil
        | Token::True
        | Token::False
        | Token::Number
        | Token::String
        | Token::RawString => parse_scalar_node(state).map(RawValueNode::Scalar),
        Token::LSquareParen => parse_array_literal_node(state).map(RawValueNode::ArrayLiteral),
        Token::PercentLPointParent => parse_map_literal_node(state).map(RawValueNode::MapLiteral),
        Token::Fn => parse_function_decl(state).map(RawValueNode::FnDecl),
        _ => Err(ParseError::UnexpectedToken(li.token, li.span, None)),
    }
}

fn parse_scalar_node(state: &mut Context) -> Result<ScalarNode, ParseError> {
    let li = *state.get_curr()?;
    let mut next = |expr| {
        state.advance();
        Ok(expr)
    };

    match li.token {
        Token::Nil => next(ScalarNode::Nil),
        Token::True => next(ScalarNode::Bool(true)),
        Token::False => next(ScalarNode::Bool(false)),
        Token::Number => parse_number(state),
        Token::String | Token::RawString => parse_string(state),
        _ => Err(ParseError::UnexpectedToken(li.token, li.span, None)),
    }
}

fn parse_struct_field_literal_node(
    state: &mut Context,
) -> Result<StructLiteralFieldNode<()>, ParseError> {
    let li = get_identifier(state)?;
    state.consume_token(Token::Equal)?;
    let value = parse_clause_node(state)?;
    Ok(StructLiteralFieldNode {
        iden: IdentifierNode::new_from_name(li.span, state.get_input()),
        value,
    })
}

fn parse_struct_literal_node(state: &mut Context) -> Result<StructLiteralNode<()>, ParseError> {
    let li = get_identifier(state)?;
    let fields = parse_comma_list(
        state,
        Token::LPointParen,
        Token::RPointParen,
        parse_struct_field_literal_node,
    )?;
    Ok(StructLiteralNode {
        iden: IdentifierNode::new_from_name(li.span, state.get_input()),
        fields,
    })
}

fn parse_identifier_node(state: &mut Context) -> Result<IdentifierNode, ParseError> {
    let li_item = state.consume_token(Token::Identifier)?;
    Ok(IdentifierNode::new_from_name(
        li_item.span,
        state.get_input(),
    ))
}

fn parse_group(state: &mut Context) -> Result<ClauseNode<()>, ParseError> {
    state.consume_token(Token::LRoundParen)?;
    let node = parse_clause_node(state)?;
    state.consume_token(Token::RRoundParen)?;
    Ok(node)
}

fn parse_array_literal_node(state: &mut Context) -> Result<ArrayLiteralNode<()>, ParseError> {
    // [:1 : 2 ]
    if state.peek_2_token(&[Token::Colon]) {
        state.consume_token(Token::LSquareParen)?;
        state.consume_token(Token::Colon)?;
        let value = parse_clause_node(state)?;
        state.consume_token(Token::Colon)?;
        let repeat = parse_clause_node(state)?;
        state.consume_token(Token::RSquareParen)?;
        Ok(ArrayLiteralNode::Repeat(Box::new(ArrayRepeatNode {
            repeat,
            value,
        })))
    // [for a in b: c]
    } else if state.peek_2_token(&[Token::For]) {
        state.consume_token(Token::LSquareParen)?;
        state.consume_token(Token::For)?;
        let iden_li = state.consume_token(Token::Identifier)?;

        state.consume_token(Token::In)?;
        let collection = parse_clause_node(state)?;

        let filter = if state.peek(&[Token::If]) {
            state.consume_token(Token::If)?;
            let clause = parse_clause_node(state)?;
            Some(clause)
        } else {
            None
        };

        state.consume_token(Token::Colon)?;

        let transformer = parse_clause_node(state)?;
        state.consume_token(Token::RSquareParen)?;

        Ok(ArrayLiteralNode::ForComprehension(Box::new(
            ArrayForComprehensionNode {
                iden: IdentifierNode::new_from_name(iden_li.span, state.get_input()),
                collection,
                transformer,
                filter,
            },
        )))
    } else {
        let exprs = parse_comma_list(
            state,
            Token::LSquareParen,
            Token::RSquareParen,
            parse_clause_node,
        )?;
        Ok(ArrayLiteralNode::List(exprs))
    }
}

fn parse_map_literal_node(state: &mut Context) -> Result<MapLiteralNode<()>, ParseError> {
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
) -> Result<MapLiteralElementNode<()>, ParseError> {
    let key = parse_scalar_node(state)?;
    state.consume_token(Token::RFatArrow)?;
    let value = parse_clause_node(state)?;
    Ok(MapLiteralElementNode { key, value })
}

fn parse_function_decl(state: &mut Context) -> Result<FnDeclNode<()>, ParseError> {
    state.consume_token(Token::Fn)?;
    let params = parse_comma_list(
        state,
        Token::LRoundParen,
        Token::RRoundParen,
        parse_fn_param,
    )?;

    let return_type = if state.peek(&[Token::RThinArrow]) {
        // TODO: consider if we have type annotation, do not allow  single expr form
        state.consume_token(Token::RThinArrow)?;
        Some(parse_type_node(state)?)
    } else {
        None
    };

    let body;

    if state.peek(&[Token::LPointParen]) {
        state.fn_depth += 1;
        body = parse_block_node(state)?;
        state.fn_depth -= 1;
    } else {
        let expr = parse_expr(state)?;
        body = BlockNode::new(vec![], Some(Box::new(expr)));
    }

    Ok(FnDeclNode {
        params,
        body,
        return_type,
    })
}

fn parse_fn_param(state: &mut Context) -> Result<FnParamNode, ParseError> {
    let li = get_identifier(state)?;
    let explicit_type = if state.peek(&[Token::Colon]) {
        state.consume_token(Token::Colon)?;
        Some(parse_type_node(state)?)
    } else {
        None
    };
    Ok(FnParamNode {
        id: IdentifierNode::new_from_name(li.span, state.get_input()),
        explicit_type,
    })
}

fn parse_number(state: &mut Context) -> Result<ScalarNode, ParseError> {
    let li = state.consume_token(Token::Number)?;
    let source = li.span.str_from_source(state.get_input());

    if let Ok(num) = source.parse::<i64>() {
        return Ok(ScalarNode::Integer(num));
    }

    match source.parse::<f64>() {
        Err(_) => Err(ParseError::ParseToNumber(li.span)),
        Ok(num) => Ok(ScalarNode::Floating(num)),
    }
}

fn parse_string(state: &mut Context) -> Result<ScalarNode, ParseError> {
    let li = *state.get_curr()?;

    let is_raw = li.token == Token::RawString;
    // remove start 'r' (if has) and '"' and end '"'
    let span_start = li.span.start + if is_raw { 2 } else { 1 };
    let span_end = li.span.end - 1;

    // Don't use the `next` closure here, the borrow checker will complain
    state.advance();
    let span = Span::new(span_start, span_end);
    if state.get_should_eval_string() {
        let string = if is_raw {
            span.string_from_source(state.get_input())
        } else {
            string_utils::unescape(span.str_from_source(state.get_input()))
        };
        Ok(ScalarNode::LiteralStr(string))
    } else {
        Ok(ScalarNode::LazyStr { span, is_raw })
    }
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

        if let Ok(next) = state.get_curr()
            && next.token == separator
        {
            state.advance();
            has_consumed_separator = true;
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

fn get_identifier(state: &mut Context) -> Result<LexItem, ParseError> {
    let li = state.consume_token(Token::Identifier)?;
    Ok(li)
}

fn convert_chaining(
    Expression { extra: _, case }: Expression<()>,
) -> Result<ChainingReassignTargetNode<()>, ParseError> {
    let ExprCase::Clause(clause) = case else {
        return Err(ParseError::ReassignRootIsNotAnIdentifier);
    };

    let mut index_chain = vec![];
    let mut clause = clause;

    loop {
        match clause.case {
            ClauseCase::Subscription(SubscriptionNode { indexer, indexee }) => {
                index_chain.push(*indexee);
                clause = *indexer;
            }
            v => {
                index_chain.push(ClauseNode { case: v, extra: () });
                break;
            }
        }
    }

    let first = index_chain
        .pop()
        .ok_or(ParseError::ReassignRootIsNotAnIdentifier)?;

    index_chain.reverse();

    let base = match first.case {
        ClauseCase::Identifier(node) => node,
        _ => return Err(ParseError::ReassignRootIsNotAnIdentifier),
    };

    Ok(ChainingReassignTargetNode {
        base,
        follows: index_chain,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        parse::{ParseError, lex},
        types::TypeId,
    };
    use pretty_assertions::assert_eq;

    fn parse_str(input: &str) -> AST<()> {
        let items = lex(input).expect("lex failed");
        parse(input, &items, false).expect("parse failed")
    }

    fn parse_err(input: &str) -> ParseError {
        let items = lex(input).expect("lex failed");
        parse(input, &items, false).expect_err("expected parse error")
    }

    /// Extract the first top-level expression statement's inner clause.
    /// Most tests use this to look at the shape of a single expression.
    fn first_clause(ast: &AST<()>) -> &ClauseCase<()> {
        match &ast.global_stmts[0] {
            Statement::Expr(Expression {
                case: ExprCase::Clause(c),
                ..
            }) => &c.case,
            other => panic!("expected first stmt to be an expression clause, got {other:?}"),
        }
    }

    fn first_stmt(ast: &AST<()>) -> &Statement<()> {
        &ast.global_stmts[0]
    }

    // ---------- literals ----------

    #[test]
    fn parse_nil_literal() {
        let ast = parse_str("nil;");
        assert!(matches!(
            first_clause(&ast),
            ClauseCase::RawValue(RawValueNode::Scalar(ScalarNode::Nil))
        ));
    }

    #[test]
    fn parse_bool_literals() {
        let t = parse_str("true;");
        assert!(matches!(
            first_clause(&t),
            ClauseCase::RawValue(RawValueNode::Scalar(ScalarNode::Bool(true)))
        ));
        let f = parse_str("false;");
        assert!(matches!(
            first_clause(&f),
            ClauseCase::RawValue(RawValueNode::Scalar(ScalarNode::Bool(false)))
        ));
    }

    #[test]
    fn parse_integer_literal() {
        let ast = parse_str("42;");
        assert!(matches!(
            first_clause(&ast),
            ClauseCase::RawValue(RawValueNode::Scalar(ScalarNode::Integer(42)))
        ));
    }

    #[test]
    fn parse_float_literal() {
        let ast = parse_str("9.8765;");
        match first_clause(&ast) {
            ClauseCase::RawValue(RawValueNode::Scalar(ScalarNode::Floating(v))) => {
                assert!((v - 9.8765).abs() < 1e-10);
            }
            other => panic!("expected Floating, got {other:?}"),
        }
    }

    #[test]
    fn parse_string_literal() {
        let ast = parse_str("\"hello\";");
        assert!(matches!(
            first_clause(&ast),
            ClauseCase::RawValue(RawValueNode::Scalar(ScalarNode::LazyStr { .. }))
        ));
    }

    #[test]
    fn parse_empty_array_literal() {
        let ast = parse_str("[];");
        match first_clause(&ast) {
            ClauseCase::RawValue(RawValueNode::ArrayLiteral(ArrayLiteralNode::List(items))) => {
                assert_eq!(items.len(), 0);
            }
            other => panic!("expected empty array List, got {other:?}"),
        }
    }

    #[test]
    fn parse_array_literal_with_items() {
        let ast = parse_str("[1, 2, 3];");
        match first_clause(&ast) {
            ClauseCase::RawValue(RawValueNode::ArrayLiteral(ArrayLiteralNode::List(items))) => {
                assert_eq!(items.len(), 3);
            }
            other => panic!("expected array List, got {other:?}"),
        }
    }

    #[test]
    fn parse_empty_map_literal() {
        let ast = parse_str("%{};");
        match first_clause(&ast) {
            ClauseCase::RawValue(RawValueNode::MapLiteral(m)) => {
                assert_eq!(m.nodes.len(), 0);
            }
            other => panic!("expected MapLiteral, got {other:?}"),
        }
    }

    #[test]
    fn parse_map_literal_with_entries() {
        let ast = parse_str("%{\"a\" => 1, \"b\" => 2};");
        match first_clause(&ast) {
            ClauseCase::RawValue(RawValueNode::MapLiteral(m)) => {
                assert_eq!(m.nodes.len(), 2);
            }
            other => panic!("expected MapLiteral, got {other:?}"),
        }
    }

    // ---------- binary operators & precedence ----------

    #[test]
    fn parse_binary_plus() {
        let ast = parse_str("1 + 2;");
        assert!(matches!(first_clause(&ast), ClauseCase::Binary(_)));
    }

    #[test]
    fn parse_multiplication_binds_tighter_than_addition() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        let ast = parse_str("1 + 2 * 3;");
        match first_clause(&ast) {
            ClauseCase::Binary(bin) => {
                assert_eq!(bin.op, Token::Plus);
                // rhs should itself be Binary(Star)
                match &bin.rhs.case {
                    ClauseCase::Binary(inner) => assert_eq!(inner.op, Token::Star),
                    other => panic!("expected Binary(Star) rhs, got {other:?}"),
                }
            }
            other => panic!("expected Binary at top, got {other:?}"),
        }
    }

    #[test]
    fn parse_comparison_operator() {
        let ast = parse_str("a < b;");
        match first_clause(&ast) {
            ClauseCase::Binary(bin) => assert_eq!(bin.op, Token::Less),
            other => panic!("expected Binary, got {other:?}"),
        }
    }

    #[test]
    fn parse_logical_and() {
        let ast = parse_str("a and b;");
        match first_clause(&ast) {
            ClauseCase::Binary(bin) => assert_eq!(bin.op, Token::And),
            other => panic!("expected Binary, got {other:?}"),
        }
    }

    // ---------- unary operators ----------

    #[test]
    fn parse_unary_minus() {
        let ast = parse_str("-x;");
        assert!(matches!(
            first_clause(&ast),
            ClauseCase::Unary(_, Token::Minus)
        ));
    }

    #[test]
    fn parse_unary_not() {
        let ast = parse_str("not true;");
        assert!(matches!(
            first_clause(&ast),
            ClauseCase::Unary(_, Token::Not)
        ));
    }

    #[test]
    fn parse_nested_unary() {
        let ast = parse_str("- -5;");
        match first_clause(&ast) {
            ClauseCase::Unary(inner, Token::Minus) => {
                assert!(matches!(inner.case, ClauseCase::Unary(_, Token::Minus)));
            }
            other => panic!("expected nested Unary, got {other:?}"),
        }
    }

    // ---------- declarations ----------

    #[test]
    fn parse_declaration_no_annotation() {
        let ast = parse_str("var x = 5;");
        match first_stmt(&ast) {
            Statement::Declare(DeclareStatementNode {
                explicit_type: type_,
                ..
            }) => {
                assert_eq!(*type_, None);
            }
            other => panic!("expected Declare, got {other:?}"),
        }
    }

    #[test]
    fn parse_declaration_with_number_annotation() {
        let ast = parse_str("var x: number = 5;");
        match first_stmt(&ast) {
            Statement::Declare(DeclareStatementNode {
                explicit_type: type_,
                ..
            }) => {
                assert_eq!(*type_, Some(TypeNode::BuiltIn(TypeId::NUMBER)));
            }
            other => panic!("expected Declare, got {other:?}"),
        }
    }

    #[test]
    fn parse_declaration_with_any_annotation() {
        let ast = parse_str("var x: any = 5;");
        match first_stmt(&ast) {
            Statement::Declare(DeclareStatementNode {
                explicit_type: type_,
                ..
            }) => {
                assert_eq!(*type_, Some(TypeNode::BuiltIn(TypeId::ANY)));
            }
            other => panic!("expected Declare, got {other:?}"),
        }
    }

    #[test]
    fn parse_declaration_with_bool_annotation() {
        let ast = parse_str("var x: bool = true;");
        match first_stmt(&ast) {
            Statement::Declare(DeclareStatementNode {
                explicit_type: type_,
                ..
            }) => {
                assert_eq!(*type_, Some(TypeNode::BuiltIn(TypeId::BOOL)));
            }
            other => panic!("expected Declare, got {other:?}"),
        }
    }

    // ---------- blocks ----------

    #[test]
    fn parse_block_with_last_expr() {
        // Block-as-expression is a statement-level expression that takes no trailing `;`.
        let ast = parse_str("{ 5 }");
        match first_stmt(&ast) {
            Statement::Expr(Expression {
                case: ExprCase::Block(block),
                ..
            }) => {
                assert_eq!(block.stmts.len(), 0);
                assert!(block.last_expr.is_some());
            }
            other => panic!("expected Expr(Block), got {other:?}"),
        }
    }

    #[test]
    fn parse_block_stmts_vs_last_expr() {
        let ast = parse_str("{ var x = 1; x }");
        match first_stmt(&ast) {
            Statement::Expr(Expression {
                case: ExprCase::Block(block),
                ..
            }) => {
                assert_eq!(block.stmts.len(), 1);
                assert!(block.last_expr.is_some());
            }
            other => panic!("expected Expr(Block), got {other:?}"),
        }
    }

    // ---------- control flow ----------
    // Note: block-based stmts (if/while/for/when) don't take a trailing `;` at the
    // top level. Only Clause and Return expression statements do.

    #[test]
    fn parse_if_only() {
        let ast = parse_str("if a { 1 }");
        match first_stmt(&ast) {
            Statement::Expr(Expression {
                case: ExprCase::IfChain(chain),
                ..
            }) => {
                assert_eq!(chain.else_if_nodes.len(), 0);
                assert!(chain.else_stmts.is_none());
            }
            other => panic!("expected IfChain, got {other:?}"),
        }
    }

    #[test]
    fn parse_if_else_if_else() {
        let ast = parse_str("if a { 1 } else if b { 2 } else { 3 }");
        match first_stmt(&ast) {
            Statement::Expr(Expression {
                case: ExprCase::IfChain(chain),
                ..
            }) => {
                assert_eq!(chain.else_if_nodes.len(), 1);
                assert!(chain.else_stmts.is_some());
            }
            other => panic!("expected IfChain, got {other:?}"),
        }
    }

    #[test]
    fn parse_while() {
        let ast = parse_str("while a { b }");
        assert!(matches!(
            first_stmt(&ast),
            Statement::Expr(Expression {
                case: ExprCase::While(_),
                ..
            })
        ));
    }

    // ---------- reassignment ----------

    #[test]
    fn parse_reassign_simple() {
        let ast = parse_str("x = 5;");
        match first_stmt(&ast) {
            Statement::ReassignIden(target, _) => {
                assert_eq!(target.follows.len(), 0);
            }
            other => panic!("expected ReassignIden, got {other:?}"),
        }
    }

    // ---------- function declarations ----------

    #[test]
    fn parse_fn_decl_with_block_body() {
        let ast = parse_str("fn(x, y) { x };");
        match first_clause(&ast) {
            ClauseCase::RawValue(RawValueNode::FnDecl(fn_decl)) => {
                assert_eq!(fn_decl.params.len(), 2);
            }
            other => panic!("expected FnDecl, got {other:?}"),
        }
    }

    #[test]
    fn parse_fn_decl_with_expression_body() {
        // Expression-body syntax: `fn(params) expr` — no `=` sign.
        let ast = parse_str("fn(x) x + 1;");
        match first_clause(&ast) {
            ClauseCase::RawValue(RawValueNode::FnDecl(fn_decl)) => {
                assert_eq!(fn_decl.params.len(), 1);
                assert_eq!(fn_decl.body.stmts.len(), 0);
                assert!(fn_decl.body.last_expr.is_some());
            }
            other => panic!("expected FnDecl, got {other:?}"),
        }
    }

    // ---------- function call & subscription ----------

    #[test]
    fn parse_function_call() {
        let ast = parse_str("f(1, 2);");
        match first_clause(&ast) {
            ClauseCase::FnCall(call) => {
                assert_eq!(call.args.len(), 2);
            }
            other => panic!("expected FnCall, got {other:?}"),
        }
    }

    #[test]
    fn parse_subscription() {
        let ast = parse_str("arr[0];");
        assert!(matches!(first_clause(&ast), ClauseCase::Subscription(_)));
    }

    // ---------- errors ----------

    #[test]
    fn error_on_unfinished_input() {
        let err = parse_err("var x = ");
        // Could be Eof or UnexpectedToken depending on where the parser gets stuck;
        // we just assert that parsing fails.
        let _ = err;
    }

    #[test]
    fn error_on_import_not_at_top() {
        let err = parse_err("var x = 5; import \"foo\" as foo;");
        assert!(
            matches!(err, ParseError::ImportNotAtTheTop(_)),
            "expected ImportNotAtTheTop, got {err:?}"
        );
    }

    // ---------- imports (happy path) ----------

    #[test]
    fn parse_import_at_top() {
        let ast = parse_str("import \"foo\" as foo;");
        assert_eq!(ast.imports.len(), 1);
        assert_eq!(ast.global_stmts.len(), 0);
    }

    // ---------- struct declarations ----------

    #[test]
    fn parse_struct_decl_no_fields() {
        let ast = parse_str("struct Foo {}");
        match first_stmt(&ast) {
            Statement::StructDecl(node) => assert_eq!(node.fields.len(), 0),
            other => panic!("expected StructDecl, got {other:?}"),
        }
    }

    #[test]
    fn parse_struct_decl_with_typed_fields() {
        let ast = parse_str("struct Point { x: number, y: number }");
        match first_stmt(&ast) {
            Statement::StructDecl(node) => assert_eq!(node.fields.len(), 2),
            other => panic!("expected StructDecl, got {other:?}"),
        }
    }

    #[test]
    fn parse_struct_decl_untyped_field() {
        let ast = parse_str("struct Bag { item }");
        match first_stmt(&ast) {
            Statement::StructDecl(node) => assert_eq!(node.fields.len(), 1),
            other => panic!("expected StructDecl, got {other:?}"),
        }
    }

    // ---------- struct literals ----------

    #[test]
    fn parse_struct_literal_with_fields() {
        let ast = parse_str("Point { x = 1, y = 2 };");
        match first_clause(&ast) {
            ClauseCase::RawValue(RawValueNode::StructLiteral(node)) => {
                assert_eq!(node.fields.len(), 2);
            }
            other => panic!("expected StructLiteral, got {other:?}"),
        }
    }

    #[test]
    fn parse_empty_struct_literal() {
        let ast = parse_str("Foo {};");
        match first_clause(&ast) {
            ClauseCase::RawValue(RawValueNode::StructLiteral(node)) => {
                assert_eq!(node.fields.len(), 0);
            }
            other => panic!("expected StructLiteral, got {other:?}"),
        }
    }

    // ---------- struct literal disambiguation ----------

    #[test]
    fn if_condition_identifier_not_parsed_as_struct() {
        // `p` in `if p { ... }` must be parsed as an identifier, not a struct literal.
        let ast = parse_str("if p { 1 }");
        match first_stmt(&ast) {
            Statement::Expr(Expression {
                case: ExprCase::IfChain(chain),
                ..
            }) => match &chain.if_node.cond.case {
                ClauseCase::Identifier(_) => {}
                other => panic!("expected Identifier in if cond, got {other:?}"),
            },
            other => panic!("expected IfChain, got {other:?}"),
        }
    }

    #[test]
    fn while_condition_identifier_not_parsed_as_struct() {
        let ast = parse_str("while p { 1 }");
        match first_stmt(&ast) {
            Statement::Expr(Expression {
                case: ExprCase::While(w),
                ..
            }) => match &w.cond.case {
                ClauseCase::Identifier(_) => {}
                other => panic!("expected Identifier in while cond, got {other:?}"),
            },
            other => panic!("expected While, got {other:?}"),
        }
    }

    #[test]
    fn for_collection_identifier_not_parsed_as_struct() {
        let ast = parse_str("for x in items { 1 }");
        match first_stmt(&ast) {
            Statement::Expr(Expression {
                case: ExprCase::For(f),
                ..
            }) => match &f.collection.case {
                ClauseCase::Identifier(_) => {}
                other => panic!("expected Identifier in for collection, got {other:?}"),
            },
            other => panic!("expected For, got {other:?}"),
        }
    }

    // ---------- return / fn_depth ----------

    #[test]
    fn return_outside_function_errors() {
        let err = parse_err("return 5;");
        assert!(
            matches!(err, ParseError::UnexpectedReturn(_)),
            "expected UnexpectedReturn, got {err:?}"
        );
    }

    #[test]
    fn return_inside_function_ok() {
        parse_str("var f = fn() { return 1; };");
    }

    #[test]
    fn return_inside_nested_function_ok() {
        // Return in an inner function must succeed even though the outer scope is a function.
        parse_str("var f = fn() { var g = fn() { return 1; }; };");
    }

    #[test]
    fn return_after_exiting_function_errors() {
        // After the function closes, fn_depth drops back to 0 — `return` is illegal again.
        let err = parse_err("var f = fn() { 1 }; return 5;");
        assert!(
            matches!(err, ParseError::UnexpectedReturn(_)),
            "expected UnexpectedReturn, got {err:?}"
        );
    }
}
