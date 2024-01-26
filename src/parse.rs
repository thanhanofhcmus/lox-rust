use crate::ast::{
    ArrayRepeatNode, BinaryOpNode, CaseNode, Expression, FnCallNode, FnDeclNode, IdentifierNode,
    IfStmtNode, IndexExprNode, ModuleNode, ReAssignIndexNode, Statement, StatementList,
    TernaryExprNode, WhileNode,
};
use crate::lex::LexItem;
use crate::parse_error::ParseError;
use crate::span::Span;
use crate::token::Token;

/*
stmt         = reassignment | declaration | print | expr | if | while | return | module
module       = "module" block
print        = "print" (clause "," ...)*
if           = "if" clause block ("else" block)?
declaration  = "var" IDENTIFIER "=" expr
while        = "while" clause block
return       = "return" (expr)?
block        = "{" (stmt ";")* "}"
reassignment = lvalue "=" expr
lvalue       = IDENTIFIER | index
expr         = clause | ternary | when
ternary      = "cond" clause "then" clause "else" clause
when         = "when" "{" ( "case" clause "->" expr "," ... )* "}"
clause       = logical
logical      = equality (( "and" | "or" ) equality)*
equality     = comparison (("==" | "!=") comparison)*
comparison   = term (("<" | "<" | "<=" | ">=") term)*
term         = factor (("+" | "-") factor)*
factor       = modulo (("*" | "/") modulo)*
modulo       = unary ("%" unary)?
unary        = ("!" | "-")* unary | call
call         = index "(" (clause "," ...)* ")"
index        = primary ("[" clause "]")?
primary      = STRING | NUMBER | identifier | atom | group | array | function
atom         = "true" | "false" | "nil"
identifier   = (IDENTIFIER "." ...)*
function     = "fn" "(" ( IDENTIFIER "," ... )* ")" (block | expr)
array        = "[" (clause, "," ... )* | (clause ":" clause) "]"
group        = "(" clause ")"
*/

struct ParseContext<'a> {
    input: &'a str,
    items: &'a [LexItem],
    curr_pos: usize,
    is_in_fn: bool,
}

impl<'a> ParseContext<'a> {
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
    let mut stmts = vec![];
    let mut state = ParseContext::new(input, items);
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

fn parse_stmt(state: &mut ParseContext) -> Result<Statement, ParseError> {
    let li = get_curr(state)?;
    match li.token {
        Token::Module => parse_module(state),
        Token::Print => parse_print(state),
        Token::Identifier => parse_reassignment(state),
        Token::Var => parse_declaration(state),
        Token::While => parse_while(state),
        Token::If => parse_if(state),
        Token::Return => parse_return(state),
        Token::LPointParen => parse_block(state),
        _ => parse_expr(state).map(Statement::Expr),
    }
}

fn parse_module(state: &mut ParseContext) -> Result<Statement, ParseError> {
    consume_token(state, Token::Module)?;
    let li = consume_token(state, Token::Identifier)?;
    Ok(Statement::Module(ModuleNode {
        name: IdentifierNode::Simple(li.span.string_from_source(state.input)),
        body: parse_block_statement_list(state)?,
    }))
}

fn parse_print(state: &mut ParseContext) -> Result<Statement, ParseError> {
    consume_token(state, Token::Print)?;
    let exprs = parse_comma_list(state, Token::LRoundParen, Token::RRoundParen, parse_clause)?;
    Ok(Statement::Print(exprs))
}

fn parse_if(state: &mut ParseContext) -> Result<Statement, ParseError> {
    consume_token(state, Token::If)?;

    let cond = parse_clause(state)?;
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
fn parse_while(state: &mut ParseContext) -> Result<Statement, ParseError> {
    consume_token(state, Token::While)?;
    let cond = parse_clause(state)?;
    let body = parse_block_statement_list(state)?;
    Ok(Statement::While(WhileNode { cond, body }))
}

fn parse_return(state: &mut ParseContext) -> Result<Statement, ParseError> {
    consume_token(state, Token::Return)?;
    if !state.is_in_fn {
        return Err(ParseError::UnexpectedReturn(Span::one(state.curr_pos)));
    }
    let expr = parse_expr(state)?;
    Ok(Statement::Return(expr))
}

fn parse_block(state: &mut ParseContext) -> Result<Statement, ParseError> {
    Ok(Statement::Block(parse_block_statement_list(state)?))
}

fn parse_block_statement_list(state: &mut ParseContext) -> Result<StatementList, ParseError> {
    consume_token(state, Token::LPointParen)?;
    let stmts = parse_repeated_with_separator(state, Token::Semicolon, parse_stmt, |t| {
        t == Token::RPointParen
    })?;
    consume_token(state, Token::RPointParen)?;
    Ok(stmts)
}

fn parse_declaration(state: &mut ParseContext) -> Result<Statement, ParseError> {
    consume_token(state, Token::Var)?;
    let id_item = consume_token(state, Token::Identifier)?;
    consume_token(state, Token::Equal)?;
    let expr = parse_expr(state)?;
    let name = id_item.span.string_from_source(state.input);
    Ok(Statement::Declare(IdentifierNode::Simple(name), expr))
}

fn parse_reassignment(state: &mut ParseContext) -> Result<Statement, ParseError> {
    if peek_2_token(state, &[Token::Equal]) {
        parse_iden_reassignment(state)
    } else {
        parse_index_reassignment_or_expr(state)
    }
}

fn parse_iden_reassignment(state: &mut ParseContext) -> Result<Statement, ParseError> {
    let id_item = consume_token(state, Token::Identifier)?;
    consume_token(state, Token::Equal)?;
    let expr = parse_expr(state)?;
    let name = id_item.span.string_from_source(state.input);
    Ok(Statement::ReassignIden(IdentifierNode::Simple(name), expr))
}

fn parse_index_reassignment_or_expr(state: &mut ParseContext) -> Result<Statement, ParseError> {
    let id = parse_expr(state)?;
    if peek(state, &[Token::Equal]) {
        let Expression::Index(index_expr) = id else {
            // TODO: more concrete error
            return Err(ParseError::Eof);
        };
        let expr = parse_expr(state)?;
        Ok(Statement::ReassignIndex({
            ReAssignIndexNode {
                indexer: *index_expr.indexer,
                indexee: *index_expr.indexee,
                expr,
            }
        }))
    } else {
        Ok(Statement::Expr(id))
    }
}

fn parse_expr(state: &mut ParseContext) -> Result<Expression, ParseError> {
    match get_curr(state)?.token {
        Token::Cond => parse_ternary(state),
        Token::When => parse_when(state),
        _ => parse_clause(state),
    }
}

fn parse_ternary(state: &mut ParseContext) -> Result<Expression, ParseError> {
    consume_token(state, Token::Cond)?;
    let cond = parse_clause(state)?;
    consume_token(state, Token::Then)?;
    let true_expr = parse_clause(state)?;
    consume_token(state, Token::Else)?;
    let false_expr = parse_clause(state)?;
    Ok(Expression::Ternary(TernaryExprNode {
        cond: Box::new(cond),
        true_expr: Box::new(true_expr),
        false_expr: Box::new(false_expr),
    }))
}

fn parse_when(state: &mut ParseContext) -> Result<Expression, ParseError> {
    consume_token(state, Token::When)?;
    let case_nodes = parse_comma_list(
        state,
        Token::LPointParen,
        Token::RPointParen,
        parse_when_case,
    )?;
    Ok(Expression::When(case_nodes))
}

fn parse_when_case(state: &mut ParseContext) -> Result<CaseNode, ParseError> {
    consume_token(state, Token::Case)?;
    let cond = parse_clause(state)?;
    consume_token(state, Token::RTArrow)?;
    let expr = parse_clause(state)?;
    Ok(CaseNode { cond, expr })
}

fn parse_clause(state: &mut ParseContext) -> Result<Expression, ParseError> {
    parse_logical(state)
}

fn parse_logical(state: &mut ParseContext) -> Result<Expression, ParseError> {
    parse_recursive_binary(state, &[Token::And, Token::Or], parse_equality)
}

fn parse_equality(state: &mut ParseContext) -> Result<Expression, ParseError> {
    parse_recursive_binary(
        state,
        &[Token::EqualEqual, Token::BangEqual],
        parse_comparison,
    )
}

fn parse_comparison(state: &mut ParseContext) -> Result<Expression, ParseError> {
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

fn parse_term(state: &mut ParseContext) -> Result<Expression, ParseError> {
    parse_recursive_binary(state, &[Token::Plus, Token::Minus], parse_factor)
}

fn parse_factor(state: &mut ParseContext) -> Result<Expression, ParseError> {
    parse_recursive_binary(state, &[Token::Star, Token::Slash], parse_modulo)
}

fn parse_modulo(state: &mut ParseContext) -> Result<Expression, ParseError> {
    parse_recursive_binary(state, &[Token::Percentage], parse_unary)
}

fn parse_recursive_binary<F>(
    state: &mut ParseContext,
    match_tokens: &'static [Token],
    lower_fn: F,
) -> Result<Expression, ParseError>
where
    F: Fn(&mut ParseContext) -> Result<Expression, ParseError>,
{
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

fn parse_unary(state: &mut ParseContext) -> Result<Expression, ParseError> {
    let li = get_curr(state)?;
    match li.token {
        Token::Bang => parse_recursive_unary(state, Token::Bang),
        Token::Minus => parse_recursive_unary(state, Token::Minus),
        _ => parse_call(state),
    }
}

fn parse_recursive_unary(
    state: &mut ParseContext,
    match_token: Token,
) -> Result<Expression, ParseError> {
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

fn parse_call(state: &mut ParseContext) -> Result<Expression, ParseError> {
    let lower = parse_index(state)?;
    if let Expression::Identifier(name) = &lower {
        if peek(state, &[Token::LRoundParen]) {
            let args =
                parse_comma_list(state, Token::LRoundParen, Token::RRoundParen, parse_clause)?;
            return Ok(Expression::FnCall(FnCallNode {
                iden: name.clone(),
                args,
            }));
        }
    }
    Ok(lower)
}

fn parse_index(state: &mut ParseContext) -> Result<Expression, ParseError> {
    let indexer = parse_primary(state)?;
    if !peek(state, &[Token::LSquareParen]) {
        return Ok(indexer);
    }

    consume_token(state, Token::LSquareParen)?;
    let indexee = parse_clause(state)?;
    consume_token(state, Token::RSquareParen)?;
    Ok(Expression::Index(IndexExprNode {
        indexer: Box::new(indexer),
        indexee: Box::new(indexee),
    }))
}

fn parse_primary(state: &mut ParseContext) -> Result<Expression, ParseError> {
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
                Span::new(li.span.start + 1, li.span.end - 1).string_from_source(state.input),
            ))
        }
        Token::Identifier => parse_identifier(state),
        Token::Number => parse_number(state),
        Token::LSquareParen => parse_array(state),
        Token::LRoundParen => parse_group(state),
        Token::Fn => parse_function(state),
        _ => Err(ParseError::UnexpectedToken(li.token, li.span, None)),
    }
}

fn parse_identifier(state: &mut ParseContext) -> Result<Expression, ParseError> {
    let lex_items = parse_repeated_with_separator(
        state,
        Token::Dot,
        |s| consume_token(s, Token::Identifier),
        |t| t != Token::Identifier,
    )?;
    Ok(Expression::Identifier(IdentifierNode::new(
        lex_items
            .into_iter()
            .map(|li| li.span.string_from_source(state.input))
            .collect(),
    )))
}

fn parse_group(state: &mut ParseContext) -> Result<Expression, ParseError> {
    consume_token(state, Token::LRoundParen)?;
    let expr = parse_expr(state)?;
    consume_token(state, Token::RRoundParen)?;
    Ok(expr)
}

fn parse_array(state: &mut ParseContext) -> Result<Expression, ParseError> {
    if peek_2_token(state, &[Token::Colon]) {
        consume_token(state, Token::LSquareParen)?;
        consume_token(state, Token::Colon)?;
        let value = parse_clause(state)?;
        consume_token(state, Token::Colon)?;
        let repeat = parse_clause(state)?;
        consume_token(state, Token::RSquareParen)?;
        return Ok(Expression::ArrayRepeat(Box::new(ArrayRepeatNode {
            repeat,
            value,
        })));
    }
    let exprs = parse_comma_list(
        state,
        Token::LSquareParen,
        Token::RSquareParen,
        parse_clause,
    )?;
    Ok(Expression::ArrayList(exprs))
}

fn parse_function(state: &mut ParseContext) -> Result<Expression, ParseError> {
    consume_token(state, Token::Fn)?;
    let arg_names = parse_comma_list(
        state,
        Token::LRoundParen,
        Token::RRoundParen,
        get_identifier,
    )?
    .into_iter()
    .map(|li| li.span.string_from_source(state.input))
    .collect();

    let body;

    if peek(state, &[Token::LPointParen]) {
        state.is_in_fn = true;
        body = parse_block_statement_list(state)?;
        state.is_in_fn = false;
    } else {
        let expr = parse_expr(state)?;
        body = vec![Statement::Return(expr)];
    }

    Ok(Expression::FnDecl(FnDeclNode { arg_names, body }))
}

fn parse_number(state: &mut ParseContext) -> Result<Expression, ParseError> {
    let li = consume_token(state, Token::Number)?;
    let source = li.span.str_from_source(state.input);
    match source.parse::<f64>() {
        Err(_) => Err(ParseError::ParseToNumber(li.span)),
        Ok(num) => Ok(Expression::Number(num)),
    }
}

fn get_identifier(state: &mut ParseContext) -> Result<LexItem, ParseError> {
    let li = consume_token(state, Token::Identifier)?;
    Ok(li.to_owned())
}

fn parse_repeated_with_separator<F, T>(
    state: &mut ParseContext,
    separator: Token,
    lower_fn: F,
    break_check_fn: impl Fn(Token) -> bool,
) -> Result<Vec<T>, ParseError>
where
    F: Fn(&mut ParseContext) -> Result<T, ParseError>,
{
    let mut result = Vec::new();
    let mut has_consumed_separator = true;

    while let Some(li) = state.items.get(state.curr_pos) {
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

        if let Some(next) = state.items.get(state.curr_pos) {
            if next.token == separator {
                state.curr_pos += 1;
                has_consumed_separator = true;
            }
        };
    }

    Ok(result)
}

fn parse_comma_list<F, T>(
    state: &mut ParseContext,
    left_paren: Token,
    right_paren: Token,
    lower_fn: F,
) -> Result<Vec<T>, ParseError>
where
    F: Fn(&mut ParseContext) -> Result<T, ParseError>,
{
    consume_token(state, left_paren)?;
    let result =
        parse_repeated_with_separator(state, Token::Comma, lower_fn, |t| t == right_paren)?;
    consume_token(state, right_paren)?;
    Ok(result)
}

fn consume_token(state: &mut ParseContext, token: Token) -> Result<LexItem, ParseError> {
    let li = *get_curr(state)?;
    if li.token != token {
        return Err(ParseError::UnexpectedToken(li.token, li.span, Some(token)));
    }
    state.curr_pos += 1;
    Ok(li)
}

fn peek(state: &ParseContext, match_tokens: &'static [Token]) -> bool {
    state
        .items
        .get(state.curr_pos)
        .map(|li| match_tokens.contains(&li.token))
        .unwrap_or(false)
}

fn peek_2_token(state: &ParseContext, match_tokens: &'static [Token]) -> bool {
    state
        .items
        .get(state.curr_pos + 1)
        .map(|li| match_tokens.contains(&li.token))
        .unwrap_or(false)
}

fn get_curr<'a>(state: &'a ParseContext) -> Result<&'a LexItem, ParseError> {
    match state.items.get(state.curr_pos) {
        Some(li) => Ok(li),
        None => Err(ParseError::Eof),
    }
}
