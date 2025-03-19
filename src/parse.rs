use crate::ast::{
    ArrayRepeatNode, BinaryOpNode, CaseNode, Expression, FnCallNode, FnDeclNode, IdentifierNode,
    IfStmtNode, IndexExprNode, ModuleNode, ReAssignIndexNode, Statement, StatementList,
    TernaryExprNode, WhileNode,
};
use crate::lex::{LexItem, Lexer};
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
    curr_pos: usize,
    is_in_fn: bool,
    // TODO: stub to reduce error message, remove this
    items: &'a [LexItem],
}

impl<'a> ParseContext<'a> {
    pub fn new(input: &'a str) -> Self {
        ParseContext {
            input,
            curr_pos: 0,
            is_in_fn: false,
            // TODO: remove this
            items: &[],
        }
    }
}

pub struct Parser<'a> {
    input: &'a str,
    lexer: Lexer<'a>,
    context: ParseContext<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, lexer: Lexer<'a>) -> Self {
        Parser {
            input,
            lexer,
            context: ParseContext::new(input),
        }
    }

    pub fn parse(&mut self) -> Result<Statement, ParseError> {
        self.parse_stmt()
    }

    fn parse_stmt(&mut self) -> Result<Statement, ParseError> {
        let li = self.get_curr()?;
        match li.token {
            Token::Module => self.parse_module(),
            Token::Print => self.parse_print(),
            Token::Identifier => self.parse_reassignment(),
            Token::Var => self.parse_declaration(),
            Token::While => self.parse_while(),
            Token::If => self.parse_if(),
            Token::Return => self.parse_return(),
            Token::LPointParen => self.parse_block(),
            _ => self.parse_expr().map(Statement::Expr),
        }
    }

    fn parse_module(&mut self) -> Result<Statement, ParseError> {
        self.consume_token(Token::Module)?;
        let li = self.consume_token(Token::Identifier)?;
        Ok(Statement::Module(ModuleNode {
            name: IdentifierNode::Simple(li.span.string_from_source(self.input)),
            body: self.parse_block_statement_list()?,
        }))
    }

    fn parse_print(&mut self) -> Result<Statement, ParseError> {
        self.consume_token(Token::Print)?;
        let exprs =
            self.parse_comma_list(Token::LRoundParen, Token::RRoundParen, Parser::parse_clause)?;
        Ok(Statement::Print(exprs))
    }

    fn parse_if(&mut self) -> Result<Statement, ParseError> {
        self.consume_token(Token::If)?;

        let cond = self.parse_clause()?;
        let if_stmts = self.parse_block_statement_list()?;
        let mut else_stmts = None;

        if self.peek(&[Token::Else]) {
            self.consume_token(Token::Else)?;
            else_stmts = Some(self.parse_block_statement_list()?);
        }

        Ok(Statement::If(IfStmtNode {
            cond,
            if_stmts,
            else_stmts,
        }))
    }

    fn parse_while(&mut self) -> Result<Statement, ParseError> {
        self.consume_token(Token::While)?;
        let cond = self.parse_clause()?;
        let body = self.parse_block_statement_list()?;
        Ok(Statement::While(WhileNode { cond, body }))
    }

    fn parse_return(&mut self) -> Result<Statement, ParseError> {
        self.consume_token(Token::Return)?;
        if !self.context.is_in_fn {
            return Err(ParseError::UnexpectedReturn(Span::one(
                self.context.curr_pos,
            )));
        }
        let expr = self.parse_expr()?;
        Ok(Statement::Return(expr))
    }

    fn parse_block(&mut self) -> Result<Statement, ParseError> {
        Ok(Statement::Block(self.parse_block_statement_list()?))
    }

    fn parse_block_statement_list(&mut self) -> Result<StatementList, ParseError> {
        self.consume_token(Token::LPointParen)?;
        let stmts =
            self.parse_repeated_with_separator(Token::Semicolon, Parser::parse_stmt, |t| {
                t == Token::RPointParen
            })?;
        self.consume_token(Token::RPointParen)?;
        Ok(stmts)
    }

    fn parse_declaration(&mut self) -> Result<Statement, ParseError> {
        self.consume_token(Token::Var)?;
        let id_item = self.consume_token(Token::Identifier)?;
        self.consume_token(Token::Equal)?;
        let expr = self.parse_expr()?;
        let name = id_item.span.string_from_source(self.input);
        Ok(Statement::Declare(IdentifierNode::Simple(name), expr))
    }

    fn parse_reassignment(&mut self) -> Result<Statement, ParseError> {
        if self.peek_2_token(&[Token::Equal]) {
            self.parse_iden_reassignment()
        } else {
            self.parse_index_reassignment_or_expr()
        }
    }

    fn parse_iden_reassignment(&mut self) -> Result<Statement, ParseError> {
        let id_item = self.consume_token(Token::Identifier)?;
        self.consume_token(Token::Equal)?;
        let expr = self.parse_expr()?;
        let name = id_item.span.string_from_source(self.input);
        Ok(Statement::ReassignIden(IdentifierNode::Simple(name), expr))
    }

    fn parse_index_reassignment_or_expr(&mut self) -> Result<Statement, ParseError> {
        let id = self.parse_expr()?;
        if self.peek(&[Token::Equal]) {
            let Expression::Index(index_expr) = id else {
                // TODO: more concrete error
                return Err(ParseError::Eof);
            };
            let expr = self.parse_expr()?;
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

    fn parse_expr(&mut self) -> Result<Expression, ParseError> {
        match self.get_curr()?.token {
            Token::Cond => self.parse_ternary(),
            Token::When => self.parse_when(),
            _ => self.parse_clause(),
        }
    }

    fn parse_ternary(&mut self) -> Result<Expression, ParseError> {
        self.consume_token(Token::Cond)?;
        let cond = self.parse_clause()?;
        self.consume_token(Token::Then)?;
        let true_expr = self.parse_clause()?;
        self.consume_token(Token::Else)?;
        let false_expr = self.parse_clause()?;
        Ok(Expression::Ternary(TernaryExprNode {
            cond: Box::new(cond),
            true_expr: Box::new(true_expr),
            false_expr: Box::new(false_expr),
        }))
    }

    fn parse_when(&mut self) -> Result<Expression, ParseError> {
        self.consume_token(Token::When)?;
        let case_nodes = self.parse_comma_list(
            Token::LPointParen,
            Token::RPointParen,
            Parser::parse_when_case,
        )?;
        Ok(Expression::When(case_nodes))
    }

    fn parse_when_case(&mut self) -> Result<CaseNode, ParseError> {
        self.consume_token(Token::Case)?;
        let cond = self.parse_clause()?;
        self.consume_token(Token::RTArrow)?;
        let expr = self.parse_clause()?;
        Ok(CaseNode { cond, expr })
    }

    fn parse_clause(&mut self) -> Result<Expression, ParseError> {
        self.parse_logical()
    }

    fn parse_logical(&mut self) -> Result<Expression, ParseError> {
        self.parse_recursive_binary(&[Token::And, Token::Or], Parser::parse_equality)
    }

    fn parse_equality(&mut self) -> Result<Expression, ParseError> {
        self.parse_recursive_binary(
            &[Token::EqualEqual, Token::BangEqual],
            Parser::parse_comparison,
        )
    }

    fn parse_comparison(&mut self) -> Result<Expression, ParseError> {
        self.parse_recursive_binary(
            &[
                Token::Less,
                Token::LessEqual,
                Token::Greater,
                Token::GreaterEqual,
            ],
            Parser::parse_term,
        )
    }

    fn parse_term(&mut self) -> Result<Expression, ParseError> {
        self.parse_recursive_binary(&[Token::Plus, Token::Minus], Parser::parse_factor)
    }

    fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        self.parse_recursive_binary(&[Token::Star, Token::Slash], Parser::parse_modulo)
    }

    fn parse_modulo(&mut self) -> Result<Expression, ParseError> {
        self.parse_recursive_binary(&[Token::Percentage], Parser::parse_unary)
    }

    fn parse_recursive_binary<F>(
        &mut self,
        match_tokens: &'static [Token],
        lower_fn: F,
    ) -> Result<Expression, ParseError>
    where
        F: Fn(&mut Parser<'a>) -> Result<Expression, ParseError>,
    {
        let mut lhs = lower_fn(self)?;

        while let Some(op) = self.context.items.get(self.context.curr_pos) {
            if !match_tokens.contains(&op.token) {
                break;
            }
            self.context.curr_pos += 1;
            let rhs = lower_fn(self)?;
            lhs = Expression::BinaryOp(BinaryOpNode {
                lhs: Box::new(lhs),
                op: op.token,
                rhs: Box::new(rhs),
            });
        }

        Ok(lhs)
    }

    fn parse_unary(&mut self) -> Result<Expression, ParseError> {
        let li = self.get_curr()?;
        match li.token {
            Token::Bang => self.parse_recursive_unary(Token::Bang),
            Token::Minus => self.parse_recursive_unary(Token::Minus),
            _ => self.parse_call(),
        }
    }

    fn parse_recursive_unary(&mut self, match_token: Token) -> Result<Expression, ParseError> {
        let li = self.get_curr()?;
        if li.token == match_token {
            self.context.curr_pos += 1;
            Ok(Expression::UnaryOp(
                Box::new(self.parse_recursive_unary(match_token)?),
                match_token,
            ))
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> Result<Expression, ParseError> {
        let lower = self.parse_index()?;
        if let Expression::Identifier(name) = &lower {
            if self.peek(&[Token::LRoundParen]) {
                let args = self.parse_comma_list(
                    Token::LRoundParen,
                    Token::RRoundParen,
                    Parser::parse_clause,
                )?;
                return Ok(Expression::FnCall(FnCallNode {
                    iden: name.clone(),
                    args,
                }));
            }
        }
        Ok(lower)
    }

    fn parse_index(&mut self) -> Result<Expression, ParseError> {
        let indexer = self.parse_primary()?;
        if !self.peek(&[Token::LSquareParen]) {
            return Ok(indexer);
        }

        self.consume_token(Token::LSquareParen)?;
        let indexee = self.parse_clause()?;
        self.consume_token(Token::RSquareParen)?;
        Ok(Expression::Index(IndexExprNode {
            indexer: Box::new(indexer),
            indexee: Box::new(indexee),
        }))
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        let li = self.get_curr()?;
        let mut next = |expr| {
            self.context.curr_pos += 1;
            Ok(expr)
        };

        match li.token {
            Token::Nil => next(Expression::Nil),
            Token::True => next(Expression::Bool(true)),
            Token::False => next(Expression::Bool(false)),
            Token::String => {
                next(Expression::Str(
                    // remove start '"' and end '"'
                    Span::new(li.span.start + 1, li.span.end - 1).string_from_source(self.input),
                ))
            }
            Token::Identifier => self.parse_identifier(),
            Token::Number => self.parse_number(),
            Token::LSquareParen => self.parse_array(),
            Token::LRoundParen => self.parse_group(),
            Token::Fn => self.parse_function(),
            _ => Err(ParseError::UnexpectedToken(li.token, li.span, None)),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParseError> {
        let lex_items = self.parse_repeated_with_separator(
            Token::Dot,
            |s| s.consume_token(Token::Identifier),
            |t| t != Token::Identifier,
        )?;
        Ok(Expression::Identifier(IdentifierNode::new(
            lex_items
                .into_iter()
                .map(|li| li.span.string_from_source(self.context.input))
                .collect(),
        )))
    }

    fn parse_group(&mut self) -> Result<Expression, ParseError> {
        self.consume_token(Token::LRoundParen)?;
        let expr = self.parse_expr()?;
        self.consume_token(Token::RRoundParen)?;
        Ok(expr)
    }

    fn parse_array(&mut self) -> Result<Expression, ParseError> {
        if self.peek_2_token(&[Token::Colon]) {
            self.consume_token(Token::LSquareParen)?;
            self.consume_token(Token::Colon)?;
            let value = self.parse_clause()?;
            self.consume_token(Token::Colon)?;
            let repeat = self.parse_clause()?;
            self.consume_token(Token::RSquareParen)?;
            return Ok(Expression::ArrayRepeat(Box::new(ArrayRepeatNode {
                repeat,
                value,
            })));
        }
        let exprs = self.parse_comma_list(
            Token::LSquareParen,
            Token::RSquareParen,
            Parser::parse_clause,
        )?;
        Ok(Expression::ArrayList(exprs))
    }

    fn parse_function(&mut self) -> Result<Expression, ParseError> {
        self.consume_token(Token::Fn)?;
        let arg_names = self
            .parse_comma_list(
                Token::LRoundParen,
                Token::RRoundParen,
                Parser::get_identifier,
            )?
            .into_iter()
            .map(|li| li.span.string_from_source(self.input))
            .collect();

        let body;

        if self.peek(&[Token::LPointParen]) {
            self.context.is_in_fn = true;
            body = self.parse_block_statement_list()?;
            self.context.is_in_fn = false;
        } else {
            let expr = self.parse_expr()?;
            body = vec![Statement::Return(expr)];
        }

        Ok(Expression::FnDecl(FnDeclNode { arg_names, body }))
    }

    fn parse_number(&mut self) -> Result<Expression, ParseError> {
        let li = self.consume_token(Token::Number)?;
        let source = li.span.str_from_source(self.input);
        match source.parse::<f64>() {
            Err(_) => Err(ParseError::ParseToNumber(li.span)),
            Ok(num) => Ok(Expression::Number(num)),
        }
    }

    fn get_identifier(&mut self) -> Result<LexItem, ParseError> {
        let li = self.consume_token(Token::Identifier)?;
        Ok(li.to_owned())
    }

    fn parse_repeated_with_separator<F, T>(
        &mut self,
        separator: Token,
        mut lower_fn: F,
        break_check_fn: impl Fn(Token) -> bool,
    ) -> Result<Vec<T>, ParseError>
    where
        F: FnMut(&mut Parser<'a>) -> Result<T, ParseError>,
    {
        let mut result = Vec::new();
        let mut has_consumed_separator = true;

        while let Some(li) = self.context.items.get(self.context.curr_pos) {
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

            let expr = lower_fn(self)?;
            result.push(expr);
            has_consumed_separator = false;

            if let Some(next) = self.context.items.get(self.context.curr_pos) {
                if next.token == separator {
                    self.context.curr_pos += 1;
                    has_consumed_separator = true;
                }
            };
        }

        Ok(result)
    }

    fn parse_comma_list<F, T>(
        &mut self,
        left_paren: Token,
        right_paren: Token,
        lower_fn: F,
    ) -> Result<Vec<T>, ParseError>
    where
        F: Fn(&mut Parser<'a>) -> Result<T, ParseError>,
    {
        self.consume_token(left_paren)?;
        let result =
            self.parse_repeated_with_separator(Token::Comma, lower_fn, |t| t == right_paren)?;
        self.consume_token(right_paren)?;
        Ok(result)
    }

    fn consume_token(&mut self, token: Token) -> Result<LexItem, ParseError> {
        let li = self.get_curr()?;
        if li.token != token {
            return Err(ParseError::UnexpectedToken(li.token, li.span, Some(token)));
        }
        self.context.curr_pos += 1;
        Ok(li)
    }

    fn peek(&mut self, match_tokens: &'static [Token]) -> bool {
        self.lexer.peek_one(match_tokens)
    }

    fn peek_2_token(&mut self, match_tokens: &'static [Token]) -> bool {
        self.lexer.peek_two(match_tokens)
    }

    fn get_curr(&mut self) -> Result<LexItem, ParseError> {
        // TODO: maybe translate error
        self.lexer.next_token()
    }
}
