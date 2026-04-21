use crate::token::Token;

use super::error::ParseError;
use super::lex::LexItem;

/// Hard cap on structural recursion depth in the parser. Keeps pathological
/// input (e.g. thousands of nested parentheses) from blowing the native stack
/// before reaching the interpreter. 256 is generous for handwritten code while
/// well below the default thread stack size.
pub const MAX_RECURSION_DEPTH: usize = 256;

pub struct Context<'a> {
    input: &'a str,
    items: &'a [LexItem],
    curr_pos: usize,

    /// Tell the parser to copy the string content to the AST node
    /// or defer later, use full in the case of running in a REPL
    /// when the source might got already deleted when the actual string is needed
    should_eval_string: bool,

    pub fn_depth: usize,

    /// Current parser recursion depth. Incremented/decremented by
    /// `enter_recursion` / `leave_recursion` around recursive entry points.
    recursion_depth: usize,

    /// When false, `Identifier {` is NOT parsed as a struct literal.
    /// Set to false while parsing if/while/for conditions to avoid ambiguity
    /// with block expressions (mirrors Rust's grammar restriction).
    pub allow_struct_literal: bool,
}

impl<'a> Context<'a> {
    pub fn new(input: &'a str, items: &'a [LexItem], should_eval_string: bool) -> Self {
        Self {
            input,
            items,
            curr_pos: 0,
            should_eval_string,
            fn_depth: 0,
            recursion_depth: 0,
            allow_struct_literal: true,
        }
    }

    /// Bump the recursion counter; return an error if it exceeds the cap.
    /// Every call must be paired with `leave_recursion` (see `with_recursion`).
    pub fn enter_recursion(&mut self) -> Result<(), ParseError> {
        self.recursion_depth += 1;
        if self.recursion_depth > MAX_RECURSION_DEPTH {
            return Err(ParseError::RecursionLimitExceeded(MAX_RECURSION_DEPTH));
        }
        Ok(())
    }

    /// Pair with `enter_recursion` when a recursive call returns. Saturating so
    /// a stray extra call doesn't underflow on buggy code paths.
    pub fn leave_recursion(&mut self) {
        self.recursion_depth = self.recursion_depth.saturating_sub(1);
    }

    pub fn get_should_eval_string(&self) -> bool {
        self.should_eval_string
    }

    pub fn get_input(&self) -> &'a str {
        self.input
    }

    pub fn is_at_end(&self) -> bool {
        self.curr_pos >= self.items.len()
    }

    pub fn consume_token(&mut self, token: Token) -> Result<LexItem, ParseError> {
        if self.is_at_end() {
            return Err(ParseError::Eof(Some(token)));
        }
        let li = *self.get_curr()?;
        if li.token != token {
            return Err(ParseError::UnexpectedToken(li.token, li.span, Some(token)));
        }
        self.advance();
        Ok(li)
    }

    pub fn advance(&mut self) {
        self.curr_pos += 1;
        self.prepare_next();
    }

    pub fn peek(&mut self, match_tokens: &'static [Token]) -> bool {
        self.prepare_next();
        self.items
            .get(self.curr_pos)
            .map(|li| match_tokens.contains(&li.token))
            .unwrap_or(false)
    }

    pub fn peek_2_token(&mut self, match_tokens: &'static [Token]) -> bool {
        self.prepare_next();
        self.items
            .get(self.curr_pos + 1)
            .map(|li| match_tokens.contains(&li.token))
            .unwrap_or(false)
    }

    pub fn get_curr(&mut self) -> Result<&LexItem, ParseError> {
        match self.items.get(self.curr_pos) {
            Some(li) => Ok(li),
            None => Err(ParseError::Eof(None)),
        }
    }

    pub fn prepare_next(&mut self) {
        while self
            .items
            .get(self.curr_pos)
            .map(|li| li.token == Token::Comment)
            .unwrap_or(false)
        {
            self.curr_pos += 1;
        }
    }
}
