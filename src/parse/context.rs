use crate::token::Token;

use super::error::ParseError;
use super::lex::LexItem;

pub struct Context<'a> {
    input: &'a str,
    items: &'a [LexItem],
    curr_pos: usize,

    /// Tell the parser to copy the string content to the AST node
    /// or defer later, use full in the case of running in a REPL
    /// when the source might got already deleted when the actual string is needed
    should_eval_string: bool,

    pub fn_depth: usize,
}

impl<'a> Context<'a> {
    pub fn new(input: &'a str, items: &'a [LexItem], should_eval_string: bool) -> Self {
        Self {
            input,
            items,
            curr_pos: 0,
            should_eval_string,
            fn_depth: 0,
        }
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
