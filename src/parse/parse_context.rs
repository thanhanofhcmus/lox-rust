use crate::span::Span;
use crate::token::Token;

use super::lex::LexItem;
use super::parse_error::ParseError;

pub struct ParseContext<'a> {
    input: &'a str,
    items: &'a [LexItem],
    curr_pos: usize,
    pub is_in_fn: bool,
}

impl<'a> ParseContext<'a> {
    pub fn new(input: &'a str, items: &'a [LexItem]) -> Self {
        Self {
            input,
            items,
            curr_pos: 0,
            is_in_fn: false,
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.curr_pos >= self.items.len()
    }

    pub fn consume_token(&mut self, token: Token) -> Result<LexItem, ParseError> {
        let li = *self.get_curr()?;
        if li.token != token {
            return Err(ParseError::UnexpectedToken(li.token, li.span, Some(token)));
        }
        self.curr_pos += 1;
        Ok(li)
    }

    pub fn advance(&mut self) {
        self.curr_pos += 1;
    }

    pub fn peek(&mut self, match_tokens: &'static [Token]) -> bool {
        self.items
            .get(self.curr_pos)
            .map(|li| match_tokens.contains(&li.token))
            .unwrap_or(false)
    }

    pub fn peek_2_token(&mut self, match_tokens: &'static [Token]) -> bool {
        self.items
            .get(self.curr_pos + 1)
            .map(|li| match_tokens.contains(&li.token))
            .unwrap_or(false)
    }

    pub fn get_curr(&mut self) -> Result<&LexItem, ParseError> {
        match self.items.get(self.curr_pos) {
            Some(li) => Ok(li),
            None => Err(ParseError::Eof),
        }
    }

    pub fn string_from_span(&self, span: Span) -> String {
        span.string_from_source(self.input)
    }
}
