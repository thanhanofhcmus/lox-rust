use crate::parse_error::ParseError;

use crate::span::Span;
use crate::token::Token;

static KEYWORDS: phf::Map<&'static str, Token> = phf::phf_map!(
    "and" => Token::And,
    "or" => Token::Or,

    "true" => Token::True,
    "false" => Token::False,
    "nil" => Token::Nil,

    "var" => Token::Var,
    "if" => Token::If,
    "else" => Token::Else,
    "while" => Token::While,
    "fn" => Token::Fn,
    "return" => Token::Return,
    "cond" => Token::Cond,
    "then" => Token::Then,
    "when" => Token::When,
    "case" => Token::Case,
    "module" => Token::Module,

    "print" => Token::Print,
);

#[derive(Debug, Clone, Copy)]
pub struct LexItem {
    pub span: Span,
    pub token: Token,
}

impl LexItem {
    pub fn new(token: Token, span: Span) -> Self {
        LexItem { token, span }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    current_offset: usize,
    next_one: Option<LexItem>,
    next_two: Option<LexItem>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            current_offset: 0,
            next_one: None,
            next_two: None,
        }
    }

    pub fn get_source(&self) -> &'a str {
        self.input
    }

    pub fn peek_one(&self, match_tokens: &'static [Token]) -> bool {
        self.next_one
            .map(|t| match_tokens.contains(&t.token))
            .unwrap_or(false)
    }

    pub fn peek_two(&self, match_tokens: &'static [Token]) -> bool {
        self.next_two
            .map(|t| match_tokens.contains(&t.token))
            .unwrap_or(false)
    }

    pub fn prepare_peek_one(&mut self) -> Result<(), ParseError> {
        if self.next_one.is_some() {
            return Ok(());
        }

        let tok = self.lex_next()?;
        self.next_one = Some(tok);
        Ok(())
    }

    pub fn preare_peek_two(&mut self) -> Result<(), ParseError> {
        if self.next_two.is_some() {
            return Ok(());
        }

        self.prepare_peek_one()?;

        let tok = self.lex_next()?;
        self.next_two = Some(tok);
        Ok(())
    }

    pub fn next_token(&mut self) -> Result<LexItem, ParseError> {
        if let Some(tok) = self.next_one {
            std::mem::swap(&mut self.next_one, &mut self.next_two);
            return Ok(tok);
        }
        self.lex_next()
    }

    fn lex_next(&mut self) -> Result<LexItem, ParseError> {
        self.skip_whitespaces();

        let Some(c) = self.input.chars().nth(self.current_offset) else {
            return Err(ParseError::Eof);
        };
        let const_offset = self.current_offset; // we don't want to borrow mutate of curr_offset
        let tok_one = |t| LexItem::new(t, Span::one(const_offset));

        let mut make_with_equal = |token_has_equal, token_no_equal| {
            if is_next_char(self.input, self.current_offset, '=') {
                self.current_offset += 1;
                LexItem::new(token_has_equal, Span::two(self.current_offset - 1))
            } else {
                LexItem::new(token_no_equal, Span::one(self.current_offset))
            }
        };

        let tok = match c {
            '(' => tok_one(Token::LRoundParen),
            ')' => tok_one(Token::RRoundParen),
            '[' => tok_one(Token::LSquareParen),
            ']' => tok_one(Token::RSquareParen),
            '{' => tok_one(Token::LPointParen),
            '}' => tok_one(Token::RPointParen),
            '.' => tok_one(Token::Dot),
            ',' => tok_one(Token::Comma),
            ':' => tok_one(Token::Colon),
            ';' => tok_one(Token::Semicolon),
            '+' => tok_one(Token::Plus),
            '*' => tok_one(Token::Star),
            '/' => tok_one(Token::Slash),
            '%' => tok_one(Token::Percentage),

            '-' => {
                if is_next_char(self.input, self.current_offset, '>') {
                    self.current_offset += 1;
                    LexItem::new(Token::RTArrow, Span::two(self.current_offset - 1))
                } else {
                    tok_one(Token::Minus)
                }
            }

            '=' => make_with_equal(Token::EqualEqual, Token::Equal),
            '!' => make_with_equal(Token::BangEqual, Token::Bang),
            '<' => make_with_equal(Token::LessEqual, Token::Less),
            '>' => make_with_equal(Token::GreaterEqual, Token::Greater),
            '"' => self.lex_string()?,

            v if v.is_ascii_digit() => self.lex_number()?,
            v if is_keyword_or_identifier_char(v) => self.lex_keyword_or_identifier(),

            _ => {
                return Err(ParseError::UnexpectedCharacter(Span::one(
                    self.current_offset,
                )));
            }
        };
        self.current_offset += 1;

        Ok(tok)
    }

    fn skip_whitespaces(&mut self) {
        while let Some(c) = self.input.chars().nth(self.current_offset) {
            match c {
                ' ' | '\t' | '\r' | '\n' => self.current_offset += 1,
                _ => return,
            }
        }
    }

    fn lex_number(&mut self) -> Result<LexItem, ParseError> {
        let start_offset = self.current_offset;
        let mut has_lexed_dot = false;

        while let Some(c) = self.input.chars().nth(self.current_offset + 1) {
            if !(c.is_ascii_digit() || c == '.') {
                break;
            }
            if c == '.' {
                if has_lexed_dot {
                    return Err(ParseError::UnexpectedCharacter(Span::one(
                        self.current_offset,
                    )));
                }
                has_lexed_dot = true
            }
            self.current_offset += 1;
        }

        Ok(LexItem::new(
            Token::Number,
            Span::new(start_offset, self.current_offset),
        ))
    }

    fn lex_string(&mut self) -> Result<LexItem, ParseError> {
        let start_offset = self.current_offset;
        self.current_offset += 1; // consume '"'
        while let Some(c) = self.input.chars().nth(self.current_offset) {
            if c == '"' {
                break;
            }
            self.current_offset += 1;
        }

        if self.input.chars().nth(self.current_offset).is_none() {
            return Err(ParseError::UnclosedString(start_offset));
        }

        Ok(LexItem::new(
            Token::String,
            Span::new(start_offset, self.current_offset),
        ))
    }

    fn lex_keyword_or_identifier(&mut self) -> LexItem {
        let start_offset = self.current_offset;

        while let Some(c) = self.input.chars().nth(self.current_offset + 1) {
            if !is_keyword_or_identifier_char(c) {
                break;
            }
            self.current_offset += 1;
        }

        let id = &self.input[start_offset..(self.current_offset + 1)];
        let span = Span::new(start_offset, self.current_offset);

        match KEYWORDS.get(id) {
            Some(&token) => LexItem::new(token, span),
            None => LexItem::new(Token::Identifier, span),
        }
    }
}

fn is_keyword_or_identifier_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric()
}

fn is_next_char(input: &str, curr_offset: usize, expect: char) -> bool {
    input
        .chars()
        .nth(curr_offset + 1)
        .map(|next| next == expect)
        .unwrap_or(false)
}
