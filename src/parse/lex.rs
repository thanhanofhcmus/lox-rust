use crate::span::Span;
use crate::token::Token;

use super::error::ParseError;

static KEYWORDS: phf::Map<&'static [u8], Token> = phf::phf_map!(
    b"and" => Token::And,
    b"or" => Token::Or,
    b"not" => Token::Not,

    b"true" => Token::True,
    b"false" => Token::False,
    b"nil" => Token::Nil,

    b"var" => Token::Var,

    b"if" => Token::If,
    b"else" => Token::Else,
    b"while" => Token::While,
    b"for" => Token::For,
    b"when" => Token::When,
    b"in" => Token::In,

    b"fn" => Token::Fn,
    b"return" => Token::Return,

    b"any" => Token::TypeAny,
    b"bool" => Token::TypeBool,
    b"number" => Token::TypeNumber,
    b"str" => Token::TypeStr,

    b"struct" => Token::Struct,

    b"import" => Token::Import,
    b"as" => Token::As,
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

fn is_keyword_or_identifier_char(c: u8) -> bool {
    let c = c as char;
    c == '_' || c.is_ascii_alphanumeric()
}

fn is_next_char(input: &[u8], curr_offset: usize, expect: u8) -> bool {
    input
        .get(curr_offset + 1)
        .map(|next| *next == expect)
        .unwrap_or(false)
}

pub fn lex(input: &str) -> Result<Vec<LexItem>, ParseError> {
    let mut curr_offset = 0;
    let mut result = vec![];

    let utf8_bytes = input.as_bytes();

    while let Some(&c) = utf8_bytes.get(curr_offset) {
        let const_offset = curr_offset; // we don't want to borrow mutate of curr_offset
        let tok_one = |t| LexItem::new(t, Span::one(const_offset));

        let mut push_with_equal = |token_has_equal, token_no_equal| {
            if is_next_char(utf8_bytes, curr_offset, b'=') {
                result.push(LexItem::new(token_has_equal, Span::two(curr_offset)));
                curr_offset += 1;
            } else {
                result.push(LexItem::new(token_no_equal, Span::one(curr_offset)));
            }
        };

        match c {
            b' ' | b'\t' | b'\r' | b'\n' => { /* skip */ }
            b'(' => result.push(tok_one(Token::LRoundParen)),
            b')' => result.push(tok_one(Token::RRoundParen)),
            b'[' => result.push(tok_one(Token::LSquareParen)),
            b']' => result.push(tok_one(Token::RSquareParen)),
            b'{' => result.push(tok_one(Token::LPointParen)),
            b'}' => result.push(tok_one(Token::RPointParen)),
            b'.' => result.push(tok_one(Token::Dot)),
            b',' => result.push(tok_one(Token::Comma)),
            b':' => result.push(tok_one(Token::Colon)),
            b';' => result.push(tok_one(Token::Semicolon)),
            b'+' => result.push(tok_one(Token::Plus)),
            b'*' => result.push(tok_one(Token::Star)),
            b'/' => result.push(tok_one(Token::Slash)),

            b'%' => {
                if is_next_char(utf8_bytes, curr_offset, b'{') {
                    result.push(LexItem::new(
                        Token::PercentLPointParent,
                        Span::two(curr_offset),
                    ));
                    curr_offset += 1;
                } else if is_next_char(utf8_bytes, curr_offset, b'(') {
                    result.push(LexItem::new(
                        Token::PercentLRoundParen,
                        Span::two(curr_offset),
                    ));
                    curr_offset += 1;
                } else {
                    result.push(tok_one(Token::Percentage));
                }
            }

            b'-' => {
                if is_next_char(utf8_bytes, curr_offset, b'>') {
                    result.push(LexItem::new(Token::RThinArrow, Span::two(curr_offset)));
                    curr_offset += 1;
                } else {
                    result.push(tok_one(Token::Minus));
                }
            }

            b'=' => {
                if is_next_char(utf8_bytes, curr_offset, b'=') {
                    result.push(LexItem::new(Token::EqualEqual, Span::two(curr_offset)));
                    curr_offset += 1;
                } else if is_next_char(utf8_bytes, curr_offset, b'>') {
                    result.push(LexItem::new(Token::RFatArrow, Span::two(curr_offset)));
                    curr_offset += 1;
                } else {
                    result.push(tok_one(Token::Equal));
                }
            }

            b'!' => {
                if is_next_char(utf8_bytes, curr_offset, b'=') {
                    result.push(LexItem::new(Token::BangEqual, Span::two(curr_offset)));
                    curr_offset += 1;
                } else {
                    return Err(ParseError::UnexpectedCharacter(Span::one(curr_offset)));
                }
            }

            b'<' => push_with_equal(Token::LessEqual, Token::Less),
            b'>' => push_with_equal(Token::GreaterEqual, Token::Greater),
            b'"' => result.push(lex_string(utf8_bytes, &mut curr_offset)?),

            b'#' => result.push(lex_comment(utf8_bytes, &mut curr_offset)),

            // need to be before the is_keyword_or_identifier_char check
            b'r' if is_next_char(utf8_bytes, curr_offset, b'"') => {
                result.push(lex_raw_string(utf8_bytes, &mut curr_offset)?)
            }

            v if v.is_ascii_digit() => result.push(lex_number(utf8_bytes, &mut curr_offset)?),
            v if is_keyword_or_identifier_char(v) => {
                result.push(lex_keyword_or_identifier(utf8_bytes, &mut curr_offset))
            }
            _ => {
                return Err(ParseError::UnexpectedCharacter(Span::one(curr_offset)));
            }
        }

        curr_offset += 1;
    }

    Ok(result)
}

fn lex_number(input: &[u8], offset: &mut usize) -> Result<LexItem, ParseError> {
    let start_offset = *offset;
    let mut has_parsed_dot = false;

    while let Some(&c) = input.get(*offset + 1) {
        if !(c.is_ascii_digit() || c == b'.') {
            break;
        }
        if c == b'.' {
            if has_parsed_dot {
                return Err(ParseError::UnexpectedCharacter(Span::one(*offset)));
            }
            has_parsed_dot = true
        }
        *offset += 1;
    }

    // check if the last thing we parsed is a dot
    let end_offset = *offset;
    if let Some(&c) = input.get(end_offset)
        && c == b'.'
    {
        return Err(ParseError::UnexpectedCharacter(Span::one(end_offset)));
    }

    Ok(LexItem::new(
        Token::Number,
        Span::new(start_offset, end_offset),
    ))
}

fn lex_string(input: &[u8], offset: &mut usize) -> Result<LexItem, ParseError> {
    let start_offset = *offset;

    *offset += 1; // consume '"'
    while let Some(&c) = input.get(*offset) {
        match c {
            // closing '"'
            b'"' => {
                let end_offset = *offset;

                return Ok(LexItem::new(
                    Token::String,
                    Span::new(start_offset, end_offset),
                ));
            }

            // escape char
            b'\\' => {
                // Skip the current char '\'
                *offset += 1;

                // After advancing past '\', the next byte must be the escaped
                // character. If we're at or past input end, the string is unclosed.
                if *offset >= input.len() {
                    return Err(ParseError::UnclosedString(start_offset));
                }

                // we skip a single byte since we only handle the case of \"
                // TODO: Handle Unicode escape
                *offset += 1;
            }

            // normal char
            _ => {
                *offset += 1;
            }
        };
    }

    // We hit the end with out hitting the closing '"'
    Err(ParseError::UnclosedString(start_offset))
}

fn lex_raw_string(input: &[u8], offset: &mut usize) -> Result<LexItem, ParseError> {
    let start_offset = *offset;

    *offset += 2; // consume 'r' and '"'
    while let Some(&c) = input.get(*offset) {
        match c {
            // closing '"'
            b'"' => {
                let end_offset = *offset;

                return Ok(LexItem::new(
                    Token::RawString,
                    Span::new(start_offset, end_offset),
                ));
            }

            // escape char
            b'\\' if is_next_char(input, *offset, b'"') => {
                // Skip the current char '\' and '"'
                *offset += 2;
            }

            // normal char
            _ => {
                *offset += 1;
            }
        };
    }

    // We hit the end with out hitting the closing '"'
    Err(ParseError::UnclosedString(start_offset))
}

fn lex_keyword_or_identifier(input: &[u8], offset: &mut usize) -> LexItem {
    let start_offset = *offset;

    while let Some(&c) = input.get(*offset + 1) {
        if !is_keyword_or_identifier_char(c) {
            break;
        }
        *offset += 1;
    }

    let id = &input[start_offset..(*offset + 1)];
    let span = Span::new(start_offset, *offset);

    match KEYWORDS.get(id) {
        Some(&token) => LexItem::new(token, span),
        None => LexItem::new(Token::Identifier, span),
    }
}

fn lex_comment(input: &[u8], offset: &mut usize) -> LexItem {
    let start_offset = *offset;

    while let Some(&c) = input.get(*offset) {
        // skip until we get a new line
        if c == b'\n' {
            break;
        }
        *offset += 1;
    }

    LexItem::new(Token::Comment, Span::new(start_offset, *offset))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn tokens(input: &str) -> Vec<Token> {
        lex(input).unwrap().into_iter().map(|li| li.token).collect()
    }

    fn with_text(input: &str) -> Vec<(Token, String)> {
        lex(input)
            .unwrap()
            .into_iter()
            .map(|li| (li.token, li.span.string_from_source(input)))
            .collect()
    }

    // ---------- single-char tokens ----------

    #[test]
    fn parens_and_punctuation() {
        assert_eq!(
            tokens("()[]{}.,:;"),
            vec![
                Token::LRoundParen,
                Token::RRoundParen,
                Token::LSquareParen,
                Token::RSquareParen,
                Token::LPointParen,
                Token::RPointParen,
                Token::Dot,
                Token::Comma,
                Token::Colon,
                Token::Semicolon,
            ]
        );
    }

    #[test]
    fn arithmetic_operators() {
        assert_eq!(
            tokens("+-*/"),
            vec![Token::Plus, Token::Minus, Token::Star, Token::Slash]
        );
    }

    // ---------- compound vs single-char disambiguation ----------

    #[test]
    fn equal_vs_equal_equal_vs_fat_arrow() {
        assert_eq!(tokens("="), vec![Token::Equal]);
        assert_eq!(tokens("=="), vec![Token::EqualEqual]);
        assert_eq!(tokens("=>"), vec![Token::RFatArrow]);
    }

    #[test]
    fn less_and_less_equal() {
        assert_eq!(tokens("<"), vec![Token::Less]);
        assert_eq!(tokens("<="), vec![Token::LessEqual]);
    }

    #[test]
    fn greater_and_greater_equal() {
        assert_eq!(tokens(">"), vec![Token::Greater]);
        assert_eq!(tokens(">="), vec![Token::GreaterEqual]);
    }

    #[test]
    fn bang_equal_only() {
        assert_eq!(tokens("!="), vec![Token::BangEqual]);
    }

    #[test]
    fn lone_bang_is_unexpected_character() {
        let err = lex("!").unwrap_err();
        assert!(
            matches!(err, ParseError::UnexpectedCharacter(_)),
            "expected UnexpectedCharacter, got {err:?}"
        );
    }

    #[test]
    fn minus_vs_arrow() {
        assert_eq!(tokens("-"), vec![Token::Minus]);
        assert_eq!(tokens("->"), vec![Token::RThinArrow]);
    }

    #[test]
    fn percent_vs_percent_brace() {
        assert_eq!(tokens("%"), vec![Token::Percentage]);
        assert_eq!(tokens("%{"), vec![Token::PercentLPointParent]);
    }

    // ---------- keywords ----------

    #[test]
    fn keyword_true_false_nil() {
        assert_eq!(tokens("true"), vec![Token::True]);
        assert_eq!(tokens("false"), vec![Token::False]);
        assert_eq!(tokens("nil"), vec![Token::Nil]);
    }

    #[test]
    fn keyword_control_flow() {
        assert_eq!(
            tokens("if else while for when in"),
            vec![
                Token::If,
                Token::Else,
                Token::While,
                Token::For,
                Token::When,
                Token::In,
            ]
        );
    }

    #[test]
    fn keyword_fn_return_var() {
        assert_eq!(
            tokens("fn return var"),
            vec![Token::Fn, Token::Return, Token::Var]
        );
    }

    #[test]
    fn keyword_logical() {
        assert_eq!(
            tokens("and or not"),
            vec![Token::And, Token::Or, Token::Not]
        );
    }

    #[test]
    fn keyword_types() {
        assert_eq!(
            tokens("any bool number str"),
            vec![
                Token::TypeAny,
                Token::TypeBool,
                Token::TypeNumber,
                Token::TypeStr,
            ]
        );
    }

    #[test]
    fn keyword_import_as() {
        assert_eq!(tokens("import as"), vec![Token::Import, Token::As]);
    }

    // ---------- identifiers vs keyword prefixes ----------

    #[test]
    fn identifier_with_keyword_prefix_is_identifier() {
        assert_eq!(tokens("truex"), vec![Token::Identifier]);
        assert_eq!(tokens("variable"), vec![Token::Identifier]);
        assert_eq!(tokens("nilx"), vec![Token::Identifier]);
    }

    #[test]
    fn identifier_with_underscore_and_digits() {
        assert_eq!(tokens("_foo"), vec![Token::Identifier]);
        assert_eq!(tokens("foo_bar123"), vec![Token::Identifier]);
    }

    // ---------- numbers ----------

    #[test]
    fn integer_literal() {
        assert_eq!(with_text("42"), vec![(Token::Number, "42".to_string())]);
    }

    #[test]
    fn floating_literal() {
        assert_eq!(with_text("3.14"), vec![(Token::Number, "3.14".to_string())]);
    }

    #[test]
    fn number_with_multiple_dots_errors() {
        let err = lex("1.2.3").unwrap_err();
        assert!(matches!(err, ParseError::UnexpectedCharacter(_)));
    }

    #[test]
    fn number_trailing_dot_errors() {
        let err = lex("5.").unwrap_err();
        assert!(matches!(err, ParseError::UnexpectedCharacter(_)));
    }

    // ---------- strings ----------

    #[test]
    fn plain_string_literal() {
        assert_eq!(
            with_text("\"hello\""),
            vec![(Token::String, "\"hello\"".to_string())]
        );
    }

    #[test]
    fn empty_string_literal() {
        assert_eq!(with_text("\"\""), vec![(Token::String, "\"\"".to_string())]);
    }

    #[test]
    fn string_with_escaped_quote_keeps_going() {
        // Span covers the full \"\\\"\" including escaped inner quote
        let toks = lex("\"a\\\"b\"").unwrap();
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].token, Token::String);
    }

    #[test]
    fn unclosed_string_errors() {
        let err = lex("\"unterminated").unwrap_err();
        assert!(
            matches!(err, ParseError::UnclosedString(_)),
            "expected UnclosedString, got {err:?}"
        );
    }

    #[test]
    fn raw_string_literal() {
        let toks = lex(r#"r"hello""#).unwrap();
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].token, Token::RawString);
    }

    #[test]
    fn raw_string_with_escaped_quote() {
        let toks = lex(r#"r"a\"b""#).unwrap();
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].token, Token::RawString);
    }

    // ---------- comments ----------

    #[test]
    fn comment_until_newline() {
        let toks = lex("# a comment\nvar").unwrap();
        assert_eq!(
            toks.iter().map(|li| li.token).collect::<Vec<_>>(),
            vec![Token::Comment, Token::Var]
        );
    }

    #[test]
    fn comment_at_eof() {
        let toks = lex("# end").unwrap();
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].token, Token::Comment);
    }

    // ---------- whitespace ----------

    #[test]
    fn whitespace_is_skipped() {
        assert_eq!(
            tokens(" \t\nvar\n\t x "),
            vec![Token::Var, Token::Identifier]
        );
    }

    #[test]
    fn tokens_back_to_back_without_whitespace() {
        assert_eq!(
            tokens("var x=1;"),
            vec![
                Token::Var,
                Token::Identifier,
                Token::Equal,
                Token::Number,
                Token::Semicolon,
            ]
        );
    }

    // ---------- errors ----------

    #[test]
    fn unknown_character_errors() {
        let err = lex("@").unwrap_err();
        assert!(
            matches!(err, ParseError::UnexpectedCharacter(_)),
            "expected UnexpectedCharacter, got {err:?}"
        );
    }

    // ---------- span correctness ----------

    #[test]
    fn compound_operator_span_covers_both_chars() {
        let toks = lex("==").unwrap();
        assert_eq!(toks[0].span.start, 0);
        assert_eq!(toks[0].span.end, 1);
    }

    #[test]
    fn identifier_span_covers_full_name() {
        let toks = lex("foo").unwrap();
        assert_eq!(toks[0].span.str_from_source("foo"), "foo");
    }
}
