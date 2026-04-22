use crate::span::Span;
use crate::token::Token;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Invalid character encountered: '{0}'")]
    UnexpectedCharacter(Span),

    #[error("Invalid placement: 'import' statements must appear at the top of the file.")]
    ImportNotAtTheTop(Span),

    #[error("Unexpected token `{0}` found. {}", format_expected(.2))]
    UnexpectedToken(Token, Span, Option<Token>), // Changed Option to String for better custom messaging

    #[error("Syntax error: String literal started at position {0} remains unclosed.")]
    UnclosedString(usize),

    #[error("Numeric conversion failed: The sequence at {0} is not a valid number.")]
    ParseToNumber(Span),

    #[error("Scope error: 'return' is only permitted within the body of a function.")]
    UnexpectedReturn(Span),

    #[error("Unexpected End of File (EOF): {}", format_expected(.0))]
    Eof(Option<Token>),

    #[error(
        "Process halted: The parser reached the end of the logic but found trailing tokens starting with `{0}`."
    )]
    Unfinished(Token, Span),

    #[error("Assignment error: The left-hand side of a reassignment must be a valid identifier.")]
    ReassignRootIsNotAnIdentifier,

    #[error(
        "Parser recursion limit ({0}) exceeded — expression is nested too deeply to parse safely."
    )]
    RecursionLimitExceeded(usize),

    #[error(
        "Invalid map key type: only `any`, `bool`, `number`, `str`, and `nil` are allowed as map keys."
    )]
    InvalidMapKeyType(Span),
}

impl ParseError {
    pub fn get_source_start(&self, input: &str) -> (usize, usize) {
        use ParseError::*;
        match self {
            UnexpectedCharacter(s) => s.to_start_row_col(input),
            ImportNotAtTheTop(s) => s.to_start_row_col(input),
            UnexpectedToken(_, s, _) => s.to_start_row_col(input),
            UnclosedString(u) => (*u, *u),
            ParseToNumber(s) => s.to_start_row_col(input),
            UnexpectedReturn(s) => s.to_start_row_col(input),
            Eof(_) => (0, 0),
            Unfinished(_, s) => s.to_start_row_col(input),
            ReassignRootIsNotAnIdentifier => (0, 0),
            RecursionLimitExceeded(_) => (0, 0),
            InvalidMapKeyType(s) => s.to_start_row_col(input),
        }
    }

    pub fn generate_user_facing_error(&self, source_name: Option<&str>, input: &str) -> String {
        let (line, col) = self.get_source_start(input);

        // Locate the line in source
        let source_line = input.lines().nth(line.saturating_sub(1)).unwrap_or("");

        // Format the visual components
        let source_name = source_name.map(|v| v.to_string()).unwrap_or_default();
        let line_label = line.to_string();
        let gutter_padding = " ".repeat(line_label.len());
        let pointer_indent = " ".repeat(col.saturating_sub(1));

        // Construct the full diagnostic
        let mut output = String::new();

        // Header line
        output.push_str(&format!("Error: {}\n", self));

        // File location line (clickable in most IDEs)
        output.push_str(&format!("  --> {}:{}:{}\n", source_name, line, col));

        // Code snippet visualization
        output.push_str(&format!("{} |\n", gutter_padding));
        output.push_str(&format!("{} | {}\n", line_label, source_line));
        output.push_str(&format!(
            "{} | {}{}\n",
            gutter_padding, pointer_indent, "^--- here"
        ));

        output
    }
}

/// Helper for constructing expectation messages without first-person pronouns
fn format_expected(expected: &Option<Token>) -> String {
    match expected {
        Some(t) => format!("The parser expected the token `{:?}` in this position.", t),
        None => "Verify the syntax conforms to the language grammar.".to_string(),
    }
}
