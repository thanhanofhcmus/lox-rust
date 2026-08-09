use std::fs;
use std::io;
use std::path::PathBuf;

use crate::span::Span;

/// Represents the source of a Lox module being executed.
/// Used for error reporting to extract source text at specific spans.
#[derive(Debug, Clone)]
pub enum InputSource {
    /// REPL input — each line is standalone. source_name is "<line>".
    Repl(String),
    /// Prompt mode — single-statement execution. source_name is "<prompt>".
    Prompt(String),
    /// File — stores the absolute path; source text is read on demand.
    File(PathBuf),
}

impl InputSource {
    /// Returns the human-readable source name for use in diagnostics
    /// (e.g. "at ...:line:col").
    pub fn source_name(&self) -> &str {
        match self {
            InputSource::Repl(_) => "<line>",
            InputSource::Prompt(_) => "<prompt>",
            InputSource::File(path) => path.to_str().unwrap_or("<unknown>"),
        }
    }

    /// Returns the full source text.
    pub fn get_text(&self) -> io::Result<String> {
        match self {
            InputSource::Repl(s) | InputSource::Prompt(s) => Ok(s.clone()),
            InputSource::File(path) => fs::read_to_string(path),
        }
    }

    /// Converts a byte-offset `Span` start into a 1-indexed (row, col).
    pub fn to_start_row_col(&self, span: Span) -> (usize, usize) {
        let text = match self.get_text() {
            Ok(t) => t,
            Err(_) => return (0, 0),
        };
        span.to_start_row_col(&text)
    }

    /// Returns the text covered by the given span.
    pub fn span_text(&self, span: Span) -> String {
        let text = match self.get_text() {
            Ok(t) => t,
            Err(_) => return String::new(),
        };
        span.string_from_source(&text)
    }

    /// Returns a specific source line (1-indexed).
    pub fn line(&self, line_number: usize) -> Option<String> {
        let text = self.get_text().ok()?;
        text.lines().nth(line_number.saturating_sub(1)).map(|s| s.to_string())
    }
}
