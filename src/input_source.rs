use std::fs;
use std::io;
use std::path::{Component, Path, PathBuf};

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

    /// Returns the path this source was read from, if it is a file.
    pub fn path(&self) -> Option<&Path> {
        match self {
            InputSource::File(path) => Some(path),
            _ => None,
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

/// Lexically normalize a path: drop `.` components and pop each `..` against
/// the preceding directory name.
///
/// This runs before the file is known to exist, so it must not touch the
/// filesystem. Without it, `a/../b.lox` and `b.lox` are different module
/// identities even though they name the same file — which defeats the parse
/// cache and, for a `..`-spelled circular import, cycle detection as well.
pub fn normalize_path(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => match normalized.components().next_back() {
                // Pop a real directory name.
                Some(Component::Normal(_)) => {
                    normalized.pop();
                }
                // `..` at the filesystem root is the root itself.
                Some(Component::RootDir) => {}
                // Nothing to pop against (empty, or a leading run of `..`).
                _ => normalized.push(component),
            },
            other => normalized.push(other),
        }
    }
    if normalized.as_os_str().is_empty() {
        PathBuf::from(".")
    } else {
        normalized
    }
}

/// Resolve an import path to an absolute, normalized path.
///
/// Paths spelled `./` or `../` resolve against the importing module's
/// directory; every other spelling is CWD-relative (legacy behavior). The
/// result is absolute and normalized so that one file always has exactly one
/// module identity, however it was spelled.
pub fn resolve_relative_path(importer_dir: &Path, rel_path: &str) -> String {
    let joined = if rel_path.starts_with("./") || rel_path.starts_with("../") {
        importer_dir.join(rel_path)
    } else {
        PathBuf::from(rel_path)
    };
    let absolute = std::path::absolute(&joined).unwrap_or(joined);
    normalize_path(&absolute).to_string_lossy().into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    // ---------------------------------------------------------------------
    // normalize_path
    // ---------------------------------------------------------------------

    #[track_caller]
    fn assert_normalizes(input: &str, expected: &str) {
        assert_eq!(
            normalize_path(Path::new(input)),
            PathBuf::from(expected),
            "input: {input}"
        );
    }

    #[test]
    fn normalize_path_pops_parent_dir() {
        assert_normalizes("/a/b/../c.lox", "/a/c.lox");
        assert_normalizes("/a/b/c/../../d.lox", "/a/d.lox");
    }

    #[test]
    fn normalize_path_drops_cur_dir() {
        assert_normalizes("/a/./b/./c.lox", "/a/b/c.lox");
        assert_normalizes("./a.lox", "a.lox");
    }

    #[test]
    fn normalize_path_keeps_leading_parent_dir_when_nothing_to_pop() {
        // Relative paths can genuinely escape their base; keep those `..`s.
        assert_normalizes("../a.lox", "../a.lox");
        assert_normalizes("../../a.lox", "../../a.lox");
        assert_normalizes("a/../../b.lox", "../b.lox");
    }

    #[test]
    fn normalize_path_clamps_parent_dir_at_root() {
        assert_normalizes("/../a.lox", "/a.lox");
        assert_normalizes("/a/../../b.lox", "/b.lox");
    }

    #[test]
    fn normalize_path_of_empty_is_cur_dir() {
        assert_normalizes("", ".");
        assert_normalizes("a/..", ".");
    }

    #[test]
    fn normalize_path_is_idempotent() {
        let once = normalize_path(Path::new("/a/b/../c/./d.lox"));
        assert_eq!(normalize_path(&once), once);
    }

    // ---------------------------------------------------------------------
    // resolve_relative_path
    // ---------------------------------------------------------------------

    #[test]
    fn resolve_relative_path_collapses_parent_dir_spellings() {
        let dir = Path::new("/proj/cyc");
        // These three name the same file and must produce one identity.
        let a = resolve_relative_path(dir, "./b.lox");
        let b = resolve_relative_path(dir, "../cyc/b.lox");
        let c = resolve_relative_path(dir, "./sub/../b.lox");
        assert_eq!(a, "/proj/cyc/b.lox");
        assert_eq!(a, b);
        assert_eq!(a, c);
    }

    #[test]
    fn resolve_relative_path_returns_absolute_for_cwd_relative_spelling() {
        // Non-`./` paths stay CWD-relative, but are still absolutized so the
        // resulting identity does not depend on how they were spelled.
        let resolved = resolve_relative_path(Path::new("/proj"), "modules/m.lox");
        assert!(Path::new(&resolved).is_absolute(), "not absolute: {resolved}");
        assert!(resolved.ends_with("modules/m.lox"), "unexpected: {resolved}");
    }
}
