use derive_more::Display;

#[derive(Debug, Clone, Copy, Display)]
#[display("[{}:{}]", start, end)]
pub struct Span {
    // inclusive range
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    pub fn one(start: usize) -> Self {
        Span { start, end: start }
    }

    pub fn two(start: usize) -> Self {
        Span {
            start,
            end: start + 1,
        }
    }

    pub fn str_from_source<'a>(&self, input: &'a str) -> &'a str {
        &input[self.start..=self.end]
    }

    pub fn string_from_source(&self, input: &str) -> String {
        self.str_from_source(input).to_string()
    }

    /// Translate the byte offset `self.start` into a 1-indexed (row, col).
    /// Counts Unicode scalar values (chars), not bytes, so multi-byte UTF-8
    /// characters contribute a single column. After a newline the next char
    /// is reported as column 1.
    pub fn to_start_row_col(self, input: &str) -> (usize, usize) {
        let mut row = 1;
        let mut col = 1;
        for (i, c) in input.char_indices() {
            if i >= self.start {
                break;
            }
            if c == '\n' {
                row += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        (row, col)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn new_builds_inclusive_range() {
        let s = Span::new(2, 5);
        assert_eq!(s.start, 2);
        assert_eq!(s.end, 5);
    }

    #[test]
    fn one_is_single_char_span() {
        let s = Span::one(3);
        assert_eq!(s.start, 3);
        assert_eq!(s.end, 3);
    }

    #[test]
    fn two_covers_two_chars() {
        let s = Span::two(3);
        assert_eq!(s.start, 3);
        assert_eq!(s.end, 4);
    }

    #[test]
    fn display_format_shows_start_colon_end() {
        assert_eq!(format!("{}", Span::new(1, 4)), "[1:4]");
    }

    #[test]
    fn str_from_source_slices_inclusive() {
        let src = "abcdef";
        assert_eq!(Span::new(0, 0).str_from_source(src), "a");
        assert_eq!(Span::new(0, 2).str_from_source(src), "abc");
        assert_eq!(Span::new(3, 5).str_from_source(src), "def");
    }

    #[test]
    fn str_from_source_single_char_via_one() {
        assert_eq!(Span::one(2).str_from_source("hello"), "l");
    }

    #[test]
    fn string_from_source_matches_str_version() {
        let src = "hello world";
        let span = Span::new(6, 10);
        assert_eq!(span.string_from_source(src), "world");
        assert_eq!(span.string_from_source(src), span.str_from_source(src));
    }

    #[test]
    fn row_col_at_start_is_one_one() {
        assert_eq!(Span::one(0).to_start_row_col("hello"), (1, 1));
    }

    #[test]
    fn row_col_mid_first_line() {
        // "abc" — char at index 2 is 'c', which sits at column 3
        assert_eq!(Span::one(2).to_start_row_col("abcdef"), (1, 3));
    }

    #[test]
    fn row_col_right_after_newline_is_col_one() {
        // "a\nb" — 'b' at index 2 is the first char on row 2 → col 1.
        assert_eq!(Span::one(2).to_start_row_col("a\nb"), (2, 1));
    }

    #[test]
    fn row_col_across_multiple_newlines() {
        // "a\nb\nc" — 'c' is the first char on row 3 → col 1.
        assert_eq!(Span::one(4).to_start_row_col("a\nb\nc"), (3, 1));
    }

    #[test]
    fn row_col_mid_line_on_later_row() {
        // "abc\ndef" — 'e' at byte 5 is the 2nd char on row 2 → col 2.
        assert_eq!(Span::one(5).to_start_row_col("abc\ndef"), (2, 2));
    }

    #[test]
    fn row_col_counts_chars_not_bytes_for_multibyte() {
        // "Öx" — 'Ö' is 2 bytes, 'x' is at byte 2. It's the 2nd char on row 1 → col 2.
        assert_eq!(Span::one(2).to_start_row_col("Öx"), (1, 2));
    }

    #[test]
    fn row_col_multibyte_after_newline() {
        // "a\nÖx" — 'x' at byte 4 is the 2nd char on row 2 → col 2.
        assert_eq!(Span::one(4).to_start_row_col("a\nÖx"), (2, 2));
    }

    #[test]
    fn row_col_past_end_stops_gracefully() {
        // Span start beyond input length: the loop exits early via get() -> None
        // and returns whatever was accumulated up to the actual end.
        let src = "ab";
        let (row, col) = Span::one(100).to_start_row_col(src);
        assert_eq!(row, 1);
        assert_eq!(col, 3); // walked past "ab" twice, no newline
    }

    #[test]
    fn row_col_on_empty_input_is_one_one() {
        assert_eq!(Span::one(0).to_start_row_col(""), (1, 1));
    }
}
