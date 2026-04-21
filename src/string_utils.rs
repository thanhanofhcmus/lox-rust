pub fn unescape(input: &str) -> String {
    let mut iter = input.chars();
    let mut result = String::with_capacity(input.len());

    while let Some(c) = iter.next() {
        if c != '\\' {
            result.push(c);
            continue;
        }
        // go past '\'
        let Some(nc) = iter.next() else {
            panic!("Unclosed escape \\\" chars should already be caught at the lexing stage");
        };
        match nc {
            'r' => result.push('\r'),
            'n' => result.push('\n'),
            't' => result.push('\t'),
            '"' => result.push('"'),
            '\\' => result.push('\\'),
            _ => {
                // for other char, we treat it as is
                // \z => z, \c => c
                result.push(nc);
            }
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn empty_input_stays_empty() {
        assert_eq!(unescape(""), "");
    }

    #[test]
    fn no_escape_passes_through() {
        assert_eq!(unescape("hello world"), "hello world");
    }

    #[test]
    fn newline_escape() {
        assert_eq!(unescape("a\\nb"), "a\nb");
    }

    #[test]
    fn carriage_return_escape() {
        assert_eq!(unescape("a\\rb"), "a\rb");
    }

    #[test]
    fn tab_escape() {
        assert_eq!(unescape("a\\tb"), "a\tb");
    }

    #[test]
    fn quote_escape() {
        assert_eq!(unescape("say \\\"hi\\\""), "say \"hi\"");
    }

    #[test]
    fn backslash_escape() {
        assert_eq!(unescape("path\\\\to"), "path\\to");
    }

    #[test]
    fn unknown_escape_drops_the_backslash() {
        // Current behavior: \z becomes z (backslash removed, escaped char passes through)
        assert_eq!(unescape("\\z"), "z");
        assert_eq!(unescape("\\q\\w\\e"), "qwe");
    }

    #[test]
    fn consecutive_escapes() {
        assert_eq!(unescape("\\n\\t\\r"), "\n\t\r");
    }

    #[test]
    fn escape_at_start_and_end() {
        assert_eq!(unescape("\\nhello\\n"), "\nhello\n");
    }

    #[test]
    #[should_panic(expected = "Unclosed escape")]
    fn trailing_backslash_panics() {
        // Documented behavior: a dangling '\\' with no following char panics.
        // The lexer is responsible for rejecting this earlier.
        unescape("abc\\");
    }
}
