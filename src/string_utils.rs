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
            panic!("Unclosesed escape \\\" chars should already be catch at the lexing state");
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
