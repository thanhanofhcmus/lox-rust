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

    pub fn to_start_row_col(self, input: &str) -> (usize, usize) {
        let mut row = 1;
        let mut col = 1;
        for i in 0..self.start {
            if let Some(c) = input.chars().nth(i) {
                match c {
                    '\n' => {
                        row += 1;
                        col = 0;
                    }
                    _ => {
                        col += 1;
                    }
                }
            }
        }
        (row, col)
    }
}
