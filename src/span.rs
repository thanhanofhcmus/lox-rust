use derive_more::Display;

#[derive(Debug, Clone, Copy, Display)]
#[display(fmt = "[{}:{}]", start, end)]
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
}
