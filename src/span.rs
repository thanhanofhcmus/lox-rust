use derive_more::Display;

#[derive(Debug, Clone, Copy, Display)]
#[display(fmt = "[{}:{}]", start, end)]
pub struct Span {
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

    pub fn extract_from_source<'a>(&self, input: &'a [u8]) -> &'a [u8] {
        &input[self.start..self.end]
    }
}
