mod lex;
mod span;
mod token;

fn main() {
    let input = "(123+-*/)";

    let tokens = lex::lex(input.as_bytes()).unwrap();

    for token in tokens {
        println!(
            "{} - {:?}: {:?}",
            token.span,
            token.token,
            std::str::from_utf8(token.span.extract_from_source(input.as_bytes())).unwrap(),
        );
    }
}
