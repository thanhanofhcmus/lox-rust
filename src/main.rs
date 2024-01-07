mod expression;
mod lex;
mod parse_error;
mod span;
mod token;

fn main() {
    let input = "(123+456)".as_bytes();

    println!("{:?}", std::str::from_utf8(input).unwrap());

    let tokens = lex::lex(input).unwrap();

    for token in &tokens {
        println!(
            "{} - {:?}: {:?}",
            token.span,
            token.token,
            std::str::from_utf8(token.span.extract_from_source(input)).unwrap(),
        );
    }

    let expr = expression::parse(input, &tokens);
    println!("{:?}", expr);
}
