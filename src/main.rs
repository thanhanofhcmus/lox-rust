mod expr;
mod interpreter;
mod lex;
mod parse;
mod parse_error;
mod span;
mod token;

fn main() {
    let input = "1 + 1 * 2".as_bytes();

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

    let expr = parse::parse(input, &tokens);
    println!("{:?}", expr);

    let calc_result = interpreter::calculate(expr.unwrap());
    println!("{:?}", calc_result);
}
