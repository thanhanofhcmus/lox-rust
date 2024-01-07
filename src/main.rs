mod expr;
mod interpreter;
mod lex;
mod parse;
mod parse_error;
mod span;
mod token;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let input = args.get(1).expect("must have one argument");
    println!("{:?}", input);

    let tokens = match lex::lex(input) {
        Ok(list) => list,
        Err(err) => {
            println!("Lex error: {}", err);
            return;
        }
    };

    for token in &tokens {
        println!(
            "{} - {:?}: {:?}",
            token.span,
            token.token,
            token.span.extract_from_source(input),
        );
    }

    let expr = match parse::parse(input, &tokens) {
        Ok(list) => list,
        Err(err) => {
            println!("Parse error: {}", err);
            return;
        }
    };
    println!("{:?}", expr);

    let calc_result = interpreter::calculate(expr);
    println!("{:?}", calc_result);
}
