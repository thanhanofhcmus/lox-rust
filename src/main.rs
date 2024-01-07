mod ast;
mod interpreter;
mod lex;
mod parse;
mod parse_error;
mod span;
mod token;

use log::{debug, error, info};

fn main() {
    env_logger::init();

    let args = std::env::args().collect::<Vec<String>>();
    let input = args.get(1).expect("must have one argument");
    debug!("{:?}", input);

    let mut it = interpreter::Environment::new();

    if input != "-i" {
        run_one_stmt(input, &mut it);
        return;
    }

    loop {
        let mut line = String::new();
        std::io::stdin()
            .read_line(&mut line)
            .expect("read from stdin failed");

        if line == "quit" {
            break;
        }

        run_one_stmt(line.trim_end(), &mut it);
    }
}

fn run_one_stmt(input: &str, it: &mut interpreter::Environment) {
    let tokens = match lex::lex(input) {
        Ok(list) => list,
        Err(err) => {
            error!("Lex error: {}", err);
            return;
        }
    };

    for token in &tokens {
        debug!(
            "{} - {:?}: {:?}",
            token.span,
            token.token,
            token.span.extract_from_source(input),
        );
    }

    let expr = match parse::parse(input, &tokens) {
        Ok(list) => list,
        Err(err) => {
            error!("Parse error: {}", err);
            return;
        }
    };

    let calc_result = interpreter::interpret_stmt(it, expr);

    match calc_result {
        Ok(value) => info!(
            "{:?}",
            value
                .map(|v| format!("{}", v))
                .unwrap_or("None".to_string())
        ),
        Err(err) => error!("{:?}", err),
    }
}
