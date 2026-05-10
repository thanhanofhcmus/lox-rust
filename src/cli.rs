use std::env;

#[derive(Debug)]
pub enum Mode {
    Repl { initial_line: Option<String> },
    Prompt { line: String },
    File { path: String },
}

pub struct Config {
    pub mode: Mode,
    pub strict_assert: bool,
}

impl Config {
    pub fn parse() -> Self {
        let mut args: Vec<String> = env::args().skip(1).collect();

        // Help flag — checked first so it works regardless of position.
        if args.iter().any(|a| a == "-h" || a == "--help") {
            print_help();
            std::process::exit(0);
        }

        let strict_assert = {
            let before = args.len();
            args.retain(|a| a != "--strict-assert");
            args.len() != before
        };

        if args.is_empty() {
            eprintln!("error: expected a mode (-i, -p, -f)");
            eprintln!("run with -h for help");
            std::process::exit(1);
        }

        let mode_str = args.remove(0);
        let mode = match mode_str.as_str() {
            "-i" => Mode::Repl {
                initial_line: args.into_iter().next(),
            },
            "-p" => Mode::Prompt {
                line: args.into_iter().next().unwrap_or_else(|| {
                    eprintln!("error: must provide a line for -p mode");
                    std::process::exit(1);
                }),
            },
            "-f" => Mode::File {
                path: args.into_iter().next().unwrap_or_else(|| {
                    eprintln!("error: must provide a file path for -f mode");
                    std::process::exit(1);
                }),
            },
            other => {
                eprintln!("error: unknown mode '{other}'");
                eprintln!("run with -h for help");
                std::process::exit(1);
            }
        };

        Self { mode, strict_assert }
    }
}

fn print_help() {
    let name = env::args().next().unwrap_or_else(|| "lox-rust".into());
    println!("Usage: {name} [--strict-assert] <mode> [args]");
    println!();
    println!("A Lox language interpreter.");
    println!();
    println!("Modes:");
    println!("  -i [line]        Start interactive REPL (optionally execute <line> first)");
    println!("  -p <line>        Execute a single line of Lox code");
    println!("  -f <path>        Execute a Lox source file");
    println!("  -h, --help       Show this help message");
    println!();
    println!("Options:");
    println!("  --strict-assert  Enable strict assertion checking");
}
