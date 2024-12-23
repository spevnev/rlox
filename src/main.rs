use std::{
    env, fs,
    io::{stdin, stdout, Write},
    process::{self, ExitCode},
};

use lexer::LexerError;
use parser::print_ast;

mod error;
mod lexer;
mod parser;

fn run<'a>(path: Option<&'a str>, source: &str) -> Result<(), LexerError<'a>> {
    let tokens = lexer::get_tokens(path, source)?; // TODO: rename to lex?
    let ast = parser::parse(tokens).unwrap(); // TODO: propagate/handle error

    print_ast(&ast);

    Ok(())
}

fn run_repl() {
    loop {
        print!("> ");
        stdout().flush().unwrap();

        let mut line = String::new();
        let size = stdin().read_line(&mut line).unwrap();
        if size == 0 {
            // Ctrl-D
            break;
        }

        let _ = run(None, &line).inspect_err(|err| eprintln!("{err}"));
    }
}

fn run_file(path: &str) {
    let source = fs::read_to_string(path).unwrap_or_else(|err| {
        eprintln!("Unable to read file \"{path}\": {err}");
        process::exit(1);
    });

    run(Some(path), &source).unwrap_or_else(|err| {
        eprintln!("{err}");
        process::exit(1);
    });
}

fn usage(program: &str) {
    eprintln!("usage:");
    eprintln!("{program}        - REPL");
    eprintln!("{program} [path] - interpret file");
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    let program = args[0].split('/').last().unwrap();

    match args.len() {
        1 => {
            run_repl();
            ExitCode::SUCCESS
        }
        2 => {
            run_file(&args[1]);
            ExitCode::SUCCESS
        }
        _ => {
            usage(program);
            ExitCode::FAILURE
        }
    }
}
