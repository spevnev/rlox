use std::{
    env, fs,
    io::{stdin, stdout, Write},
    process::{self, ExitCode},
};

use interpreter::Interpreter;
use parser::Stmt;

mod error;
mod interpreter;
mod lexer;
mod parser;
mod print;

fn parse(source: &str) -> Result<Vec<Stmt>, ()> {
    let tokens = lexer::get_tokens(source)?;
    if tokens.is_empty() {
        return Ok(Vec::new());
    }

    println!("Tokens:");
    for token in &tokens {
        println!("{token:?} ");
    }
    println!();

    let stmts = parser::parse(tokens)?;

    println!("Statements AST:");
    print::print_stmts(&stmts);
    println!();

    Ok(stmts)
}

fn run_repl() {
    let mut interpreter = Interpreter::new();

    loop {
        print!("> ");
        stdout().flush().unwrap();

        let mut line = String::new();
        let size = stdin().read_line(&mut line).unwrap();
        if size == 0 {
            // Ctrl-D
            break;
        }

        let stmts = parse(&line);
        if stmts.is_ok() {
            let _ = interpreter.eval(stmts.unwrap());
        }
    }
}

fn run_file(path: &str) {
    let source = fs::read_to_string(path).unwrap_or_else(|err| {
        eprintln!("Unable to read file \"{path}\": {err}");
        process::exit(1);
    });

    let stmts = parse(&source).unwrap_or_else(|_| process::exit(1));
    Interpreter::new()
        .eval(stmts)
        .unwrap_or_else(|_| process::exit(1));
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
