use std::{
    env, fs,
    io::{stdin, stdout, Write},
    process::ExitCode,
};

use interpreter::Interpreter;

mod error;
mod interpreter;
mod lexer;
mod native;
mod parser;

fn run_repl() -> ExitCode {
    let mut interpreter = Interpreter::new();
    let mut line = String::new();

    loop {
        print!("> ");
        stdout().flush().unwrap();

        line.clear();
        let size = stdin().read_line(&mut line).unwrap();
        if size == 0 {
            // Ctrl-D
            break;
        }

        let Ok(tokens) = lexer::tokenize(&line) else {
            continue;
        };

        // TODO: Allow expressions and print their result
        let Ok(stmts) = parser::parse(tokens) else {
            continue;
        };

        let _ = interpreter.eval(stmts);
    }

    ExitCode::SUCCESS
}

fn run_file(path: &str) -> ExitCode {
    let source = match fs::read_to_string(path) {
        Ok(source) => source,
        Err(error) => {
            eprintln!("Unable to read file \"{path}\": {error}");
            return ExitCode::FAILURE;
        },
    };

    let Ok(tokens) = lexer::tokenize(&source) else {
        return ExitCode::FAILURE;
    };
    let Ok(stmts) = parser::parse(tokens) else {
        return ExitCode::FAILURE;
    };
    let Ok(_) = Interpreter::new().eval(stmts) else {
        return ExitCode::FAILURE;
    };

    ExitCode::SUCCESS
}

fn usage(program: &str) -> ExitCode {
    eprintln!("usage:");
    eprintln!("{program}        - REPL");
    eprintln!("{program} [path] - interpret file");

    ExitCode::FAILURE
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => run_repl(),
        2 => run_file(&args[1]),
        _ => {
            let program = args[0].split('/').last().unwrap();
            usage(program)
        },
    }
}

