use std::{
    cell::Cell,
    collections::VecDeque,
    env, fs,
    io::{stderr, stdin, stdout, Write},
    process::ExitCode,
};

use interpreter::Interpreter;
use lexer::tokenize;
use parser::parse;
use resolver::resolve;

mod error;
mod interpreter;
mod lexer;
mod native;
mod parser;
mod resolver;
mod value;

const ONLY_ERRORS_FLAG: &'static str = "--only-errors";

thread_local! {
    pub static ONLY_ERRORS: Cell<bool> = Cell::new(false);
}

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

        let Ok(tokens) = tokenize(&line) else {
            continue;
        };
        // TODO: Allow expressions and print their result
        let Ok(stmts) = parse(tokens) else {
            continue;
        };
        let Ok(_) = resolve(&stmts) else {
            continue;
        };
        let _ = interpreter.eval(&stmts);
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

    let Ok(tokens) = tokenize(&source) else {
        return ExitCode::FAILURE;
    };
    let Ok(stmts) = parse(tokens) else {
        return ExitCode::FAILURE;
    };
    let Ok(_) = resolve(&stmts) else {
        return ExitCode::FAILURE;
    };
    let Ok(_) = Interpreter::new().eval(&stmts) else {
        return ExitCode::FAILURE;
    };

    ExitCode::SUCCESS
}

/// Print the usage. If `is_help` is true, prints to stdout and exits successfully,
/// otherwise prints to stderr and exits with failure.
fn usage(program: &str, is_help: bool) -> ExitCode {
    let usage = format!(
        "
Usage:
  {program}        - REPL
  {program} [path] - Interpret file

Options:
  -h, --help    - show this message
  {ONLY_ERRORS_FLAG} - only print errors (ignore warnings)
"
    );

    if is_help {
        stdout().write(usage.as_bytes()).unwrap();
        ExitCode::SUCCESS
    } else {
        stderr().write(usage.as_bytes()).unwrap();
        ExitCode::FAILURE
    }
}

fn main() -> ExitCode {
    let mut args: VecDeque<String> = env::args().collect();

    let program_path = args.pop_front().unwrap();
    let program_name = program_path.split('/').last().unwrap();

    let mut opt_path = None;
    while let Some(arg) = args.pop_front() {
        if arg == "-h" || arg == "--help" {
            return usage(program_name, true);
        } else if arg == ONLY_ERRORS_FLAG {
            ONLY_ERRORS.set(true);
        } else if opt_path.is_some() {
            return usage(program_name, false);
        } else {
            opt_path = Some(arg);
        }
    }

    if let Some(path) = opt_path {
        run_file(&path)
    } else {
        run_repl()
    }
}
