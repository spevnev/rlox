use std::{
    collections::VecDeque,
    env, fs,
    io::{stderr, stdin, stdout, Write},
    process::ExitCode,
    sync::OnceLock,
};

use interpreter::Interpreter;
use lexer::tokenize;
use parser::{parse, parse_expr};
use resolver::resolve;

#[macro_use]
mod error;
mod interpreter;
mod lexer;
mod native;
mod parser;
mod resolver;
mod value;

pub struct Options {
    pub only_errors: bool,
}

impl Options {
    fn new() -> Self {
        Self { only_errors: false }
    }
}

pub static OPTIONS: OnceLock<Options> = OnceLock::new();

/// Exit codes from "sysexits.h" (https://man.openbsd.org/sysexits):
const EX_USAGE: u8 = 64;
const EX_DATA_ERROR: u8 = 65;
const EX_NO_INPUT: u8 = 66;

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

        let Ok(tokens) = tokenize(&line) else { continue };
        match parse(&tokens) {
            Ok(stmts) => {
                let Ok(_) = resolve(&stmts) else { continue };
                let _ = interpreter.eval(&stmts);
            },
            // If statement parsing fails, try parsing input as an expression.
            Err(errors) => {
                let Ok(expr) = parse_expr(&tokens) else {
                    // If parsing expression also fails, print original errors.
                    eprintln!("{}", errors);
                    continue;
                };
                let Ok(value) = interpreter.eval_expr(&expr) else {
                    continue;
                };
                println!("{}", value.convert_to_string());
            },
        }
    }

    ExitCode::SUCCESS
}

fn run_file(path: &str) -> ExitCode {
    let source = match fs::read_to_string(path) {
        Ok(source) => source,
        Err(error) => {
            eprintln!("Unable to read file \"{}\": {}", path, error);
            return ExitCode::from(EX_NO_INPUT);
        },
    };

    let Ok(tokens) = tokenize(&source) else {
        return ExitCode::from(EX_DATA_ERROR);
    };
    let stmts = match parse(&tokens) {
        Ok(stmts) => stmts,
        Err(errors) => {
            eprintln!("{}", errors);
            return ExitCode::from(EX_DATA_ERROR);
        },
    };
    let Ok(_) = resolve(&stmts) else {
        return ExitCode::from(EX_DATA_ERROR);
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
Interpreter for Lox programming language.

Usage:
  {program}        - REPL
  {program} <path> - interpret file

Options:
  -h, --help    - show this message
  --only-errors - only print errors (ignore warnings)
"
    );

    if is_help {
        stdout().write(usage.as_bytes()).unwrap();
        ExitCode::SUCCESS
    } else {
        stderr().write(usage.as_bytes()).unwrap();
        ExitCode::from(EX_USAGE)
    }
}

fn main() -> ExitCode {
    let mut args: VecDeque<String> = env::args().collect();

    let program_path = args.pop_front().unwrap();
    let program_name = program_path.split('/').last().unwrap();

    let mut command_args = Vec::new();
    let mut options = Options::new();

    // IMPORTANT: Keep options/commands in sync with `usage`.
    // Parse flags to set options, and save command args.
    while let Some(arg) = args.pop_front() {
        if arg == "-h" || arg == "--help" {
            return usage(program_name, true);
        } else if arg == "--only-errors" {
            options.only_errors = true;
        } else {
            command_args.push(arg);
        }
    }

    let Ok(()) = OPTIONS.set(options) else {
        panic!("Unable to set options.");
    };

    match command_args.len() {
        0 => run_repl(),
        1 => run_file(&command_args[0]),
        _ => usage(program_name, false),
    }
}
