use std::{env::args, process::ExitCode};

fn usage(program: &str) {
    eprintln!("usage:");
    eprintln!("{program}        - REPL");
    eprintln!("{program} [path] - interpret file");
}

fn run_repl() {
    println!("Running REPL");
}

fn run_file(path: &str) {
    println!("Interpreting \"{path}\"");
}

fn main() -> ExitCode {
    let args: Vec<String> = args().collect();
    let program = args[0].split('/').last().unwrap();

    match args.len() {
        1 => {
            run_repl();
            return ExitCode::SUCCESS;
        }
        2 => {
            run_file(&args[1]);
            return ExitCode::SUCCESS;
        }
        _ => {
            usage(program);
            return ExitCode::FAILURE;
        }
    }
}
