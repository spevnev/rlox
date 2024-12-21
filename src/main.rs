use std::{
    env, fs,
    io::{stdin, stdout, Write},
    process::{self, ExitCode},
};

mod error;
mod lexer;

fn run(path: Option<&str>, source: &str) -> Result<(), ()> {
    let tokens = lexer::get_tokens(path, source)?;

    for token in tokens {
        println!("{token:?}");
    }

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

        let _ = run(None, &line);
    }
}

fn run_file(path: &str) {
    let source = fs::read_to_string(path).unwrap_or_else(|err| {
        eprintln!("Unable to read file \"{path}\": {err}");
        process::exit(1);
    });

    run(Some(path), &source).unwrap_or_else(|_| process::exit(1));
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
