use std::{
    env, fs,
    io::{self, stdin, stdout, Read, Write},
    process::{self, ExitCode},
};

fn usage(program: &str) {
    eprintln!("usage:");
    eprintln!("{program}        - REPL");
    eprintln!("{program} [path] - interpret file");
}

fn run(code: &str) {
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

        run(&line);
    }
}

fn run_file(path: &str) {
    let code = fs::read_to_string(path).unwrap_or_else(|err| {
        eprintln!("Unable to read file \"{path}\": {err}");
        process::exit(1);
    });

    run(&code);
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
