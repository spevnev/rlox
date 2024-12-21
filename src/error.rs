pub fn print_error(path: Option<&str>, loc: (usize, usize), message: &str) {
    if path.is_some() {
        // Interpreting file
        eprintln!(
            "[ERROR] {} at {}:{}:{}.",
            message,
            path.unwrap(),
            loc.0,
            loc.1
        );
    } else {
        // REPL
        assert!(loc.0 == 1, "REPL should only have a single line");
        eprintln!("[ERROR] {} at column {}.", message, loc.1);
    }
}
