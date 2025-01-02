#[derive(PartialEq, Clone, Copy)]
pub struct Loc {
    pub line: usize,
    pub column: usize,
}

impl Loc {
    pub fn none() -> Self {
        Self { line: 0, column: 0 }
    }
}

pub fn print_error(loc: Loc, message: &str) {
    eprintln!("[ERROR] {} at {}:{}.", message, loc.line, loc.column);
}

pub fn error<T>(loc: Loc, message: &str) -> Result<T, ()> {
    print_error(loc, message);
    Err(())
}
