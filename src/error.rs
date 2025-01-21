use crate::OPTIONS;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Loc {
    pub line: usize,
    pub column: usize,
}

impl Loc {
    pub fn none() -> Self {
        Self { line: 0, column: 0 }
    }
}

pub fn warning(loc: Loc, message: &str) {
    if !OPTIONS.get().unwrap().only_errors {
        eprintln!("[WARNING] {} at {}:{}.", message, loc.line, loc.column);
    }
}

pub fn error(loc: Loc, message: &str) {
    eprintln!("[ERROR] {} at {}:{}.", message, loc.line, loc.column);
}
