pub type Loc = (usize, usize);

pub struct ErrorMessage {
    loc: Loc,
    message: String,
}

impl ErrorMessage {
    pub fn new(loc: Loc, message: String) -> ErrorMessage {
        ErrorMessage { loc, message }
    }
}

pub fn write_error(
    f: &mut std::fmt::Formatter<'_>,
    path: Option<&str>,
    error: &ErrorMessage,
) -> Result<(), std::fmt::Error> {
    if path.is_some() {
        write!(
            f,
            "[ERROR] {} at {}:{}:{}.",
            error.message,
            path.unwrap(),
            error.loc.0,
            error.loc.1
        )
    } else {
        assert!(error.loc.0 == 1, "REPL should only have a single line");

        write!(f, "[ERROR] {} at column {}.", error.message, error.loc.1)
    }
}
