use crate::lexer::Value;

pub type Loc = (usize, usize);

pub fn print_error(loc: Loc, message: &str) {
    eprintln!("[ERROR] {} at {}:{}.", message, loc.0, loc.1)
}

pub fn error_expected<T>(expected: &str, value: &Value, loc: Loc) -> Result<T, ()> {
    print_error(
        loc,
        &format!(
            "Expected {expected} but found '{}'",
            value.convert_to_string(true)
        ),
    );
    Err(())
}
