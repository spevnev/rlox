pub type Loc = (usize, usize);

pub fn print_error(loc: Loc, message: String) {
    eprintln!("[ERROR] {} at {}:{}.", message, loc.0, loc.1)
}
