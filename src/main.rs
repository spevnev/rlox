use std::{
    env, fs,
    io::{stdin, stdout, Write},
    process::{self, ExitCode},
};

#[derive(Clone, Debug, PartialEq)]
enum TokenKind {
    // One or two character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Slash,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Null,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf::phf_map! {
    "and"    => TokenKind::And,
    "class"  => TokenKind::Class,
    "else"   => TokenKind::Else,
    "false"  => TokenKind::False,
    "for"    => TokenKind::For,
    "fun"    => TokenKind::Fun,
    "if"     => TokenKind::If,
    "null"   => TokenKind::Null,
    "or"     => TokenKind::Or,
    "print"  => TokenKind::Print,
    "return" => TokenKind::Return,
    "super"  => TokenKind::Super,
    "this"   => TokenKind::This,
    "true"   => TokenKind::True,
    "var"    => TokenKind::Var,
    "while"  => TokenKind::While
};

type Loc = (usize, usize);

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    str: String,
    loc: Loc,
}

struct Lexer {
    source: Vec<char>,
    index: usize,
    line_index: usize,
    line: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source: source.chars().collect(),
            index: 0,
            line_index: 0,
            line: 1,
        }
    }

    fn is_done(&self) -> bool {
        self.index >= self.source.len()
    }

    fn loc(&self) -> Loc {
        (self.line, self.index - self.line_index + 1)
    }

    fn next_line(&mut self) {
        self.line_index = self.index;
        self.line += 1;
    }

    // Return current char without advancing
    fn peek(&self) -> Option<char> {
        if self.is_done() {
            return None;
        }

        Some(self.source[self.index])
    }

    // Return current char and advances
    fn advance(&mut self) -> Option<char> {
        if self.is_done() {
            return None;
        }

        let ch = self.source[self.index];
        self.index += 1;
        Some(ch)
    }

    // Advances if the next char is `c`
    fn consume(&mut self, c: char) -> bool {
        if self.is_done() || self.source[self.index] != c {
            return false;
        }

        self.index += 1;
        return true;
    }

    // Advances until the current char is `c` or the end
    fn advance_until(&mut self, c: char) {
        while !self.is_done() && self.source[self.index] != c {
            self.index += 1;
        }
    }
}

fn print_error(path: Option<&str>, loc: Loc, message: &str) {
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

fn get_tokens(path: Option<&str>, source: &str) -> Result<Vec<Token>, ()> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut lexer = Lexer::new(source);
    let mut success = true;

    while !lexer.is_done() {
        let start = lexer.index;
        let loc = lexer.loc();

        let kind = match lexer.advance().unwrap() {
            ' ' | '\r' | '\t' => continue,
            '\n' => {
                lexer.next_line();
                continue;
            }

            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '-' => TokenKind::Minus,
            '+' => TokenKind::Plus,
            ';' => TokenKind::Semicolon,
            '*' => TokenKind::Star,

            '/' => {
                if lexer.consume('/') {
                    lexer.advance_until('\n');
                    continue; // Comments don't produce tokens
                } else {
                    TokenKind::Slash
                }
            }
            '!' => {
                if lexer.consume('=') {
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                }
            }
            '=' => {
                if lexer.consume('=') {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                }
            }
            '>' => {
                if lexer.consume('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                }
            }
            '<' => {
                if lexer.consume('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                }
            }

            '"' => {
                let mut backslashes = 0;
                let mut is_terminated = false;

                while let Some(ch) = lexer.advance() {
                    if ch == '"' && backslashes % 2 == 0 {
                        is_terminated = true;
                        break;
                    }

                    if ch == '\n' {
                        lexer.next_line();
                    }

                    backslashes = if ch == '\\' { backslashes + 1 } else { 0 }
                }

                if !is_terminated {
                    print_error(path, loc, "Unterminated string");
                    return Err(());
                }

                TokenKind::String
            }
            'a'..='z' | 'A'..='Z' => {
                while lexer
                    .peek()
                    .is_some_and(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
                {
                    lexer.advance();
                }

                KEYWORDS
                    .get(&source[start..lexer.index])
                    .cloned()
                    .unwrap_or(TokenKind::Identifier)
            }
            '0'..='9' => {
                while lexer.peek().is_some_and(|c| c.is_ascii_digit()) {
                    lexer.advance();
                }

                // Fractional part
                if lexer.consume('.') {
                    while lexer.peek().is_some_and(|c| c.is_ascii_digit()) {
                        lexer.advance();
                    }
                }

                TokenKind::Number
            }

            c => {
                print_error(path, loc, &format!("Unknown character '{c}'"));
                success = false;
                continue;
            }
        };

        tokens.push(Token {
            kind,
            str: source[start..lexer.index].to_string(),
            loc,
        });
    }

    if success {
        Ok(tokens)
    } else {
        Err(())
    }
}

fn run(path: Option<&str>, source: &str) -> Result<(), ()> {
    let tokens = get_tokens(path, source)?;

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
