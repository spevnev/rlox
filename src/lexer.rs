use std::rc::Rc;

use crate::{
    error::{error, print_error, Loc},
    parser::Stmt,
};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
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
    AndAnd,
    PipePipe,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Null,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl TokenKind {
    pub fn to_string(&self) -> &'static str {
        match self {
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftBrace => "{",
            TokenKind::RightBrace => "}",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Minus => "-",
            TokenKind::Plus => "+",
            TokenKind::Semicolon => ";",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Bang => "!",
            TokenKind::BangEqual => "!=",
            TokenKind::Equal => "=",
            TokenKind::EqualEqual => "==",
            TokenKind::Greater => ">",
            TokenKind::GreaterEqual => ">=",
            TokenKind::Less => "<",
            TokenKind::LessEqual => "<=",
            TokenKind::AndAnd => "&&",
            TokenKind::PipePipe => "||",
            TokenKind::Identifier => "identifier",
            TokenKind::String => "string",
            TokenKind::Number => "number",
            TokenKind::Class => "class",
            TokenKind::Else => "else",
            TokenKind::False => "false",
            TokenKind::Fun => "fun",
            TokenKind::For => "for",
            TokenKind::If => "if",
            TokenKind::Null => "null",
            TokenKind::Return => "return",
            TokenKind::Super => "super",
            TokenKind::This => "this",
            TokenKind::True => "true",
            TokenKind::Var => "var",
            TokenKind::While => "while",
        }
    }
}

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf::phf_map! {
    "class"  => TokenKind::Class,
    "else"   => TokenKind::Else,
    "false"  => TokenKind::False,
    "for"    => TokenKind::For,
    "fun"    => TokenKind::Fun,
    "if"     => TokenKind::If,
    "null"   => TokenKind::Null,
    "return" => TokenKind::Return,
    "super"  => TokenKind::Super,
    "this"   => TokenKind::This,
    "true"   => TokenKind::True,
    "var"    => TokenKind::Var,
    "while"  => TokenKind::While
};

pub type NativeFunction = fn(Vec<Value>) -> Value;

#[derive(PartialEq)]
pub struct LoxFunction {
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(PartialEq)]
pub enum Function {
    Native(NativeFunction),
    /// params, body
    Lox(Rc<LoxFunction>),
}

#[derive(PartialEq)]
pub struct Callable {
    pub name: String,
    /// number of arguments
    pub arity: usize,
    pub fun: Function,
}

#[derive(PartialEq, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Identifier(String),
    Bool(bool),
    Null,
    Callable(Rc<Callable>),
}

impl Value {
    pub fn convert_to_string(&self, quote_string: bool) -> String {
        match self {
            Value::Number(number) => number.to_string(),
            Value::String(string) => {
                // Quoting differentiates identifier and string in error messages.
                if quote_string {
                    format!("\"{string}\"")
                } else {
                    string.clone()
                }
            },
            Value::Identifier(identifier) => identifier.clone(),
            Value::Bool(bool) => bool.to_string(),
            Value::Null => "null".to_owned(),
            Value::Callable(callable) => format!("<fun {}>", callable.name),
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Value,
    pub loc: Loc,
    pub len: usize,
}

impl Token {
    fn to_error_string(&self) -> String {
        match self.kind {
            TokenKind::Number | TokenKind::String | TokenKind::Identifier => self.value.convert_to_string(true),
            _ => self.kind.to_string().to_owned(),
        }
    }

    fn type_expected_error<T>(&self, expected: &str) -> Result<T, ()> {
        error(
            self.loc,
            &format!("Expected {expected} but found '{}'", self.to_error_string()),
        )
    }

    pub fn to_identifier(&self) -> Result<String, ()> {
        match &self.value {
            Value::Identifier(id) => Ok(id.clone()),
            _ => self.type_expected_error("identifier"),
        }
    }
}

struct Lexer {
    source: Vec<char>,
    index: usize,
    line_index: usize,
    line: usize,
}

impl Lexer {
    fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            index: 0,
            line_index: 0,
            line: 1,
        }
    }

    fn current_loc(&self) -> Loc {
        Loc {
            line: self.line,
            column: self.index - self.line_index + 1,
        }
    }

    fn is_done(&self) -> bool {
        self.index >= self.source.len()
    }

    /// Update location on '\n'.
    fn next_line(&mut self) {
        self.line_index = self.index;
        self.line += 1;
    }

    fn peek(&self) -> Option<char> {
        if self.is_done() {
            return None;
        }

        Some(self.source[self.index])
    }

    fn advance(&mut self) -> Option<char> {
        if self.is_done() {
            return None;
        }

        let ch = self.source[self.index];
        self.index += 1;
        Some(ch)
    }

    /// Advances if the next char is `c`.
    fn consume(&mut self, c: char) -> bool {
        if self.is_done() || self.source[self.index] != c {
            return false;
        }

        self.index += 1;
        true
    }
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, ()> {
    let mut lexer = Lexer::new(source);
    let mut tokens: Vec<Token> = Vec::new();
    let mut has_error = false;

    while !lexer.is_done() {
        let start = lexer.index;
        let loc = lexer.current_loc();

        let mut value = Value::Null;
        let kind = match lexer.advance().unwrap() {
            ' ' | '\r' | '\t' => continue,
            '\n' => {
                lexer.next_line();
                continue;
            },
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
                    while let Some(c) = lexer.peek() {
                        if c == '\n' {
                            break;
                        }
                        lexer.advance();
                    }
                    continue; // Comments don't produce tokens.
                } else {
                    TokenKind::Slash
                }
            },
            '!' => {
                if lexer.consume('=') {
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                }
            },
            '=' => {
                if lexer.consume('=') {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                }
            },
            '>' => {
                if lexer.consume('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                }
            },
            '<' => {
                if lexer.consume('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                }
            },
            '&' => {
                if lexer.consume('&') {
                    TokenKind::AndAnd
                } else {
                    print_error(loc, "Lox doesn't have bitwise operators");
                    has_error = true;
                    continue;
                }
            },
            '|' => {
                if lexer.consume('|') {
                    TokenKind::PipePipe
                } else {
                    print_error(loc, "Lox doesn't have bitwise operators");
                    has_error = true;
                    continue;
                }
            },
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
                    return error(loc, "Unterminated string");
                }

                value = Value::String(source[(start + 1)..(lexer.index - 1)].to_owned());
                TokenKind::String
            },
            'a'..='z' | 'A'..='Z' => {
                while lexer.peek().is_some_and(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_') {
                    lexer.advance();
                }

                let identifier = &source[start..lexer.index];
                if let Some(keyword) = KEYWORDS.get(identifier).cloned() {
                    keyword
                } else {
                    value = Value::Identifier(identifier.to_owned());
                    TokenKind::Identifier
                }
            },
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

                if let Ok(number) = source[start..lexer.index].parse::<f64>() {
                    value = Value::Number(number);
                    TokenKind::Number
                } else {
                    print_error(
                        loc,
                        &format!("Unable to parse number '{}'", &source[start..lexer.index]),
                    );
                    has_error = true;
                    continue;
                }
            },
            c => {
                print_error(loc, &format!("Unknown character '{c}'"));
                has_error = true;
                continue;
            },
        };

        tokens.push(Token {
            kind,
            value,
            loc,
            len: lexer.index - start,
        });
    }

    if !has_error {
        Ok(tokens)
    } else {
        Err(())
    }
}
