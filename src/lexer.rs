use crate::error::{print_error, Loc};

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

#[derive(PartialEq, Debug, Clone)]
pub enum TokenValue {
    Number(f64),
    String(String),
    Identifier(String),
    Bool(bool),
    Null(()),
}

impl TokenValue {
    pub fn convert_to_string(&self) -> String {
        match self {
            TokenValue::Number(number) => number.to_string(),
            TokenValue::String(string) => string.to_owned(),
            TokenValue::Identifier(identifier) => identifier.to_owned(),
            TokenValue::Bool(bool) => bool.to_string(),
            TokenValue::Null(()) => "null".to_owned(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: TokenValue,
    pub loc: Loc,
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
        true
    }

    // Advances until the current char is `c` or the end
    fn advance_until(&mut self, c: char) {
        while !self.is_done() && self.source[self.index] != c {
            self.index += 1;
        }
    }
}

pub fn get_tokens(source: &str) -> Result<Vec<Token>, ()> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut lexer = Lexer::new(source);
    let mut has_error = false;

    while !lexer.is_done() {
        let start = lexer.index;
        let loc = lexer.loc();

        let mut value = TokenValue::Null(());
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
                    print_error(loc, "Unterminated string".to_owned());
                    return Err(());
                }

                value = TokenValue::String(source[start..lexer.index].to_owned());
                TokenKind::String
            }
            'a'..='z' | 'A'..='Z' => {
                while lexer
                    .peek()
                    .is_some_and(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
                {
                    lexer.advance();
                }

                let identifier = &source[start..lexer.index];
                if let Some(keyword) = KEYWORDS.get(identifier).cloned() {
                    keyword
                } else {
                    value = TokenValue::Identifier(identifier.to_owned());
                    TokenKind::Identifier
                }
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

                if let Ok(number) = source[start..lexer.index].parse::<f64>() {
                    value = TokenValue::Number(number);
                    TokenKind::Number
                } else {
                    print_error(
                        loc,
                        format!("Unable to parse number '{}'", &source[start..lexer.index]),
                    );
                    has_error = true;
                    continue;
                }
            }

            c => {
                print_error(loc, format!("Unknown character '{c}'"));
                has_error = true;
                continue;
            }
        };

        tokens.push(Token { kind, value, loc });
    }

    if has_error {
        Err(())
    } else {
        Ok(tokens)
    }
}
