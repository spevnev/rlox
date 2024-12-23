use core::fmt;

use crate::error::{write_error, ErrorMessage, Loc};

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub str: String,
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
        return true;
    }

    // Advances until the current char is `c` or the end
    fn advance_until(&mut self, c: char) {
        while !self.is_done() && self.source[self.index] != c {
            self.index += 1;
        }
    }
}

pub struct LexerError<'a> {
    path: Option<&'a str>,
    errors: Vec<ErrorMessage>,
}

impl<'a> LexerError<'a> {
    fn new(path: Option<&'a str>) -> LexerError<'a> {
        LexerError {
            path,
            errors: Vec::new(),
        }
    }

    fn add(&mut self, loc: Loc, message: String) {
        self.errors.push(ErrorMessage::new(loc, message));
    }
}

impl<'a> fmt::Display for LexerError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.errors.len() {
            if i > 0 {
                write!(f, "\n")?;
            }
            write_error(f, self.path, &self.errors[i])?;
        }

        Ok(())
    }
}

pub fn get_tokens<'a>(path: Option<&'a str>, source: &str) -> Result<Vec<Token>, LexerError<'a>> {
    let mut error = LexerError::new(path);
    let mut tokens: Vec<Token> = Vec::new();
    let mut lexer = Lexer::new(source);

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
                    error.add(loc, "Unterminated string".to_string());
                    return Err(error);
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
                error.add(loc, format!("Unknown character '{c}'"));
                continue;
            }
        };

        tokens.push(Token {
            kind,
            str: source[start..lexer.index].to_string(),
            loc,
        });
    }

    if error.errors.len() == 0 {
        Ok(tokens)
    } else {
        Err(error)
    }
}
