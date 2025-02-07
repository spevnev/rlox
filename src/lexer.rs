use crate::{
    error::{error, Loc},
    value::Value,
};

type Result<V, E = ()> = std::result::Result<V, E>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // One or two character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
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
    Question,
    Colon,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Break,
    Class,
    Continue,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
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
            TokenKind::LeftBracket => "[",
            TokenKind::RightBracket => "]",
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
            TokenKind::Question => "?",
            TokenKind::Colon => ":",
            TokenKind::Identifier => "identifier",
            TokenKind::String => "string",
            TokenKind::Number => "number",
            TokenKind::And => "and",
            TokenKind::Break => "break",
            TokenKind::Class => "class",
            TokenKind::Continue => "continue",
            TokenKind::Else => "else",
            TokenKind::False => "false",
            TokenKind::Fun => "fun",
            TokenKind::For => "for",
            TokenKind::If => "if",
            TokenKind::Nil => "nil",
            TokenKind::Or => "or",
            TokenKind::Print => "print",
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
    "and"      => TokenKind::And,
    "break"    => TokenKind::Break,
    "class"    => TokenKind::Class,
    "continue" => TokenKind::Continue,
    "else"     => TokenKind::Else,
    "false"    => TokenKind::False,
    "for"      => TokenKind::For,
    "fun"      => TokenKind::Fun,
    "if"       => TokenKind::If,
    "nil"      => TokenKind::Nil,
    "or"       => TokenKind::Or,
    "print"    => TokenKind::Print,
    "return"   => TokenKind::Return,
    "super"    => TokenKind::Super,
    "this"     => TokenKind::This,
    "true"     => TokenKind::True,
    "var"      => TokenKind::Var,
    "while"    => TokenKind::While
};

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Value,
    pub loc: Loc,
    pub len: usize,
}

impl Token {
    pub fn end_loc(&self) -> Loc {
        let mut end_loc = self.loc;
        end_loc.column += self.len;
        end_loc
    }

    pub fn to_identifier(&self) -> String {
        match &self.value {
            Value::Identifier(id) => id.clone(),
            _ => panic!("Expected Token to be an Identifier."),
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

pub fn tokenize(source: &str) -> Result<Vec<Token>> {
    if !source.is_ascii() {
        eprintln!("[ERROR] non-ASCII characters aren't supported.");
        return Err(());
    }

    let mut lexer = Lexer::new(source);
    let mut tokens: Vec<Token> = Vec::new();
    let mut had_error = false;

    while !lexer.is_done() {
        let start = lexer.index;
        let loc = lexer.current_loc();

        let mut value = Value::Nil;
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
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '-' => TokenKind::Minus,
            '+' => TokenKind::Plus,
            ';' => TokenKind::Semicolon,
            '*' => TokenKind::Star,
            '?' => TokenKind::Question,
            ':' => TokenKind::Colon,
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
                    error(loc, "Unterminated string");
                    return Err(());
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
                    let mut has_fraction = false;
                    while lexer.peek().is_some_and(|c| c.is_ascii_digit()) {
                        lexer.advance();
                        has_fraction = true;
                    }
                    if !has_fraction {
                        // If there are no digits after the dot, exclude it from the token.
                        lexer.index -= 1;
                    }
                }

                if let Ok(number) = source[start..lexer.index].parse::<f64>() {
                    value = Value::Number(number);
                    TokenKind::Number
                } else {
                    error!(loc, "Unable to parse number '{}'", &source[start..lexer.index]);
                    had_error = true;
                    continue;
                }
            },
            c => {
                error!(loc, "Unknown character '{c}'");
                had_error = true;
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

    if had_error {
        Err(())
    } else {
        Ok(tokens)
    }
}
