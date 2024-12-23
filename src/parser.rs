use crate::lexer::{Token, TokenKind, TokenValue};

pub enum Expr {
    Unary(Unary),
    Binary(Binary),
    Literal(TokenValue),
}

pub struct Unary {
    op: Token,
    expr: Box<Expr>,
}

impl Unary {
    fn new(op: Token, expr: Expr) -> Unary {
        Unary {
            op,
            expr: Box::new(expr),
        }
    }
}

pub struct Binary {
    left: Box<Expr>,
    op: Token,
    right: Box<Expr>,
}

impl Binary {
    fn new(left: Expr, op: Token, right: Expr) -> Binary {
        Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0 }
    }

    fn advance(&mut self) -> Option<Token> {
        if self.index >= self.tokens.len() {
            return None;
        }

        let token = self.tokens[self.index].clone();
        self.index += 1;
        Some(token)
    }

    fn is_next(&mut self, kinds: &[TokenKind]) -> bool {
        if self.index >= self.tokens.len() {
            return false;
        }

        for kind in kinds {
            if self.tokens[self.index].kind == *kind {
                return true;
            }
        }

        false
    }

    fn consume(&mut self, kinds: &[TokenKind]) -> bool {
        if let Some(token) = self.advance() {
            for kind in kinds {
                if token.kind == *kind {
                    return true;
                }
            }
        }

        false
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        let opt_token = self.advance();
        if opt_token.is_none() {
            return Err("Expected expression.".to_string()); // TODO: better message, proper type
        }

        let token = opt_token.unwrap();
        match token.kind {
            TokenKind::Number | TokenKind::String => Ok(Expr::Literal(token.value)),
            TokenKind::False => Ok(Expr::Literal(TokenValue::Bool(false))),
            TokenKind::True => Ok(Expr::Literal(TokenValue::Bool(true))),
            TokenKind::Null => Ok(Expr::Literal(TokenValue::Null(()))),
            TokenKind::LeftParen => {
                let expr = self.parse_expr()?;
                if self.consume(&[TokenKind::RightParen]) {
                    Ok(expr)
                } else {
                    // TODO: better message, proper type
                    Err(format!("Unclosed '(' at {}:{}.", token.loc.0, token.loc.1,))
                }
            }
            _ => Err("Expected expression.".to_string()), // TODO: better message, proper type
        }
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        if self.is_next(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary(Unary::new(op, expr)));
        }

        self.parse_primary()
    }

    fn parse_mult(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_unary()?;

        while self.is_next(&[TokenKind::Star, TokenKind::Slash]) {
            let op = self.advance().unwrap();
            let right = self.parse_unary()?;
            expr = Expr::Binary(Binary::new(expr, op, right));
        }

        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_mult()?;

        while self.is_next(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let right = self.parse_mult()?;
            expr = Expr::Binary(Binary::new(expr, op, right));
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_addition()?;

        while self.is_next(&[
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let op = self.advance().unwrap();
            let right = self.parse_addition()?;
            expr = Expr::Binary(Binary::new(expr, op, right));
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_comparison()?;

        while self.is_next(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
            let op = self.advance().unwrap();
            let right = self.parse_comparison()?;
            expr = Expr::Binary(Binary::new(expr, op, right));
        }

        Ok(expr)
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_equality()
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Expr, String> {
    Parser::new(tokens).parse_expr()
}

fn print_ast_rec(expr: &Expr) {
    match expr {
        Expr::Unary(unary) => {
            print!("({:?} ", unary.op.kind);
            print_ast_rec(&unary.expr);
            print!(")");
        }
        Expr::Binary(binary) => {
            print!("(");
            print_ast_rec(&binary.left);
            print!(" {:?} ", binary.op.kind);
            print_ast_rec(&binary.right);
            print!(")");
        }
        Expr::Literal(value) => match value {
            TokenValue::Number(number) => print!("{number}"),
            TokenValue::String(string) => print!("{string}"),
            TokenValue::Identifier(identifier) => print!("{identifier}"),
            TokenValue::Bool(bool) => print!("{bool}"),
            TokenValue::Null(()) => print!("null"),
            TokenValue::None(()) => panic!(),
        },
    };
}

pub fn print_ast(expr: &Expr) {
    print_ast_rec(expr);
    print!("\n");
}
