use crate::{
    error::{print_error, Loc},
    lexer::{Token, TokenKind, TokenValue},
};

pub enum Expr {
    Literal(Literal),
    Unary(Unary),
    Binary(Binary),
}
impl Expr {
    pub fn loc(&self) -> Loc {
        match self {
            Self::Literal(literal) => literal.loc,
            Self::Unary(unary) => unary.loc,
            Self::Binary(binary) => binary.loc,
        }
    }
}

pub struct Literal {
    pub loc: Loc,
    pub value: TokenValue,
}

impl Literal {
    fn new(loc: Loc, value: TokenValue) -> Literal {
        Literal { loc, value }
    }
}

pub struct Unary {
    pub loc: Loc,
    pub op: TokenKind,
    pub expr: Box<Expr>,
}

impl Unary {
    fn new(op: Token, expr: Expr) -> Unary {
        Unary {
            loc: op.loc,
            op: op.kind,
            expr: Box::new(expr),
        }
    }
}

pub struct Binary {
    pub loc: Loc,
    pub left: Box<Expr>,
    pub op: TokenKind,
    pub right: Box<Expr>,
}

impl Binary {
    fn new(left: Expr, op: Token, right: Expr) -> Binary {
        Binary {
            loc: op.loc,
            left: Box::new(left),
            op: op.kind,
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

    fn parse_primary(&mut self) -> Result<Expr, ()> {
        let opt_token = self.advance();
        if opt_token.is_none() {
            assert!(self.tokens.len() > 0);
            print_error(
                self.tokens[self.tokens.len() - 1].loc,
                "Expected expression".to_string(),
            );
            return Err(());
        }

        let token = opt_token.unwrap();
        match token.kind {
            TokenKind::Number | TokenKind::String => {
                Ok(Expr::Literal(Literal::new(token.loc, token.value)))
            }
            TokenKind::False => Ok(Expr::Literal(Literal::new(
                token.loc,
                TokenValue::Bool(false),
            ))),
            TokenKind::True => Ok(Expr::Literal(Literal::new(
                token.loc,
                TokenValue::Bool(true),
            ))),
            TokenKind::Null => Ok(Expr::Literal(Literal::new(token.loc, TokenValue::Null(())))),
            TokenKind::LeftParen => {
                let expr = self.parse_expr()?;
                if self.consume(&[TokenKind::RightParen]) {
                    Ok(expr)
                } else {
                    print_error(token.loc, "Unclosed '('".to_string());
                    Err(())
                }
            }
            _ => {
                // TODO: change error message
                print_error(token.loc, "Expected expression".to_string());
                Err(())
            }
        }
    }

    fn parse_unary(&mut self) -> Result<Expr, ()> {
        if self.is_next(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary(Unary::new(op, expr)));
        }

        self.parse_primary()
    }

    fn parse_mult(&mut self) -> Result<Expr, ()> {
        let mut expr = self.parse_unary()?;

        while self.is_next(&[TokenKind::Star, TokenKind::Slash]) {
            let op = self.advance().unwrap();
            let right = self.parse_unary()?;
            expr = Expr::Binary(Binary::new(expr, op, right));
        }

        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<Expr, ()> {
        let mut expr = self.parse_mult()?;

        while self.is_next(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let right = self.parse_mult()?;
            expr = Expr::Binary(Binary::new(expr, op, right));
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ()> {
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

    fn parse_equality(&mut self) -> Result<Expr, ()> {
        let mut expr = self.parse_comparison()?;

        while self.is_next(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
            let op = self.advance().unwrap();
            let right = self.parse_comparison()?;
            expr = Expr::Binary(Binary::new(expr, op, right));
        }

        Ok(expr)
    }

    fn parse_expr(&mut self) -> Result<Expr, ()> {
        self.parse_equality()
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Expr, ()> {
    Parser::new(tokens).parse_expr()
}

fn print_ast_rec(expr: &Expr) {
    match expr {
        Expr::Unary(unary) => {
            print!("({:?} ", unary.op);
            print_ast_rec(&unary.expr);
            print!(")");
        }
        Expr::Binary(binary) => {
            print!("(");
            print_ast_rec(&binary.left);
            print!(" {:?} ", binary.op);
            print_ast_rec(&binary.right);
            print!(")");
        }
        Expr::Literal(literal) => print!("{}", literal.value.convert_to_string()),
    };
}

pub fn print_ast(expr: &Expr) {
    print_ast_rec(expr);
    print!("\n");
}
