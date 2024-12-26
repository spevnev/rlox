use crate::{
    error::{error_expected, print_error, Loc},
    lexer::{Token, TokenKind, Value},
};

pub struct Unary {
    pub op: TokenKind,
    pub expr: Box<LocExpr>,
}

pub struct Binary {
    pub left: Box<LocExpr>,
    pub op: TokenKind,
    pub right: Box<LocExpr>,
}

pub enum Expr {
    Literal(Value),
    Unary(Unary),
    Binary(Binary),
}

pub struct LocExpr {
    pub loc: Loc,
    pub expr: Expr,
}

impl LocExpr {
    fn new_literal(loc: Loc, value: Value) -> LocExpr {
        LocExpr {
            loc,
            expr: Expr::Literal(value),
        }
    }

    fn new_unary(op: Token, expr: LocExpr) -> LocExpr {
        LocExpr {
            loc: op.loc,
            expr: Expr::Unary(Unary {
                op: op.kind,
                expr: Box::new(expr),
            }),
        }
    }

    fn new_binary(left: LocExpr, op: Token, right: LocExpr) -> LocExpr {
        LocExpr {
            loc: left.loc,
            expr: Expr::Binary(Binary {
                left: Box::new(left),
                op: op.kind,
                right: Box::new(right),
            }),
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

    fn parse_primary(&mut self) -> Result<LocExpr, ()> {
        let opt_token = self.advance();
        if opt_token.is_none() {
            assert!(self.tokens.len() > 0);
            print_error(
                self.tokens[self.tokens.len() - 1].loc,
                "Expected expression but reached the end.".to_owned(),
            );
            return Err(());
        }

        let token = opt_token.unwrap();
        match token.kind {
            TokenKind::Number | TokenKind::String => {
                Ok(LocExpr::new_literal(token.loc, token.value))
            }
            TokenKind::False => Ok(LocExpr::new_literal(token.loc, Value::Bool(false))),
            TokenKind::True => Ok(LocExpr::new_literal(token.loc, Value::Bool(true))),
            TokenKind::Null => Ok(LocExpr::new_literal(token.loc, Value::Null(()))),
            TokenKind::LeftParen => {
                let expr = self.parse_expr()?;
                if self.is_next(&[TokenKind::RightParen]) {
                    self.advance();
                    Ok(expr)
                } else {
                    print_error(token.loc, "Unclosed '('".to_owned());
                    Err(())
                }
            }
            _ => error_expected("expression", &token.value, token.loc),
        }
    }

    fn parse_unary(&mut self) -> Result<LocExpr, ()> {
        if self.is_next(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let expr = self.parse_unary()?;
            return Ok(LocExpr::new_unary(op, expr));
        }

        self.parse_primary()
    }

    fn parse_mult(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_unary()?;

        while self.is_next(&[TokenKind::Star, TokenKind::Slash]) {
            let op = self.advance().unwrap();
            let right = self.parse_unary()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_mult()?;

        while self.is_next(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let right = self.parse_mult()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_addition()?;

        while self.is_next(&[
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let op = self.advance().unwrap();
            let right = self.parse_addition()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_comparison()?;

        while self.is_next(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
            let op = self.advance().unwrap();
            let right = self.parse_comparison()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_expr(&mut self) -> Result<LocExpr, ()> {
        self.parse_equality()
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<LocExpr, ()> {
    Parser::new(tokens).parse_expr()
}
