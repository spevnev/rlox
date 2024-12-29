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
    Var(String),
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

    fn new_var(loc: Loc, id: String) -> LocExpr {
        LocExpr {
            loc,
            expr: Expr::Var(id),
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

pub enum Stmt {
    Expr(LocExpr),
    Print(LocExpr),
    Var(Loc, String, LocExpr),
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0 }
    }

    fn is_done(&self) -> bool {
        self.index >= self.tokens.len()
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
        if self.index >= self.tokens.len() {
            return false;
        }

        if self.is_next(kinds) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, ()> {
        if self.index >= self.tokens.len() {
            assert!(self.tokens.len() > 0);
            print_error(
                self.tokens[self.tokens.len() - 1].loc,
                format!("Expected {:?} but reached the end", kind),
            );
            return Err(());
        }

        if self.is_next(&[kind.clone()]) {
            Ok(self.advance().unwrap())
        } else {
            error_expected(
                &format!("{:?}", kind),
                &self.tokens[self.index].value,
                self.tokens[self.index].loc,
            )
        }
    }

    // Discards all the tokens until the next statement before entering panic mode
    fn sync(&mut self) {
        while !self.is_done() && !self.consume(&[TokenKind::Semicolon]) {
            self.advance();

            if self.is_next(&[
                TokenKind::Class,
                TokenKind::Fun,
                TokenKind::Var,
                TokenKind::For,
                TokenKind::While,
                TokenKind::If,
                TokenKind::Print,
                TokenKind::Return,
            ]) {
                return;
            }
        }
    }

    fn parse_primary(&mut self) -> Result<LocExpr, ()> {
        let opt_token = self.advance();
        if opt_token.is_none() {
            assert!(self.tokens.len() > 0);
            print_error(
                self.tokens[self.tokens.len() - 1].loc,
                "Expected expression but reached the end".to_owned(),
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
            TokenKind::Identifier => Ok(LocExpr::new_var(
                token.loc,
                token.value.to_identifier(token.loc)?,
            )),
            TokenKind::LeftParen => {
                let expr = self.parse_expr()?;
                if self.consume(&[TokenKind::RightParen]) {
                    Ok(expr)
                } else {
                    print_error(token.loc, "Unclosed '('".to_owned());
                    Err(())
                }
            }
            _ => {
                error_expected("expression", &token.value, token.loc)
            }
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

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ()> {
        let expr = self.parse_expr()?;

        if self.is_done() {
            assert!(self.tokens.len() > 0);
            print_error(
                self.tokens[self.tokens.len() - 1].loc,
                "Expected semicolon after the statement but reached the end".to_owned(),
            );
            return Err(());
        }

        let token = self.advance().unwrap();
        if token.kind == TokenKind::Semicolon {
            Ok(Stmt::Expr(expr))
        } else {
            print_error(
                token.loc,
                "Expected semicolon after the statement".to_owned(),
            );
            Err(())
        }
    }

    fn parse_print_stmt(&mut self) -> Result<Stmt, ()> {
        let expr = self.parse_expr()?;

        if self.is_done() {
            assert!(self.tokens.len() > 0);
            print_error(
                self.tokens[self.tokens.len() - 1].loc,
                "Expected semicolon after the print statement but reached the end".to_owned(),
            );
            return Err(());
        }

        let token = self.advance().unwrap();
        if token.kind == TokenKind::Semicolon {
            Ok(Stmt::Print(expr))
        } else {
            print_error(
                token.loc,
                "Expected semicolon after the print statement".to_owned(),
            );
            Err(())
        }
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ()> {
        if self.consume(&[TokenKind::Print]) {
            self.parse_print_stmt()
        } else {
            self.parse_expr_stmt()
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, ()> {
        let id = self.expect(TokenKind::Identifier)?;

        let mut init = LocExpr::new_literal((0, 0), Value::Null(()));
        if self.consume(&[TokenKind::Equal]) {
            init = self.parse_expr()?;
        }

        let token = self.advance().unwrap();
        if token.kind == TokenKind::Semicolon {
            Ok(Stmt::Var(id.loc, id.value.to_identifier(id.loc)?, init))
        } else {
            print_error(
                token.loc,
                "Expected semicolon after the variable declaration".to_owned(),
            );
            Err(())
        }
    }

    fn parse_decl(&mut self) -> Result<Stmt, ()> {
        if self.consume(&[TokenKind::Var]) {
            self.parse_var_decl()
        } else {
            self.parse_stmt()
        }
    }

    fn parse(&mut self) -> Result<Vec<Stmt>, ()> {
        let mut stmts: Vec<Stmt> = Vec::new();
        let mut has_error = false;

        while !self.is_done() {
            if let Ok(stmt) = self.parse_decl() {
                stmts.push(stmt);
            } else {
                self.sync();
                has_error = true;
            }
        }

        if has_error {
            Err(())
        } else {
            assert!(
                self.index == self.tokens.len(),
                "Parser must reach the end of tokens"
            );
            Ok(stmts)
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, ()> {
    Parser::new(tokens).parse()
}
