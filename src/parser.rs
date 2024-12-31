use crate::{
    error::{error, Loc},
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

pub struct Assign {
    pub var: String,
    pub expr: Box<LocExpr>,
}

pub enum Expr {
    Literal(Value),
    Unary(Unary),
    Binary(Binary),
    Var(String),
    Assign(Assign),
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

    fn new_var(loc: Loc, var: String) -> LocExpr {
        LocExpr {
            loc,
            expr: Expr::Var(var),
        }
    }

    fn new_assign(loc: Loc, var: String, expr: LocExpr) -> LocExpr {
        LocExpr {
            loc,
            expr: Expr::Assign(Assign {
                var,
                expr: Box::new(expr),
            }),
        }
    }
}

pub enum Stmt {
    Expr(LocExpr),
    Print(LocExpr),
    VarDecl(Token, LocExpr),
    Block(Vec<Stmt>),
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    eof_loc: Loc,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        assert!(tokens.len() > 0);
        let last = &tokens[tokens.len() - 1];

        Parser {
            eof_loc: (last.loc.0, last.loc.1 + last.len),
            tokens,
            index: 0,
        }
    }

    // Returns location immediately after the previous token.
    fn loc_after_prev(&self) -> Loc {
        if self.is_done() {
            self.eof_loc
        } else {
            assert!(self.index > 0, "Previous token must exist");
            let cur = &self.tokens[self.index - 1];
            (cur.loc.0, cur.loc.1 + cur.len)
        }
    }

    fn is_done(&self) -> bool {
        self.index >= self.tokens.len()
    }

    fn is_next(&mut self, kind: &TokenKind) -> bool {
        !self.is_done() && self.tokens[self.index].kind == *kind
    }

    fn are_next(&mut self, kinds: &[TokenKind]) -> bool {
        if self.is_done() {
            return false;
        }

        for kind in kinds {
            if *kind == self.tokens[self.index].kind {
                return true;
            }
        }

        false
    }

    fn advance(&mut self) -> Option<Token> {
        if self.is_done() {
            return None;
        }

        let token = self.tokens[self.index].clone();
        self.index += 1;
        Some(token)
    }

    // Advances if the next token is of kind `kind`.
    fn consume(&mut self, kind: &TokenKind) -> bool {
        if self.is_done() {
            return false;
        }

        if self.is_next(kind) {
            self.index += 1;
            true
        } else {
            false
        }
    }

    // Advances and returns error if next token is not of kind `kind`.
    fn expect(&mut self, kind: &TokenKind) -> Result<Token, ()> {
        if self.is_done() {
            return error(
                self.eof_loc,
                &format!("Expected {} but reached the end", kind.to_string()),
            );
        }

        if self.is_next(kind) {
            Ok(self.advance().unwrap())
        } else {
            error(
                self.tokens[self.index].loc,
                &format!("Expected {}", kind.to_string()),
            )
        }
    }

    // Discards all the tokens until the next statement before entering panic mode.
    fn sync(&mut self) {
        while !self.is_done()
            && !self.consume(&TokenKind::Semicolon)
            && !self.are_next(&[
                TokenKind::Class,
                TokenKind::Fun,
                TokenKind::For,
                TokenKind::If,
                TokenKind::Print,
                TokenKind::Return,
                TokenKind::Var,
                TokenKind::While,
            ])
        {
            self.advance();
        }
    }

    fn parse_primary(&mut self) -> Result<LocExpr, ()> {
        let Some(token) = self.advance() else {
            return error(self.eof_loc, "Expected expression");
        };

        match token.kind {
            TokenKind::Number | TokenKind::String => {
                Ok(LocExpr::new_literal(token.loc, token.value))
            },
            TokenKind::False => Ok(LocExpr::new_literal(token.loc, Value::Bool(false))),
            TokenKind::True => Ok(LocExpr::new_literal(token.loc, Value::Bool(true))),
            TokenKind::Null => Ok(LocExpr::new_literal(token.loc, Value::Null(()))),
            TokenKind::Identifier => Ok(LocExpr::new_var(token.loc, token.to_identifier()?)),
            TokenKind::LeftParen => {
                let expr = self.parse_expr()?;
                if self.consume(&TokenKind::RightParen) {
                    Ok(expr)
                } else {
                    error(self.loc_after_prev(), "Unclosed '(', expected ')'")
                }
            },
            _ => error(token.loc, "Expected expression"),
        }
    }

    fn parse_unary(&mut self) -> Result<LocExpr, ()> {
        if self.are_next(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let expr = self.parse_unary()?;
            Ok(LocExpr::new_unary(op, expr))
        } else {
            self.parse_primary()
        }
    }

    fn parse_mult(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_unary()?;

        while self.are_next(&[TokenKind::Star, TokenKind::Slash]) {
            let op = self.advance().unwrap();
            let right = self.parse_unary()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_mult()?;

        while self.are_next(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let right = self.parse_mult()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_addition()?;

        while self.are_next(&[
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

        while self.are_next(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
            let op = self.advance().unwrap();
            let right = self.parse_comparison()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_assignment(&mut self) -> Result<LocExpr, ()> {
        let l_expr = self.parse_equality()?;

        if self.consume(&TokenKind::Equal) {
            match l_expr.expr {
                Expr::Var(var) => {
                    let r_expr = self.parse_expr()?;
                    Ok(LocExpr::new_assign(l_expr.loc, var, r_expr))
                },
                _ => error(l_expr.loc, "Invalid l-value"),
            }
        } else {
            Ok(l_expr)
        }
    }

    fn parse_expr(&mut self) -> Result<LocExpr, ()> {
        self.parse_assignment()
    }

    fn expect_semicolon(&mut self) -> Result<(), ()> {
        if self.is_next(&TokenKind::Semicolon) {
            self.advance();
            Ok(())
        } else {
            error(
                self.loc_after_prev(),
                "Expected semicolon after the statement",
            )
        }
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ()> {
        let expr = self.parse_expr()?;
        self.expect_semicolon()?;

        Ok(Stmt::Expr(expr))
    }

    fn parse_print_stmt(&mut self) -> Result<Stmt, ()> {
        self.expect(&TokenKind::Print)?;
        let expr = self.parse_expr()?;
        self.expect_semicolon()?;

        Ok(Stmt::Print(expr))
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, ()> {
        let mut stmts = Vec::new();

        self.expect(&TokenKind::LeftBrace)?;
        while !self.is_done() && !self.is_next(&TokenKind::RightBrace) {
            stmts.push(self.parse_decl()?);
        }

        if self.consume(&TokenKind::RightBrace) {
            Ok(stmts)
        } else {
            error(self.loc_after_prev(), "Unclosed '{', expected '}'")
        }
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ()> {
        assert!(!self.is_done());
        match self.tokens[self.index].kind {
            TokenKind::Print => self.parse_print_stmt(),
            TokenKind::LeftBrace => Ok(Stmt::Block(self.parse_block()?)),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, ()> {
        let mut init = LocExpr::new_literal((0, 0), Value::Null(()));

        let var = self.expect(&TokenKind::Identifier)?;
        if self.consume(&TokenKind::Equal) {
            init = self.parse_expr()?;
        }
        self.expect_semicolon()?;

        Ok(Stmt::VarDecl(var, init))
    }

    fn parse_decl(&mut self) -> Result<Stmt, ()> {
        if self.consume(&TokenKind::Var) {
            self.parse_var_decl()
        } else {
            self.parse_stmt()
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, ()> {
    if tokens.is_empty() {
        return Ok(Vec::new());
    }

    let mut parser = Parser::new(tokens);
    let mut stmts: Vec<Stmt> = Vec::new();
    let mut panic_mode = false;

    while !parser.is_done() {
        if let Ok(stmt) = parser.parse_decl() {
            stmts.push(stmt);
        } else {
            parser.sync();
            panic_mode = true;
        }
    }

    if !panic_mode {
        Ok(stmts)
    } else {
        Err(())
    }
}
