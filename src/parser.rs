use crate::{
    error::{error, Loc},
    lexer::{Token, TokenKind, Value},
};

#[derive(PartialEq, Clone)]
pub struct Unary {
    pub op: TokenKind,
    pub expr: Box<LocExpr>,
}

#[derive(PartialEq, Clone)]
pub struct Binary {
    pub left: Box<LocExpr>,
    pub op: TokenKind,
    pub right: Box<LocExpr>,
}

#[derive(PartialEq, Clone)]
pub struct Assign {
    pub var: String,
    pub expr: Box<LocExpr>,
}

#[derive(PartialEq, Clone)]
pub struct Call {
    pub callee: Box<LocExpr>,
    pub args: Vec<LocExpr>,
}

#[derive(PartialEq, Clone)]
pub enum Expr {
    Literal(Value),
    Unary(Unary),
    Binary(Binary),
    Logical(Binary),
    Var(String),
    Assign(Assign),
    Call(Call),
}

#[derive(PartialEq, Clone)]
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

    fn new_logical(left: LocExpr, op: Token, right: LocExpr) -> LocExpr {
        LocExpr {
            loc: left.loc,
            expr: Expr::Logical(Binary {
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

    fn new_call(callee: LocExpr, args: Vec<LocExpr>) -> LocExpr {
        LocExpr {
            loc: callee.loc,
            expr: Expr::Call(Call {
                callee: Box::new(callee),
                args,
            }),
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum Stmt {
    Expr(LocExpr),
    Block(Vec<Stmt>),
    If(LocExpr, Box<Stmt>, Option<Box<Stmt>>), // condition, then, else
    While(LocExpr, Box<Stmt>),                 // condition, body
    VarDecl(Token, LocExpr),                   // name, init
    FunDecl(Token, Vec<Token>, Vec<Stmt>),     // name, params, body
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    eof_loc: Loc,
}

impl Parser {
    const MAX_ARGS: usize = 255;

    fn new(tokens: Vec<Token>) -> Self {
        assert!(tokens.len() > 0, "Tokens must not be empty");
        let prev = &tokens[tokens.len() - 1];

        let mut eof_loc = prev.loc;
        eof_loc.column += prev.len;

        Self {
            eof_loc,
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
            let prev = &self.tokens[self.index - 1];

            let mut loc = prev.loc;
            loc.column += prev.len;

            loc
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

    // Un-advances to the previous token
    fn back(&mut self) {
        assert!(self.index > 0, "Previous token must exist");
        self.index -= 1;
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
                &format!(
                    "Expected {} but found '{}'",
                    kind.to_string(),
                    self.tokens[self.index].value.convert_to_string(true)
                ),
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
                TokenKind::Return,
                TokenKind::Var,
                TokenKind::While,
                TokenKind::LeftBrace,
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
            TokenKind::Number | TokenKind::String => Ok(LocExpr::new_literal(token.loc, token.value)),
            TokenKind::False => Ok(LocExpr::new_literal(token.loc, Value::Bool(false))),
            TokenKind::True => Ok(LocExpr::new_literal(token.loc, Value::Bool(true))),
            TokenKind::Null => Ok(LocExpr::new_literal(token.loc, Value::Null)),
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

    fn parse_args(&mut self) -> Result<Vec<LocExpr>, ()> {
        let mut args = Vec::new();

        if self.consume(&TokenKind::RightParen) {
            return Ok(args);
        }

        args.push(self.parse_expr()?);
        while args.len() < Self::MAX_ARGS && self.consume(&TokenKind::Comma) {
            args.push(self.parse_expr()?);
        }
        if self.is_next(&TokenKind::Comma) {
            return error(
                self.tokens[self.index + 1].loc,
                &format!("Max number of arguments is {}", Self::MAX_ARGS),
            );
        }
        if !self.consume(&TokenKind::RightParen) {
            return error(self.loc_after_prev(), "Unclosed '(', expected ')' after the arguments");
        }

        Ok(args)
    }

    fn parse_call(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_primary()?;

        while self.consume(&TokenKind::LeftParen) {
            expr = LocExpr::new_call(expr, self.parse_args()?);
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<LocExpr, ()> {
        if self.are_next(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let expr = self.parse_unary()?;
            Ok(LocExpr::new_unary(op, expr))
        } else {
            self.parse_call()
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

    fn parse_logic_and(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_equality()?;

        while self.is_next(&TokenKind::AndAnd) {
            let op = self.advance().unwrap();
            let right = self.parse_equality()?;
            expr = LocExpr::new_logical(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_logic_or(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_logic_and()?;

        while self.is_next(&TokenKind::PipePipe) {
            let op = self.advance().unwrap();
            let right = self.parse_logic_and()?;
            expr = LocExpr::new_logical(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_assignment(&mut self) -> Result<LocExpr, ()> {
        let l_expr = self.parse_logic_or()?;

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
            error(self.loc_after_prev(), "Expected semicolon after the statement")
        }
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ()> {
        let expr = self.parse_expr()?;
        self.expect_semicolon()?;

        Ok(Stmt::Expr(expr))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ()> {
        if !self.consume(&TokenKind::LeftParen) {
            return error(self.loc_after_prev(), "Expected '(' after 'if'");
        }
        let condition = self.parse_expr()?;
        if !self.consume(&TokenKind::RightParen) {
            return error(self.loc_after_prev(), "Unclosed '(', expected ')' after condition");
        }

        let then_branch = self.parse_stmt()?;
        let else_branch = if self.consume(&TokenKind::Else) {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };

        Ok(Stmt::If(condition, Box::new(then_branch), else_branch))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ()> {
        if !self.consume(&TokenKind::LeftParen) {
            return error(self.loc_after_prev(), "Expected '(' after 'while'");
        }
        let condition = self.parse_expr()?;
        if !self.consume(&TokenKind::RightParen) {
            return error(self.loc_after_prev(), "Unclosed '(', expected ')' after condition");
        }

        let body = self.parse_stmt()?;
        Ok(Stmt::While(condition, Box::new(body)))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ()> {
        if !self.consume(&TokenKind::LeftParen) {
            return error(self.loc_after_prev(), "Expected '(' after 'for'");
        }

        let initializer;
        if self.consume(&TokenKind::Semicolon) {
            initializer = None;
        } else if self.consume(&TokenKind::Var) {
            initializer = Some(self.parse_var_decl()?);
        } else {
            initializer = Some(self.parse_expr_stmt()?);
        }

        let condition;
        if self.consume(&TokenKind::Semicolon) {
            // `for` without condition is infinite, so use `while (true)`.
            condition = LocExpr::new_literal(Loc::none(), Value::Bool(true));
        } else {
            condition = self.parse_expr()?;
            self.expect(&TokenKind::Semicolon)?;
        }

        let update;
        if self.consume(&TokenKind::RightParen) {
            update = None;
        } else {
            update = Some(Stmt::Expr(self.parse_expr()?));
            if !self.consume(&TokenKind::RightParen) {
                return error(self.loc_after_prev(), "Unclosed '(', expected ')'");
            }
        }

        let body = self.parse_stmt()?;

        // Instead of adding a new statement kind, `for` is transformed into `while`.
        let while_body = if update.is_some() {
            Stmt::Block(vec![body, update.unwrap()])
        } else {
            body
        };
        let while_loop = Stmt::While(condition, Box::new(while_body));

        if initializer.is_some() {
            Ok(Stmt::Block(vec![initializer.unwrap(), while_loop]))
        } else {
            Ok(while_loop)
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, ()> {
        let mut stmts = Vec::new();

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
        match self.advance().unwrap().kind {
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::LeftBrace => Ok(Stmt::Block(self.parse_block()?)),
            _ => {
                self.back();
                self.parse_expr_stmt()
            },
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, ()> {
        let mut init = LocExpr::new_literal(Loc::none(), Value::Null);

        let name = self.expect(&TokenKind::Identifier)?;
        if self.consume(&TokenKind::Equal) {
            init = self.parse_expr()?;
        }
        self.expect_semicolon()?;

        Ok(Stmt::VarDecl(name, init))
    }

    fn parse_params(&mut self) -> Result<Vec<Token>, ()> {
        let mut params = Vec::new();

        if self.consume(&TokenKind::RightParen) {
            return Ok(params);
        }

        params.push(self.expect(&TokenKind::Identifier)?);
        while params.len() < Self::MAX_ARGS && self.consume(&TokenKind::Comma) {
            params.push(self.expect(&TokenKind::Identifier)?);
        }
        if self.is_next(&TokenKind::Comma) {
            return error(
                self.tokens[self.index + 1].loc,
                &format!("Max number of parameters is {}", Self::MAX_ARGS),
            );
        }
        if !self.consume(&TokenKind::RightParen) {
            return error(self.loc_after_prev(), "Unclosed '(', expected ')' after the parameters");
        }

        Ok(params)
    }

    fn parse_fun_decl(&mut self) -> Result<Stmt, ()> {
        let name = self.expect(&TokenKind::Identifier)?;
        if !self.consume(&TokenKind::LeftParen) {
            return error(self.loc_after_prev(), "Expected '(' after 'fun'");
        }
        let params = self.parse_params()?;
        self.expect(&TokenKind::LeftBrace)?;
        let body = self.parse_block()?;

        Ok(Stmt::FunDecl(name, params, body))
    }

    fn parse_decl(&mut self) -> Result<Stmt, ()> {
        match self.advance().unwrap().kind {
            TokenKind::Var => self.parse_var_decl(),
            TokenKind::Fun => self.parse_fun_decl(),
            _ => {
                self.back();
                self.parse_stmt()
            },
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
