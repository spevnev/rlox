use std::{cell::Cell, rc::Rc};

use crate::{
    error::{error, print_error, Loc},
    lexer::{Token, TokenKind, Value},
};

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

    fn to_identifier(&self) -> Result<String, ()> {
        match &self.value {
            Value::Identifier(id) => Ok(id.clone()),
            _ => self.type_expected_error("identifier"),
        }
    }
}

pub struct Unary {
    pub op: TokenKind,
    pub expr: Box<LocExpr>,
}

pub struct Binary {
    pub left: Box<LocExpr>,
    pub op: TokenKind,
    pub right: Box<LocExpr>,
}

/// Specifies in which scope the variable that `Var` references is defined.
#[derive(Clone, Copy)]
pub enum VarScope {
    Global,
    /// Number of scopes to go outwards from the current one.
    Relative(i32),
}

pub struct Var {
    pub name: String,
    pub scope: Cell<VarScope>,
}

impl Var {
    pub fn new(name: String) -> Self {
        Self {
            name,
            scope: Cell::new(VarScope::Global),
        }
    }
}

pub struct Assign {
    pub var: Var,
    pub expr: Box<LocExpr>,
}

pub struct Call {
    pub callee: Box<LocExpr>,
    pub args: Vec<LocExpr>,
}

pub struct Get {
    pub object: Box<LocExpr>,
    pub property: String,
}

pub struct Set {
    pub object: Box<LocExpr>,
    pub property: String,
    pub expr: Box<LocExpr>,
}

pub enum Expr {
    Literal(Value),
    Unary(Unary),
    Binary(Binary),
    Logical(Binary),
    Variable(Var),
    Assign(Assign),
    Call(Call),
    Get(Get),
    Set(Set),
}

pub struct LocExpr {
    pub loc: Loc,
    pub expr: Expr,
}

impl LocExpr {
    fn new_literal(loc: Loc, value: Value) -> Self {
        Self {
            loc,
            expr: Expr::Literal(value),
        }
    }

    fn new_unary(op: Token, expr: Self) -> Self {
        Self {
            loc: op.loc,
            expr: Expr::Unary(Unary {
                op: op.kind,
                expr: Box::new(expr),
            }),
        }
    }

    fn new_binary(left: Self, op: Token, right: Self) -> Self {
        Self {
            loc: left.loc,
            expr: Expr::Binary(Binary {
                left: Box::new(left),
                op: op.kind,
                right: Box::new(right),
            }),
        }
    }

    fn new_logical(left: Self, op: Token, right: Self) -> Self {
        Self {
            loc: left.loc,
            expr: Expr::Logical(Binary {
                left: Box::new(left),
                op: op.kind,
                right: Box::new(right),
            }),
        }
    }

    fn new_var(loc: Loc, name: String) -> Self {
        Self {
            loc,
            expr: Expr::Variable(Var::new(name)),
        }
    }

    fn new_assign(loc: Loc, name: String, expr: Self) -> Self {
        Self {
            loc,
            expr: Expr::Assign(Assign {
                var: Var::new(name),
                expr: Box::new(expr),
            }),
        }
    }

    fn new_call(callee: Self, args: Vec<Self>) -> Self {
        Self {
            loc: callee.loc,
            expr: Expr::Call(Call {
                callee: Box::new(callee),
                args,
            }),
        }
    }

    fn new_get(object: Self, property: Token) -> Self {
        Self {
            loc: property.loc,
            expr: Expr::Get(Get {
                object: Box::new(object),
                property: property.to_identifier().unwrap(),
            }),
        }
    }

    fn new_set(get: Get, expr: Self) -> Self {
        Self {
            loc: get.object.loc,
            expr: Expr::Set(Set {
                object: get.object,
                property: get.property,
                expr: Box::new(expr),
            }),
        }
    }
}

pub struct LoxFunParam {
    pub loc: Loc,
    pub name: String,
}

pub struct LoxFunDecl {
    pub params: Vec<LoxFunParam>,
    pub body: Vec<Stmt>,
}

pub struct If {
    pub condition: LocExpr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

pub struct While {
    pub condition: LocExpr,
    pub body: Box<Stmt>,
}

pub struct VarDecl {
    pub name_loc: Loc,
    pub name: String,
    pub init: LocExpr,
}

pub struct FunDecl {
    pub name_loc: Loc,
    pub name: String,
    pub decl: Rc<LoxFunDecl>,
}

pub struct ClassDecl {
    pub name_loc: Loc,
    pub name: String,
    pub methods: Vec<FunDecl>,
}

pub enum Stmt {
    Expr(LocExpr),
    Block(Vec<Stmt>),
    If(If),
    While(While),
    VarDecl(VarDecl),
    FunDecl(FunDecl),
    Return(LocExpr),
    ClassDecl(Rc<ClassDecl>), // TODO: why Rc?
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    eof_loc: Loc,
    had_error: bool,
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
            had_error: false,
        }
    }

    /// Returns location immediately after the previous token.
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

    fn is_next(&mut self, kind: TokenKind) -> bool {
        !self.is_done() && self.tokens[self.index].kind == kind
    }

    fn is_next_many(&mut self, kinds: &[TokenKind]) -> bool {
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

    /// Un-advances to the previous token.
    fn back(&mut self) {
        assert!(self.index > 0, "Previous token must exist");
        self.index -= 1;
    }

    /// Advances if the next token is of kind `kind`.
    fn try_consume(&mut self, kind: TokenKind) -> bool {
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

    /// Advances if the next token is of kind `kind` or returns error.
    fn consume(&mut self, kind: TokenKind) -> Result<Token, ()> {
        if self.is_done() {
            return error(
                self.eof_loc,
                &format!("Expected {} but reached the end", kind.to_string()),
            );
        }

        if self.is_next(kind) {
            Ok(self.advance().unwrap())
        } else {
            let cur = &self.tokens[self.index];
            error(
                cur.loc,
                &format!(
                    "Expected {} but found '{}'",
                    kind.to_string(),
                    cur.value.convert_to_string(true)
                ),
            )
        }
    }

    /// Advances and returns error with `error_message` if the next token is not of kind `kind`.
    /// Intended for `(`, `)`, `;` since it reports at the location right after the previous token.
    fn expect(&mut self, kind: TokenKind, error_message: &str) -> Result<(), ()> {
        if self.try_consume(kind) {
            Ok(())
        } else {
            error(self.loc_after_prev(), error_message)
        }
    }

    /// Discards all the tokens until the next statement before entering panic mode.
    fn sync(&mut self) {
        self.had_error = true;
        while !self.is_done()
            && !self.try_consume(TokenKind::Semicolon)
            && !self.is_next_many(&[
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
                self.expect(TokenKind::RightParen, "Unclosed '(', expected ')'")?;
                Ok(expr)
            },
            _ => error(token.loc, "Expected expression"),
        }
    }

    fn parse_args(&mut self) -> Result<Vec<LocExpr>, ()> {
        let mut args = Vec::new();

        if self.try_consume(TokenKind::RightParen) {
            return Ok(args);
        }

        args.push(self.parse_expr()?);
        while args.len() < Self::MAX_ARGS && self.try_consume(TokenKind::Comma) {
            args.push(self.parse_expr()?);
        }
        if self.is_next(TokenKind::Comma) {
            self.had_error = true;
            print_error(
                self.tokens[self.index + 1].loc,
                &format!("Max number of arguments is {}", Self::MAX_ARGS),
            );
        }
        self.expect(TokenKind::RightParen, "Unclosed '(', expected ')' after the arguments")?;

        Ok(args)
    }

    fn parse_call(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_primary()?;

        while !self.is_done() {
            match self.advance().unwrap().kind {
                TokenKind::LeftParen => {
                    expr = LocExpr::new_call(expr, self.parse_args()?);
                },
                TokenKind::Dot => {
                    let property = self.consume(TokenKind::Identifier)?;
                    expr = LocExpr::new_get(expr, property);
                },
                _ => {
                    self.back();
                    break;
                },
            }
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<LocExpr, ()> {
        if self.is_next_many(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let expr = self.parse_unary()?;
            Ok(LocExpr::new_unary(op, expr))
        } else {
            self.parse_call()
        }
    }

    fn parse_mult(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_unary()?;

        while self.is_next_many(&[TokenKind::Star, TokenKind::Slash]) {
            let op = self.advance().unwrap();
            let right = self.parse_unary()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_mult()?;

        while self.is_next_many(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let right = self.parse_mult()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_addition()?;

        while self.is_next_many(&[
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

        while self.is_next_many(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
            let op = self.advance().unwrap();
            let right = self.parse_comparison()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_logic_and(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_equality()?;

        while self.is_next(TokenKind::AndAnd) {
            let op = self.advance().unwrap();
            let right = self.parse_equality()?;
            expr = LocExpr::new_logical(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_logic_or(&mut self) -> Result<LocExpr, ()> {
        let mut expr = self.parse_logic_and()?;

        while self.is_next(TokenKind::PipePipe) {
            let op = self.advance().unwrap();
            let right = self.parse_logic_and()?;
            expr = LocExpr::new_logical(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_assignment(&mut self) -> Result<LocExpr, ()> {
        let l_expr = self.parse_logic_or()?;

        if self.try_consume(TokenKind::Equal) {
            match l_expr.expr {
                Expr::Variable(var) => {
                    let r_expr = self.parse_expr()?;
                    Ok(LocExpr::new_assign(l_expr.loc, var.name, r_expr))
                },
                Expr::Get(get) => {
                    let r_expr = self.parse_expr()?;
                    Ok(LocExpr::new_set(get, r_expr))
                },
                _ => {
                    // Parser is still in a valid state that doesn't require syncing.
                    // Instead of returning `Err`, that triggers `sync()`, we print the error,
                    // set `had_error` and return `null` value since it won't be used anyways.
                    self.had_error = true;
                    print_error(l_expr.loc, "Invalid l-value");
                    Ok(LocExpr {
                        loc: Loc::none(),
                        expr: Expr::Literal(Value::Null),
                    })
                },
            }
        } else {
            Ok(l_expr)
        }
    }

    fn parse_expr(&mut self) -> Result<LocExpr, ()> {
        self.parse_assignment()
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ()> {
        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semicolon, "Expected semicolon after the statement")?;

        Ok(Stmt::Expr(expr))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ()> {
        self.expect(TokenKind::LeftParen, "Expected '(' after 'if'")?;
        let condition = self.parse_expr()?;
        self.expect(TokenKind::RightParen, "Unclosed '(', expected ')' after the condition")?;

        let then_branch = Box::new(self.parse_stmt()?);
        let else_branch = if self.try_consume(TokenKind::Else) {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };

        Ok(Stmt::If(If {
            condition,
            then_branch,
            else_branch,
        }))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ()> {
        self.expect(TokenKind::LeftParen, "Expected '(' after 'while'")?;
        let condition = self.parse_expr()?;
        self.expect(TokenKind::RightParen, "Unclosed '(', expected ')' after the condition")?;

        let body = Box::new(self.parse_stmt()?);

        Ok(Stmt::While(While { condition, body }))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ()> {
        self.expect(TokenKind::LeftParen, "Expected '(' after 'for'")?;

        let initializer;
        if self.try_consume(TokenKind::Semicolon) {
            initializer = None;
        } else if self.try_consume(TokenKind::Var) {
            initializer = Some(self.parse_var_decl()?);
        } else {
            initializer = Some(self.parse_expr_stmt()?);
        }

        let condition;
        if self.try_consume(TokenKind::Semicolon) {
            // `for` without condition is infinite, so use `while (true)`.
            condition = LocExpr::new_literal(Loc::none(), Value::Bool(true));
        } else {
            condition = self.parse_expr()?;
            self.consume(TokenKind::Semicolon)?;
        }

        let update;
        if self.try_consume(TokenKind::RightParen) {
            update = None;
        } else {
            update = Some(Stmt::Expr(self.parse_expr()?));
            self.expect(TokenKind::RightParen, "Unclosed '(', expected ')'")?;
        }

        let body = self.parse_stmt()?;

        // Instead of adding a new statement kind, `for` is transformed into `while`.
        let while_body = match update {
            Some(update) => Stmt::Block(vec![body, update]),
            None => body,
        };
        let while_loop = Stmt::While(While {
            condition,
            body: Box::new(while_body),
        });

        match initializer {
            Some(initializer) => Ok(Stmt::Block(vec![initializer, while_loop])),
            None => Ok(while_loop),
        }
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ()> {
        let value;
        if self.try_consume(TokenKind::Semicolon) {
            value = LocExpr::new_literal(self.tokens[self.index - 1].loc, Value::Null);
        } else {
            value = self.parse_expr()?;
            self.expect(TokenKind::Semicolon, "Expected semicolon after the return statement")?;
        }

        Ok(Stmt::Return(value))
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, ()> {
        let mut stmts = Vec::new();

        while !self.is_done() && !self.is_next(TokenKind::RightBrace) {
            if let Some(stmt) = self.parse_decl() {
                stmts.push(stmt);
            }
        }
        self.expect(TokenKind::RightBrace, "Unclosed '{', expected '}'")?;

        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ()> {
        match self.advance().unwrap().kind {
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::LeftBrace => Ok(Stmt::Block(self.parse_block()?)),
            _ => {
                self.back();
                self.parse_expr_stmt()
            },
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, ()> {
        let mut init = LocExpr::new_literal(Loc::none(), Value::Null);

        let name_token = self.consume(TokenKind::Identifier)?;
        if self.try_consume(TokenKind::Equal) {
            init = self.parse_expr()?;
        }
        self.expect(
            TokenKind::Semicolon,
            "Expected semicolon after the variable declaration",
        )?;

        Ok(Stmt::VarDecl(VarDecl {
            name_loc: name_token.loc,
            name: name_token.to_identifier().unwrap(),
            init,
        }))
    }

    fn parse_param(&mut self) -> Result<LoxFunParam, ()> {
        let param_token = self.consume(TokenKind::Identifier)?;
        Ok(LoxFunParam {
            loc: param_token.loc,
            name: param_token.to_identifier().unwrap(),
        })
    }

    fn parse_params(&mut self) -> Result<Vec<LoxFunParam>, ()> {
        let mut params = Vec::new();

        if self.try_consume(TokenKind::RightParen) {
            return Ok(params);
        }

        params.push(self.parse_param()?);
        while params.len() < Self::MAX_ARGS && self.try_consume(TokenKind::Comma) {
            params.push(self.parse_param()?);
        }
        if self.is_next(TokenKind::Comma) {
            self.had_error = true;
            print_error(
                self.tokens[self.index + 1].loc,
                &format!("Max number of parameters is {}", Self::MAX_ARGS),
            );
        }
        self.expect(TokenKind::RightParen, "Unclosed '(', expected ')' after the parameters")?;

        Ok(params)
    }

    fn parse_fun(&mut self) -> Result<FunDecl, ()> {
        let name_token = self.consume(TokenKind::Identifier)?;

        self.expect(TokenKind::LeftParen, "Expected '(' after function name")?;
        let params = self.parse_params()?;

        self.expect(TokenKind::LeftBrace, "Expected '{' before function body")?;
        let body = self.parse_block()?;

        Ok(FunDecl {
            name_loc: name_token.loc,
            name: name_token.to_identifier().unwrap(),
            decl: Rc::new(LoxFunDecl { params, body }),
        })
    }

    fn parse_fun_decl(&mut self) -> Result<Stmt, ()> {
        Ok(Stmt::FunDecl(self.parse_fun()?))
    }

    fn parse_class_decl(&mut self) -> Result<Stmt, ()> {
        let name_token = self.consume(TokenKind::Identifier)?;

        self.expect(TokenKind::LeftBrace, "Expected '{' before class body")?;
        let mut methods = Vec::new();
        while !self.is_done() && !self.is_next(TokenKind::RightBrace) {
            methods.push(self.parse_fun()?);
        }
        self.expect(TokenKind::RightBrace, "Unclosed '{', expected '}' after class body")?;

        Ok(Stmt::ClassDecl(Rc::new(ClassDecl {
            name_loc: name_token.loc,
            name: name_token.to_identifier().unwrap(),
            methods,
        })))
    }

    fn parse_decl(&mut self) -> Option<Stmt> {
        let result = match self.advance().unwrap().kind {
            TokenKind::Var => self.parse_var_decl(),
            TokenKind::Fun => self.parse_fun_decl(),
            TokenKind::Class => self.parse_class_decl(),
            _ => {
                self.back();
                self.parse_stmt()
            },
        };

        match result {
            Ok(value) => Some(value),
            Err(_) => {
                self.sync();
                None
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

    while !parser.is_done() {
        if let Some(stmt) = parser.parse_decl() {
            stmts.push(stmt);
        }
    }

    if parser.had_error {
        Err(())
    } else {
        Ok(stmts)
    }
}
