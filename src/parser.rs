use std::{cell::Cell, rc::Rc};

use crate::{
    error::{format_error, Loc},
    lexer::{Token, TokenKind},
    value::Value,
};

type Result<V, E = ()> = std::result::Result<V, E>;

pub struct Unary {
    pub op: TokenKind,
    pub expr: Box<LocExpr>,
}

pub struct Binary {
    pub left: Box<LocExpr>,
    pub op: TokenKind,
    pub right: Box<LocExpr>,
}

pub struct Ternary {
    pub condition: Box<LocExpr>,
    pub then_expr: Box<LocExpr>,
    pub else_expr: Box<LocExpr>,
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

pub struct GetProp {
    pub instance: Box<LocExpr>,
    pub property: String,
}

pub struct SetProp {
    pub instance: Box<LocExpr>,
    pub property: String,
    pub expr: Box<LocExpr>,
}

pub struct Super {
    pub var: Var,
    pub method: String,
}

pub struct Lambda {
    pub params: Vec<FunParam>,
    pub body: Rc<Vec<Stmt>>,
}

pub enum Expr {
    Literal(Value),
    Grouping(Box<LocExpr>),
    Unary(Unary),
    Binary(Binary),
    Ternary(Ternary),
    Logical(Binary),
    Variable(Var),
    Assign(Assign),
    Call(Call),
    GetProp(GetProp),
    SetProp(SetProp),
    This(Var),
    Super(Super),
    Lambda(Lambda),
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

    fn new_grouping(loc: Loc, expr: Self) -> Self {
        Self {
            loc,
            expr: Expr::Grouping(Box::new(expr)),
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

    fn new_ternary(condition: Self, then_expr: Self, else_expr: Self) -> Self {
        Self {
            loc: condition.loc,
            expr: Expr::Ternary(Ternary {
                condition: Box::new(condition),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
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

    fn new_get(loc: Loc, instance: Self, property: String) -> Self {
        Self {
            loc,
            expr: Expr::GetProp(GetProp {
                instance: Box::new(instance),
                property,
            }),
        }
    }

    fn new_set(get: GetProp, expr: Self) -> Self {
        Self {
            loc: get.instance.loc,
            expr: Expr::SetProp(SetProp {
                instance: get.instance,
                property: get.property,
                expr: Box::new(expr),
            }),
        }
    }

    fn new_this(loc: Loc) -> Self {
        Self {
            loc,
            expr: Expr::This(Var::new("this".to_owned())),
        }
    }

    fn new_super(loc: Loc, method: String) -> Self {
        Self {
            loc,
            expr: Expr::Super(Super {
                var: Var::new("super".to_owned()),
                method,
            }),
        }
    }

    fn new_lambda(loc: Loc, params: Vec<FunParam>, body: Rc<Vec<Stmt>>) -> Self {
        Self {
            loc,
            expr: Expr::Lambda(Lambda { params, body }),
        }
    }
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

#[derive(Clone)]
pub struct FunParam {
    pub loc: Loc,
    pub name: String,
}

pub struct FunDecl {
    pub name_loc: Loc,
    pub name: String,
    pub params: Vec<FunParam>,
    pub body: Rc<Vec<Stmt>>,
    pub is_getter: bool,
}

pub struct Return {
    pub loc: Loc,
    pub expr: Option<LocExpr>,
}

pub struct Superclass {
    pub loc: Loc,
    pub var: Var,
}

pub struct ClassDecl {
    pub name_loc: Loc,
    pub name: String,
    pub methods: Vec<FunDecl>,
    pub static_methods: Vec<FunDecl>,
    pub superclass: Option<Superclass>,
}

pub enum Stmt {
    Expr(LocExpr),
    Print(LocExpr),
    Block(Vec<Stmt>),
    If(If),
    While(While),
    Break(Loc),
    VarDecl(VarDecl),
    FunDecl(FunDecl),
    Return(Return),
    ClassDecl(ClassDecl),
}

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    index: usize,
    errors: Vec<String>,
    last_loc: Loc,
}

impl<'a> Parser<'a> {
    const MAX_ARGS: usize = 255;

    fn new(tokens: &'a Vec<Token>) -> Self {
        assert!(tokens.len() > 0, "Tokens must not be empty");
        let last_loc = tokens[tokens.len() - 1].end_loc();

        Self {
            tokens,
            index: 0,
            errors: Vec::new(),
            last_loc,
        }
    }

    fn loc(&self) -> Loc {
        if self.is_done() {
            self.last_loc
        } else {
            self.tokens[self.index].loc
        }
    }

    /// Returns location immediately after the previous token.
    fn loc_after_prev(&self) -> Loc {
        if self.is_done() {
            self.last_loc
        } else if self.index == 0 {
            Loc { line: 0, column: 0 }
        } else {
            self.tokens[self.index - 1].end_loc()
        }
    }

    fn is_done(&self) -> bool {
        self.index >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        if self.is_done() {
            None
        } else {
            Some(&self.tokens[self.index])
        }
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

    /// Advances if the next token is of kind `kind` or returns `None`.
    fn consume(&mut self, kind: TokenKind) -> Option<Token> {
        if self.is_next(kind) {
            Some(self.advance().unwrap())
        } else {
            None
        }
    }

    /// Advances and panics if the next token is not of kind `kind`.
    fn expect(&mut self, kind: TokenKind) -> Token {
        if self.is_next(kind) {
            self.advance().unwrap()
        } else if self.is_done() {
            panic!("Expected {} but reached the end", kind.to_string());
        } else {
            panic!(
                "Expected {} but found '{}'",
                kind.to_string(),
                self.tokens[self.index].value.error_to_string()
            );
        }
    }

    /// Discards all the tokens until the next statement.
    fn sync(&mut self) {
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

    /// Saves an error to `errors`. They cannot be printed right away because
    /// they have to be silenced in REPL (when parsing as expression).
    fn error(&mut self, loc: Loc, message: &str) {
        self.errors.push(format_error(loc, message));
    }

    fn parse_primary(&mut self) -> Result<LocExpr> {
        let Some(token) = self.advance() else {
            self.error(self.last_loc, "Expected an expression but reached the end");
            return Err(());
        };

        match token.kind {
            TokenKind::Number | TokenKind::String => Ok(LocExpr::new_literal(token.loc, token.value)),
            TokenKind::False => Ok(LocExpr::new_literal(token.loc, Value::Bool(false))),
            TokenKind::True => Ok(LocExpr::new_literal(token.loc, Value::Bool(true))),
            TokenKind::Nil => Ok(LocExpr::new_literal(token.loc, Value::Nil)),
            TokenKind::Identifier => Ok(LocExpr::new_var(token.loc, token.to_identifier())),
            TokenKind::This => Ok(LocExpr::new_this(token.loc)),
            TokenKind::Super => {
                self.consume(TokenKind::Dot).ok_or_else(|| {
                    self.error(
                        self.loc(),
                        "Expected '.' after 'super', 'super' can only be used to access methods",
                    )
                })?;
                let method_token = self.consume(TokenKind::Identifier).ok_or_else(|| {
                    self.error(
                        self.loc(),
                        "Expected a method name after 'super', 'super' can only be used to access methods",
                    )
                })?;
                Ok(LocExpr::new_super(token.loc, method_token.to_identifier()))
            },
            TokenKind::LeftParen => {
                let expr = self.parse_expr()?;
                self.consume(TokenKind::RightParen)
                    .ok_or_else(|| self.error(self.loc_after_prev(), "Unclosed '(', expected ')'"))?;
                Ok(LocExpr::new_grouping(token.loc, expr))
            },
            TokenKind::Fun => {
                self.consume(TokenKind::LeftParen)
                    .ok_or_else(|| self.error(self.loc(), "Expected '(' after 'fun' in anonymous function"))?;
                let params = self.parse_params()?;

                if !self.is_next(TokenKind::LeftBrace) {
                    self.error(self.loc_after_prev(), "Expected '{' before function body");
                    return Err(());
                }
                let body = Rc::new(self.parse_block()?);

                Ok(LocExpr::new_lambda(token.loc, params, body))
            },
            _ => {
                self.error(
                    token.loc,
                    &format!("Expected an expression but found '{}'", token.kind.to_string()),
                );
                Err(())
            },
        }
    }

    fn parse_args(&mut self) -> Result<Vec<LocExpr>> {
        let mut args = Vec::new();

        if self.try_consume(TokenKind::RightParen) {
            return Ok(args);
        }

        let mut error_loc = Loc::none();
        args.push(self.parse_expr()?);
        while self.try_consume(TokenKind::Comma) {
            if args.len() == Self::MAX_ARGS {
                error_loc = self.tokens[self.index - 1].loc;
            }
            args.push(self.parse_expr()?);
        }
        if args.len() > Self::MAX_ARGS {
            self.error(error_loc, &format!("The max number of arguments is {}", Self::MAX_ARGS));
        }
        self.consume(TokenKind::RightParen)
            .ok_or_else(|| self.error(self.loc_after_prev(), "Unclosed '(', expected ')' after the arguments"))?;

        Ok(args)
    }

    fn parse_call(&mut self) -> Result<LocExpr> {
        let mut expr = self.parse_primary()?;

        while !self.is_done() {
            match self.advance().unwrap().kind {
                TokenKind::LeftParen => {
                    let args = self.parse_args()?;
                    expr = LocExpr::new_call(expr, args);
                },
                TokenKind::Dot => {
                    let property = self
                        .consume(TokenKind::Identifier)
                        .ok_or_else(|| self.error(self.loc(), "Expected a property name after '.'"))?;
                    expr = LocExpr::new_get(property.loc, expr, property.to_identifier());
                },
                _ => {
                    self.back();
                    break;
                },
            }
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<LocExpr> {
        if self.is_next_many(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let expr = self.parse_unary()?;
            Ok(LocExpr::new_unary(op, expr))
        } else {
            self.parse_call()
        }
    }

    fn parse_mult(&mut self) -> Result<LocExpr> {
        let mut expr = self.parse_unary()?;

        while self.is_next_many(&[TokenKind::Star, TokenKind::Slash]) {
            let op = self.advance().unwrap();
            let right = self.parse_unary()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<LocExpr> {
        let mut expr = self.parse_mult()?;

        while self.is_next_many(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = self.advance().unwrap();
            let right = self.parse_mult()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<LocExpr> {
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

    fn parse_equality(&mut self) -> Result<LocExpr> {
        let mut expr = self.parse_comparison()?;

        while self.is_next_many(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
            let op = self.advance().unwrap();
            let right = self.parse_comparison()?;
            expr = LocExpr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_logic_and(&mut self) -> Result<LocExpr> {
        let mut expr = self.parse_equality()?;

        while self.is_next(TokenKind::And) {
            let op = self.advance().unwrap();
            let right = self.parse_equality()?;
            expr = LocExpr::new_logical(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_logic_or(&mut self) -> Result<LocExpr> {
        let mut expr = self.parse_logic_and()?;

        while self.is_next(TokenKind::Or) {
            let op = self.advance().unwrap();
            let right = self.parse_logic_and()?;
            expr = LocExpr::new_logical(expr, op, right);
        }

        Ok(expr)
    }

    fn parse_ternary(&mut self) -> Result<LocExpr> {
        let mut expr = self.parse_logic_or()?;

        if self.try_consume(TokenKind::Question) {
            let then_expr = self.parse_expr()?;
            self.consume(TokenKind::Colon)
                .ok_or_else(|| self.error(self.loc(), "Expected a colon after then branch of ternary operator"))?;
            let else_expr = self.parse_ternary()?;
            expr = LocExpr::new_ternary(expr, then_expr, else_expr);
        }

        Ok(expr)
    }

    fn parse_assignment(&mut self) -> Result<LocExpr> {
        let l_expr = self.parse_ternary()?;

        if !self.try_consume(TokenKind::Equal) {
            return Ok(l_expr);
        }

        let r_expr = self.parse_expr()?;
        match l_expr.expr {
            Expr::Variable(var) => Ok(LocExpr::new_assign(l_expr.loc, var.name, r_expr)),
            Expr::GetProp(get) => Ok(LocExpr::new_set(get, r_expr)),
            _ => {
                self.error(l_expr.loc, "Invalid assignment target");
                Ok(LocExpr {
                    loc: Loc::none(),
                    expr: Expr::Literal(Value::Nil),
                })
            },
        }
    }

    fn parse_expr(&mut self) -> Result<LocExpr> {
        self.parse_assignment()
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt> {
        let expr = self.parse_expr()?;
        self.consume(TokenKind::Semicolon)
            .ok_or_else(|| self.error(self.loc_after_prev(), "Expected a semicolon after the expression"))?;

        Ok(Stmt::Expr(expr))
    }

    fn parse_print_stmt(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::Print);
        let expr = self.parse_expr()?;
        self.consume(TokenKind::Semicolon)
            .ok_or_else(|| self.error(self.loc_after_prev(), "Expected a semicolon after the print statement"))?;

        Ok(Stmt::Print(expr))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::If);

        self.consume(TokenKind::LeftParen)
            .ok_or_else(|| self.error(self.loc(), "Expected '(' after 'if'"))?;
        let condition = self.parse_expr()?;
        self.consume(TokenKind::RightParen)
            .ok_or_else(|| self.error(self.loc_after_prev(), "Unclosed '(', expected ')' after the condition"))?;

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

    fn parse_while_stmt(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::While);
        self.consume(TokenKind::LeftParen)
            .ok_or_else(|| self.error(self.loc(), "Expected '(' after 'while'"))?;

        let condition = self.parse_expr()?;
        self.consume(TokenKind::RightParen)
            .ok_or_else(|| self.error(self.loc_after_prev(), "Unclosed '(', expected ')' after the condition"))?;

        let body = Box::new(self.parse_stmt()?);

        Ok(Stmt::While(While { condition, body }))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::For);
        self.consume(TokenKind::LeftParen)
            .ok_or_else(|| self.error(self.loc(), "Expected '(' after 'for'"))?;

        let initializer = if self.try_consume(TokenKind::Semicolon) {
            None
        } else if self.is_next(TokenKind::Var) {
            Some(self.parse_var_decl()?)
        } else {
            Some(self.parse_expr_stmt()?)
        };

        let condition;
        if self.try_consume(TokenKind::Semicolon) {
            // `for` without condition is infinite, so use `while (true)`.
            condition = LocExpr::new_literal(Loc::none(), Value::Bool(true));
        } else {
            condition = self.parse_expr()?;
            self.consume(TokenKind::Semicolon)
                .ok_or_else(|| self.error(self.loc_after_prev(), "Expected a semicolon after 'for' loop condition"))?;
        }

        let update;
        if self.try_consume(TokenKind::RightParen) {
            update = None;
        } else {
            update = Some(Stmt::Expr(self.parse_expr()?));
            self.consume(TokenKind::RightParen)
                .ok_or_else(|| self.error(self.loc_after_prev(), "Unclosed '(', expected ')'"))?;
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

    fn parse_return_stmt(&mut self) -> Result<Stmt> {
        let return_token = self.expect(TokenKind::Return);

        if self.try_consume(TokenKind::Semicolon) {
            Ok(Stmt::Return(Return {
                loc: return_token.loc,
                expr: None,
            }))
        } else {
            let expr = self.parse_expr()?;
            self.consume(TokenKind::Semicolon)
                .ok_or_else(|| self.error(self.loc(), "Expected a semicolon after the return statement"))?;

            Ok(Stmt::Return(Return {
                loc: return_token.loc,
                expr: Some(expr),
            }))
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = Vec::new();

        self.expect(TokenKind::LeftBrace);
        while !self.is_done() && !self.is_next(TokenKind::RightBrace) {
            if let Some(stmt) = self.parse_decl() {
                stmts.push(stmt);
            }
        }
        self.consume(TokenKind::RightBrace)
            .ok_or_else(|| self.error(self.loc_after_prev(), "Unclosed '{', expected '}'"))?;

        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        match self.peek().unwrap().kind {
            TokenKind::Print => self.parse_print_stmt(),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::Break => {
                let token = self.advance().unwrap();
                self.consume(TokenKind::Semicolon)
                    .ok_or_else(|| self.error(self.loc(), "Expected a semicolon after the break statement"))?;
                Ok(Stmt::Break(token.loc))
            },
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::LeftBrace => Ok(Stmt::Block(self.parse_block()?)),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::Var);
        let name_token = self
            .consume(TokenKind::Identifier)
            .ok_or_else(|| self.error(self.loc(), "Expected a variable name after 'var'"))?;

        let mut init = LocExpr::new_literal(Loc::none(), Value::Nil);
        if self.try_consume(TokenKind::Equal) {
            init = self.parse_expr()?;
        }
        self.consume(TokenKind::Semicolon)
            .ok_or_else(|| self.error(self.loc_after_prev(), "Expected a semicolon after the variable declaration"))?;

        Ok(Stmt::VarDecl(VarDecl {
            name_loc: name_token.loc,
            name: name_token.to_identifier(),
            init,
        }))
    }

    fn parse_param(&mut self) -> Result<FunParam> {
        let param_token = self
            .consume(TokenKind::Identifier)
            .ok_or_else(|| self.error(self.loc(), "Expected a parameter name in parameter list"))?;

        Ok(FunParam {
            loc: param_token.loc,
            name: param_token.to_identifier(),
        })
    }

    fn parse_params(&mut self) -> Result<Vec<FunParam>> {
        let mut params = Vec::new();

        if self.try_consume(TokenKind::RightParen) {
            return Ok(params);
        }

        let mut error_loc = Loc::none();
        params.push(self.parse_param()?);
        while self.try_consume(TokenKind::Comma) {
            if params.len() == Self::MAX_ARGS {
                error_loc = self.tokens[self.index - 1].loc;
            }
            params.push(self.parse_param()?);
        }
        if params.len() > Self::MAX_ARGS {
            self.error(error_loc, &format!("The max number of parameters is {}", Self::MAX_ARGS));
        }
        self.consume(TokenKind::RightParen)
            .ok_or_else(|| self.error(self.loc_after_prev(), "Unclosed '(', expected ')' after the parameters"))?;

        Ok(params)
    }

    fn parse_fun_decl(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::Fun);

        let name_token = self
            .consume(TokenKind::Identifier)
            .ok_or_else(|| self.error(self.loc(), "Expected a function name after 'fun'"))?;

        self.consume(TokenKind::LeftParen)
            .ok_or_else(|| self.error(self.loc(), "Expected '(' after function name"))?;
        let params = self.parse_params()?;

        if !self.is_next(TokenKind::LeftBrace) {
            self.error(self.loc_after_prev(), "Expected '{' before function body");
            return Err(());
        }
        let body = Rc::new(self.parse_block()?);

        Ok(Stmt::FunDecl(FunDecl {
            name_loc: name_token.loc,
            name: name_token.to_identifier(),
            params,
            body,
            is_getter: false,
        }))
    }

    fn parse_class_decl(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::Class);
        let name_token = self
            .consume(TokenKind::Identifier)
            .ok_or_else(|| self.error(self.loc(), "Expected a class name after 'class'"))?;

        let mut decl = ClassDecl {
            name_loc: name_token.loc,
            name: name_token.to_identifier(),
            superclass: None,
            methods: Vec::new(),
            static_methods: Vec::new(),
        };

        if self.try_consume(TokenKind::Less) {
            let token = self
                .consume(TokenKind::Identifier)
                .ok_or_else(|| self.error(self.loc(), "Expected a superclass name after '<'"))?;

            decl.superclass = Some(Superclass {
                loc: token.loc,
                var: Var::new(token.to_identifier()),
            });
        }

        self.consume(TokenKind::LeftBrace)
            .ok_or_else(|| self.error(self.loc(), "Expected '{' before class body"))?;

        while !self.is_done() && !self.is_next(TokenKind::RightBrace) {
            let is_static = self.try_consume(TokenKind::Class);

            let name_token = self
                .consume(TokenKind::Identifier)
                .ok_or_else(|| self.error(self.loc(), "Expected a method name in class body"))?;

            let params;
            let is_getter;
            if self.try_consume(TokenKind::LeftParen) {
                params = self.parse_params()?;
                is_getter = false;
            } else {
                params = Vec::new();
                is_getter = true;
            }

            if !self.is_next(TokenKind::LeftBrace) {
                self.error(self.loc_after_prev(), "Expected '{' before method body");
                return Err(());
            }
            let body = Rc::new(self.parse_block()?);

            let method = FunDecl {
                name_loc: name_token.loc,
                name: name_token.to_identifier(),
                params,
                body,
                is_getter,
            };

            if is_static {
                decl.static_methods.push(method);
            } else {
                decl.methods.push(method);
            }
        }

        self.consume(TokenKind::RightBrace)
            .ok_or_else(|| self.error(self.loc_after_prev(), "Unclosed '{', expected '}' after class body"))?;

        Ok(Stmt::ClassDecl(decl))
    }

    fn parse_decl(&mut self) -> Option<Stmt> {
        let result = match self.peek().unwrap().kind {
            TokenKind::Var => self.parse_var_decl(),
            TokenKind::Fun => {
                if self
                    .tokens
                    .get(self.index + 1)
                    .is_some_and(|token| token.kind != TokenKind::Identifier)
                {
                    // Handle anonymous function as an expr-stmt
                    self.parse_expr_stmt()
                } else {
                    self.parse_fun_decl()
                }
            },
            TokenKind::Class => self.parse_class_decl(),
            _ => self.parse_stmt(),
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

/// Returns a vector of statements or a string of all errors that occurred while parsing.
pub fn parse(tokens: &Vec<Token>) -> Result<Vec<Stmt>, String> {
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

    if parser.errors.is_empty() {
        Ok(stmts)
    } else {
        Err(parser.errors.join("\n"))
    }
}

pub fn parse_expr(tokens: &Vec<Token>) -> Result<LocExpr> {
    // `parse_expr` should only be called if `parse` failed, but it succeeds when tokens are empty.
    assert!(!tokens.is_empty());
    Parser::new(tokens).parse_expr()
}
