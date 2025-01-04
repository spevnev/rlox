use std::{cell::RefCell, collections::HashSet};

use crate::{
    error::{print_error, Loc},
    parser::{Assign, Binary, Call, Expr, FunDecl, If, LocExpr, Stmt, Var, VarDecl, While},
};

struct Resolver {
    had_error: bool,
    function_count: i32,
    scopes: Vec<HashSet<String>>,
}

impl Resolver {
    fn new() -> Self {
        Self {
            had_error: false,
            function_count: 0,
            scopes: Vec::new(),
        }
    }

    fn declare(&mut self, loc: Loc, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.insert(name.to_owned()) == false {
                self.had_error = true;
                print_error(loc, &format!("Redefinition of symbol '{}'", name));
            }
        }
    }

    fn resolve_var(&mut self, var: &Var) {
        let mut depth = 0; // TODO: rename?
        for scope in self.scopes.iter().rev() {
            if scope.contains(&var.name) {
                *RefCell::borrow_mut(&var.depth) = depth;
                return;
            }
            depth += 1;
        }
    }

    fn resolve_expr(&mut self, expr: &LocExpr) {
        match &expr.expr {
            Expr::Literal(_) => {},
            Expr::Unary(unary) => self.resolve_expr(&unary.expr),
            Expr::Binary(Binary { left, op: _, right }) | Expr::Logical(Binary { left, op: _, right }) => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            },
            Expr::Variable(var) => self.resolve_var(var),
            Expr::Assign(Assign { var, expr }) => {
                self.resolve_var(var);
                self.resolve_expr(expr);
            },
            Expr::Call(Call { callee, args }) => {
                self.resolve_expr(callee);
                for arg in args {
                    self.resolve_expr(arg);
                }
            },
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => self.resolve_expr(expr),
            Stmt::Block(stmts) => {
                self.scopes.push(HashSet::new());
                for stmt in stmts {
                    self.resolve_stmt(stmt);
                }
                self.scopes.pop();
            },
            Stmt::If(If {
                condition,
                then_branch,
                else_branch,
            }) => {
                self.resolve_expr(condition);
                self.resolve_stmt(then_branch);
                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(else_branch);
                }
            },
            Stmt::While(While { condition, body }) => {
                self.resolve_expr(condition);
                self.resolve_stmt(body);
            },
            Stmt::VarDecl(VarDecl { name, init }) => {
                // Resolve `init` before declaring the variable so that if it
                // uses variable with name `name` it resolves to the outer one.
                self.resolve_expr(init);
                self.declare(name.loc, &name.to_identifier().unwrap());
            },
            Stmt::FunDecl(FunDecl { name, decl }) => {
                self.declare(name.loc, &name.to_identifier().unwrap());

                self.scopes.push(HashSet::new());
                for param in &decl.params {
                    self.declare(param.loc, &param.to_identifier().unwrap());
                }

                self.function_count += 1;
                for stmt in &decl.body {
                    self.resolve_stmt(stmt);
                }
                self.function_count -= 1;

                self.scopes.pop();
            },
            Stmt::Return(expr) => {
                if self.function_count == 0 {
                    self.had_error = true;
                    print_error(expr.loc, "Return outside of function");
                }
                self.resolve_expr(expr);
            },
        }
    }
}
pub fn resolve(stmts: &Vec<Stmt>) -> Result<(), ()> {
    let mut resolver = Resolver::new();

    for stmt in stmts {
        resolver.resolve_stmt(stmt);
    }

    if !resolver.had_error {
        Ok(())
    } else {
        Err(())
    }
}
