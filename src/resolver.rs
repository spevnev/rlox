use ahash::AHashMap;

use crate::{
    error::{error, warning, Loc},
    parser::{
        Assign, Binary, Call, ClassDecl, Expr, FunDecl, If, LocExpr, Return, Stmt, Super, Superclass, Var, VarDecl,
        VarScope, While,
    },
    value::Class,
};

#[derive(PartialEq, Clone, Copy)]
enum VarState {
    /// Declared means that the name is taken, but usage is still invalid.
    Declared,
    Defined(Loc),
    Read,
}

#[derive(PartialEq, Clone, Copy)]
enum FunType {
    None,
    Function,
    Initializer,
}

#[derive(PartialEq, Clone, Copy)]
enum ClassType {
    None,
    Class,
    Subclass,
}

struct Resolver {
    had_error: bool,
    current_fun: FunType,
    current_class: ClassType,
    loop_depth: u32,
    scopes: Vec<AHashMap<String, VarState>>,
}

/// Resolver binds local variables to specific instances by settings their `scope`.
/// Global variables are resolved at runtime since it is valid to declare them after
/// they are used by a function, provided that the declaration is evaluated before it.
/// Additionally, it validates usage of `return` and `this`.
impl Resolver {
    fn new() -> Self {
        Self {
            had_error: false,
            current_fun: FunType::None,
            current_class: ClassType::None,
            loop_depth: 0,
            scopes: Vec::new(),
        }
    }

    fn declare(&mut self, loc: Loc, name: &str) {
        let Some(scope) = self.scopes.last_mut() else {
            return;
        };

        if scope.insert(name.to_owned(), VarState::Declared).is_some() {
            self.had_error = true;
            error(loc, &format!("Redeclaration of symbol '{}'", name));
        }
    }

    fn define(&mut self, loc: Loc, name: &str) {
        let Some(scope) = self.scopes.last_mut() else {
            return;
        };

        let Some(prev_state) = scope.insert(name.to_owned(), VarState::Defined(loc)) else {
            return;
        };

        // Define is only valid when the previous state is `None` or `Declared`.
        if prev_state != VarState::Declared {
            self.had_error = true;
            error(loc, &format!("Redefinition of symbol '{}'", name));
        }
    }

    fn resolve_var(&mut self, loc: Loc, var: &Var, is_read: bool) {
        let mut depth = 0;
        for scope in self.scopes.iter_mut().rev() {
            if let Some(state) = scope.get(&var.name) {
                match state {
                    VarState::Declared => {
                        self.had_error = true;
                        error(loc, &format!("Can't read variable '{}' in its own initializer", var.name));
                    },
                    VarState::Defined(_) => {
                        if is_read {
                            scope.insert(var.name.clone(), VarState::Read);
                        }
                        var.scope.set(VarScope::Relative(depth))
                    },
                    VarState::Read => var.scope.set(VarScope::Relative(depth)),
                }
                return;
            }
            depth += 1;
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(AHashMap::new());
    }

    fn end_scope(&mut self) {
        let prev_scope = self.scopes.pop().unwrap();

        for (name, state) in prev_scope {
            if let VarState::Defined(loc) = state {
                if name != Class::THIS && name != Class::SUPER {
                    warning(loc, &format!("Unread local variable '{}'", name));
                }
            }
        }
    }

    fn resolve_expr(&mut self, expr: &LocExpr) {
        match &expr.expr {
            Expr::Literal(_) => {},
            Expr::Grouping(expr) => self.resolve_expr(expr),
            Expr::Unary(unary) => self.resolve_expr(&unary.expr),
            Expr::Binary(Binary { left, op: _, right }) | Expr::Logical(Binary { left, op: _, right }) => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            },
            Expr::Ternary(ternary) => {
                self.resolve_expr(&ternary.condition);
                self.resolve_expr(&ternary.then_expr);
                self.resolve_expr(&ternary.else_expr);
            },
            Expr::Variable(var) => self.resolve_var(expr.loc, var, true),
            Expr::Assign(Assign { var, expr }) => {
                self.resolve_expr(expr);
                self.resolve_var(expr.loc, var, false);
            },
            Expr::Call(Call { callee, args }) => {
                self.resolve_expr(callee);
                for arg in args {
                    self.resolve_expr(arg);
                }
            },
            Expr::GetProp(get) => self.resolve_expr(&get.instance),
            Expr::SetProp(set) => {
                self.resolve_expr(&set.expr);
                self.resolve_expr(&set.instance);
            },
            Expr::This(var) => {
                if self.current_class == ClassType::None {
                    self.had_error = true;
                    error(expr.loc, "Cannot use 'this' outside of class");
                }

                self.resolve_var(expr.loc, var, true);
            },
            Expr::Super(Super { var, method: _ }) => {
                if self.current_class == ClassType::None {
                    self.had_error = true;
                    error(expr.loc, "Cannot use 'super' outside of class");
                } else if self.current_class == ClassType::Class {
                    self.had_error = true;
                    error(expr.loc, "Cannot use 'super' in a class without superclass");
                }

                self.resolve_var(expr.loc, var, true);
            },
            Expr::Lambda(lambda) => {
                self.begin_scope();

                for param in &lambda.params {
                    self.define(param.loc, &param.name);
                }

                let prev_fun = self.current_fun;
                self.current_fun = FunType::Function;
                for stmt in lambda.body.as_ref() {
                    self.resolve_stmt(stmt);
                }
                self.current_fun = prev_fun;

                self.end_scope();
            },
        }
    }

    fn resolve_fun(&mut self, fun: &FunDecl, fun_type: FunType) {
        self.define(fun.name_loc, &fun.name);

        self.begin_scope();

        for param in &fun.params {
            self.define(param.loc, &param.name);
        }

        let prev_fun = self.current_fun;
        self.current_fun = fun_type;
        for stmt in fun.body.as_ref() {
            self.resolve_stmt(stmt);
        }
        self.current_fun = prev_fun;

        self.end_scope();
    }

    fn resolve_class_decl(&mut self, decl: &ClassDecl) {
        self.define(decl.name_loc, &decl.name);

        let prev_class = self.current_class;
        if let Some(Superclass { loc, var }) = &decl.superclass {
            if var.name == decl.name {
                self.had_error = true;
                error(*loc, "Class cannot inherit from itself");
            } else {
                self.resolve_var(*loc, var, true);
            }

            self.begin_scope(); // 'super' scope
            self.define(Loc::none(), Class::SUPER);
            self.current_class = ClassType::Subclass;
        } else {
            self.current_class = ClassType::Class;
        }

        self.begin_scope(); // 'this' scope
        self.define(Loc::none(), Class::THIS);

        for method in &decl.methods {
            let fun_type = if method.name == Class::INITIALIZER_METHOD {
                FunType::Initializer
            } else {
                FunType::Function
            };
            self.resolve_fun(method, fun_type);
        }

        self.end_scope(); // 'this' scope

        if decl.superclass.is_some() {
            self.end_scope(); // 'super' scope
        }

        self.current_class = prev_class;
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) | Stmt::Print(expr) => self.resolve_expr(expr),
            Stmt::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts {
                    self.resolve_stmt(stmt);
                }
                self.end_scope();
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
                self.loop_depth += 1;
                self.resolve_stmt(body);
                self.loop_depth -= 1;
            },
            Stmt::Break(loc) => {
                if self.loop_depth == 0 {
                    self.had_error = true;
                    error(*loc, "Break outside of loop");
                }
            },
            Stmt::VarDecl(VarDecl { name_loc, name, init }) => {
                // Declaring before resolving `init` expr allows to throw an error
                // if it uses a variable with the same name as one being defined.
                self.declare(*name_loc, name);
                self.resolve_expr(init);
                self.define(*name_loc, name);
            },
            Stmt::FunDecl(fun) => self.resolve_fun(fun, FunType::Function),
            Stmt::Return(Return { loc, expr }) => {
                if self.current_fun == FunType::None {
                    self.had_error = true;
                    error(*loc, "Return outside of function");
                }

                if let Some(expr) = expr {
                    if self.current_fun == FunType::Initializer {
                        self.had_error = true;
                        error(expr.loc, "Cannot return value from initializer");
                    }
                    self.resolve_expr(expr);
                }
            },
            Stmt::ClassDecl(decl) => self.resolve_class_decl(decl),
        }
    }
}

pub fn resolve(stmts: &Vec<Stmt>) -> Result<(), ()> {
    let mut resolver = Resolver::new();

    for stmt in stmts {
        resolver.resolve_stmt(stmt);
    }

    if resolver.had_error {
        Err(())
    } else {
        Ok(())
    }
}
