use ahash::AHashMap;

use crate::{
    error::{error, warning, Loc},
    parser::{
        Assign, Binary, Call, ClassDecl, Expr, For, FunDecl, GetElement, If, LocExpr, Return, SetElement, Stmt, Super,
        Superclass, Var, VarDecl, VarScope, While,
    },
    value::Class,
};

type Result<V, E = ()> = std::result::Result<V, E>;

#[derive(PartialEq)]
enum SymbolKind {
    Variable,
    Function,
    Parameter,
    Class,
    /// Symbols like 'this' and 'super' shouldn't be reported as unused.
    IgnoreUnused,
}

#[derive(PartialEq)]
struct UnreadErrorInfo {
    loc: Loc,
    symbol_kind: SymbolKind,
}

#[derive(PartialEq)]
enum VarState {
    /// Declared means that the name is taken, but usage is still invalid.
    Declared,
    Defined(UnreadErrorInfo),
    Read,
}

#[derive(PartialEq, Clone, Copy)]
enum FunKind {
    None,
    Function,
    Initializer,
    Static,
}

#[derive(PartialEq, Clone, Copy)]
enum ClassKind {
    None,
    Class,
    Subclass,
}

struct Resolver {
    had_error: bool,
    current_fun: FunKind,
    current_class: ClassKind,
    loop_depth: u32,
    scopes: Vec<AHashMap<String, VarState>>,
}

/// Resolver binds local variables to specific instances by settings their `scope`.
/// Global variables are resolved at runtime since it is valid to declare them after
/// they are used by a function, provided that the declaration is evaluated before it.
/// Additionally, it performs a number of checks and validations, such as usage of
/// `this`, `super`, `break`, `return`.
impl Resolver {
    fn new() -> Self {
        Self {
            had_error: false,
            current_fun: FunKind::None,
            current_class: ClassKind::None,
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
            error!(loc, "Redeclaration of symbol '{name}'");
        }
    }

    fn define(&mut self, loc: Loc, name: &str, symbol_kind: SymbolKind) {
        let Some(scope) = self.scopes.last_mut() else {
            return;
        };

        let Some(prev_state) = scope.insert(name.to_owned(), VarState::Defined(UnreadErrorInfo { loc, symbol_kind }))
        else {
            return;
        };

        // Define is only valid when the previous state is `None` or `Declared`.
        if prev_state != VarState::Declared {
            self.had_error = true;
            error!(loc, "Redefinition of symbol '{name}'");
        }
    }

    fn resolve_var(&mut self, loc: Loc, var: &Var, is_read: bool) {
        let mut depth = 0;
        for scope in self.scopes.iter_mut().rev() {
            if let Some(state) = scope.get(&var.name) {
                match state {
                    VarState::Declared => {
                        self.had_error = true;
                        error!(loc, "Can't read variable '{}' in its own initializer", var.name);
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
            if let VarState::Defined(info) = state {
                match info.symbol_kind {
                    SymbolKind::Variable => warning!(info.loc, "Unread local variable '{name}'"),
                    SymbolKind::Parameter => warning!(info.loc, "Unread parameter '{name}'"),
                    SymbolKind::Function => warning!(info.loc, "Unused local function '{name}'"),
                    SymbolKind::Class => warning!(info.loc, "Unused local class '{name}'"),
                    SymbolKind::IgnoreUnused => return,
                }
            }
        }
    }

    fn resolve_expr(&mut self, LocExpr { loc, expr }: &LocExpr) {
        match expr {
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
            Expr::Variable(var) => self.resolve_var(*loc, var, true),
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
                if self.current_class == ClassKind::None {
                    self.had_error = true;
                    error(*loc, "Cannot use 'this' outside of class");
                }

                self.resolve_var(*loc, var, true);
            },
            Expr::Super(Super { var, method: _ }) => {
                if self.current_fun == FunKind::Static {
                    self.had_error = true;
                    error(*loc, "Cannot use 'super' in a static method");
                } else if self.current_class == ClassKind::None {
                    self.had_error = true;
                    error(*loc, "Cannot use 'super' outside of class");
                } else if self.current_class == ClassKind::Class {
                    self.had_error = true;
                    error(*loc, "Cannot use 'super' in a class without superclass");
                }

                self.resolve_var(*loc, var, true);
            },
            Expr::Lambda(lambda) => {
                self.begin_scope();

                for param in &lambda.params {
                    self.define(param.loc, &param.name, SymbolKind::Parameter);
                }

                let prev_fun = self.current_fun;
                self.current_fun = FunKind::Function;
                for stmt in lambda.body.as_ref() {
                    self.resolve_stmt(stmt);
                }
                self.current_fun = prev_fun;

                self.end_scope();
            },
            Expr::GetElement(GetElement { array, index }) => {
                self.resolve_expr(array);
                self.resolve_expr(index);
            },
            Expr::SetElement(SetElement { array, index, expr }) => {
                self.resolve_expr(expr);
                self.resolve_expr(array);
                self.resolve_expr(index);
            },
            Expr::ArrayLiteral(exprs) => {
                for expr in exprs {
                    self.resolve_expr(expr);
                }
            },
        }
    }

    fn resolve_fun(&mut self, fun: &FunDecl, fun_kind: FunKind, symbol_kind: SymbolKind) {
        self.define(fun.name_loc, &fun.name, symbol_kind);

        self.begin_scope();

        for param in &fun.params {
            self.define(param.loc, &param.name, SymbolKind::Parameter);
        }

        let prev_fun = self.current_fun;
        self.current_fun = fun_kind;
        for stmt in fun.body.as_ref() {
            self.resolve_stmt(stmt);
        }
        self.current_fun = prev_fun;

        self.end_scope();
    }

    fn resolve_class_decl(&mut self, decl: &ClassDecl) {
        self.define(decl.name_loc, &decl.name, SymbolKind::Class);

        let prev_class = self.current_class;
        if let Some(Superclass { loc, var }) = &decl.superclass {
            if var.name == decl.name {
                self.had_error = true;
                error(*loc, "Class cannot inherit from itself");
            } else {
                self.resolve_var(*loc, var, true);
            }

            self.begin_scope(); // 'super' scope
            self.define(Loc::none(), "super", SymbolKind::IgnoreUnused);
            self.current_class = ClassKind::Subclass;
        } else {
            self.current_class = ClassKind::Class;
        }

        self.begin_scope(); // 'this' scope
        self.define(Loc::none(), "this", SymbolKind::IgnoreUnused);
        for method in &decl.methods {
            if method.name == Class::INIT_METHOD {
                self.resolve_fun(method, FunKind::Initializer, SymbolKind::IgnoreUnused);
            } else {
                self.resolve_fun(method, FunKind::Function, SymbolKind::IgnoreUnused);
            }
        }
        self.end_scope(); // 'this' scope

        if decl.superclass.is_some() {
            self.end_scope(); // 'super' scope
        }

        self.begin_scope(); // 'this' scope
        self.define(Loc::none(), "this", SymbolKind::IgnoreUnused);
        for method in &decl.static_methods {
            if method.name == Class::INIT_METHOD {
                self.had_error = true;
                error(method.name_loc, "Class cannot have static 'init' method");
            } else {
                self.resolve_fun(method, FunKind::Static, SymbolKind::IgnoreUnused);
            }
        }
        self.end_scope(); // 'this' scope

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
            Stmt::For(For {
                initializer,
                condition,
                update,
                body,
            }) => {
                self.begin_scope();
                if let Some(initializer) = initializer {
                    self.resolve_stmt(initializer);
                }
                self.resolve_expr(condition);
                self.loop_depth += 1;
                self.resolve_stmt(body);
                self.loop_depth -= 1;
                if let Some(update) = update {
                    self.resolve_expr(update);
                }
                self.end_scope();
            },
            Stmt::Break(loc) => {
                if self.loop_depth == 0 {
                    self.had_error = true;
                    error(*loc, "Cannot break outside of loop");
                }
            },
            Stmt::Continue(loc) => {
                if self.loop_depth == 0 {
                    self.had_error = true;
                    error(*loc, "Cannot continue outside of loop");
                }
            },
            Stmt::VarDecl(VarDecl { name_loc, name, init }) => {
                // Declaring before resolving `init` expr allows to throw an error
                // if it uses a variable with the same name as one being defined.
                self.declare(*name_loc, name);
                self.resolve_expr(init);
                self.define(*name_loc, name, SymbolKind::Variable);
            },
            Stmt::FunDecl(fun) => self.resolve_fun(fun, FunKind::Function, SymbolKind::Function),
            Stmt::Return(Return { loc, expr }) => {
                if self.current_fun == FunKind::None {
                    self.had_error = true;
                    error(*loc, "Cannot return outside of function");
                }

                if let Some(expr) = expr {
                    if self.current_fun == FunKind::Initializer {
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

pub fn resolve(stmts: &Vec<Stmt>) -> Result<()> {
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
