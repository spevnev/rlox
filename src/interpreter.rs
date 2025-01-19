use std::{cell::RefCell, fmt::Debug, mem, rc::Rc};

use ahash::AHashMap;

use crate::{
    error::{error, Loc},
    lexer::TokenKind,
    native::get_native_functions_as_symbols,
    parser::{
        Binary, Call, Expr, FunDecl, GetProp, If, LocExpr, Return, SetProp, Stmt, Super, Superclass, Unary, Var,
        VarScope, While,
    },
    value::{Callable, Class, Function, Instance, LoxFun, Value},
};

pub enum Error {
    WrongType,
    UndefinedSymbol,
    WrongArity,

    // The following aren't actual errors, and are used to quickly return from a deeply nested call:
    Return(Value),
}

/// Debug trait is required in order to panic.
impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::WrongType => write!(f, "WrongType"),
            Self::UndefinedSymbol => write!(f, "UndefinedSymbol"),
            Self::WrongArity => write!(f, "WrongArity"),
            Self::Return(_) => panic!("Panic on 'Return' must be impossible."),
        }
    }
}

type Result<V, E = Error> = std::result::Result<V, E>;

impl Callable {
    fn bind(&self, instance: Rc<Instance>) -> Callable {
        match &self.fun {
            Function::Lox(fun) => {
                let instance_closure = Scope::new(fun.closure.clone());
                instance_closure.define_symbol(Class::THIS.to_owned(), Value::Instance(instance));

                Callable {
                    name: self.name.clone(),
                    arity: self.arity,
                    fun: Function::Lox(LoxFun {
                        is_initializer: fun.is_initializer,
                        params: fun.params.clone(),
                        body: fun.body.clone(),
                        closure: instance_closure,
                    }),
                }
            },
            _ => panic!(), // TODO:
        }
    }
}

impl Instance {
    // TODO: Self? don't pass this.
    fn get(&self, this: Rc<Instance>, property_loc: Loc, property: &str) -> Result<Value> {
        if let Some(value) = self.fields.borrow().get(property) {
            Ok(value.clone())
        } else if let Some(method) = self.class.get_method(property) {
            Ok(Value::Callable(Rc::new(method.bind(this))))
        } else {
            error(property_loc, &format!("Undefined property '{}'", property));
            Err(Error::UndefinedSymbol)
        }
    }

    fn set(&self, property: String, value: Value) {
        self.fields.borrow_mut().insert(property, value);
    }
}

impl Value {
    fn type_expected_error<T>(&self, loc: Loc, expected: &str) -> Result<T> {
        error(
            loc,
            &format!("Expected {expected} but found '{}'", self.convert_to_string(true)),
        );
        Err(Error::WrongType)
    }

    fn is_string(&self) -> bool {
        match self {
            Value::String(_) => true,
            _ => false,
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(bool) => *bool,
            Value::Nil => false,
            _ => true,
        }
    }

    fn to_number(&self, loc: Loc) -> Result<f64> {
        match self {
            Value::Number(num) => Ok(*num),
            _ => self.type_expected_error(loc, "number"),
        }
    }

    fn to_instance(&self) -> Option<&Rc<Instance>> {
        match self {
            Value::Instance(instance) => Some(instance),
            _ => None,
        }
    }

    fn to_class(&self) -> Option<&Rc<Class>> {
        match self {
            Value::Class(class) => Some(class),
            _ => None,
        }
    }
}

pub struct Scope {
    parent: Option<Rc<Scope>>,
    symbols: RefCell<AHashMap<String, Value>>,
}

impl Scope {
    fn new(parent: Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            parent: Some(parent),
            symbols: RefCell::new(AHashMap::new()),
        })
    }

    fn new_global() -> Rc<Self> {
        Rc::new(Self {
            parent: None,
            symbols: RefCell::new(get_native_functions_as_symbols()),
        })
    }

    fn define_symbol(&self, name: String, value: Value) {
        self.symbols.borrow_mut().insert(name, value);
    }

    fn get_symbol(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.symbols.borrow().get(name) {
            Some(value.clone())
        } else {
            None
        }
    }

    fn set_var(&self, loc: Loc, name: &str, value: Value) -> Result<()> {
        if let Some(var) = self.symbols.borrow_mut().get_mut(name) {
            *var = value;
            Ok(())
        } else {
            error(loc, &format!("Assigning to undefined variable '{}'", name));
            Err(Error::UndefinedSymbol)
        }
    }
}

pub struct Interpreter {
    global: Rc<Scope>,
    scope: Rc<Scope>,
}

impl Interpreter {
    pub fn new() -> Self {
        let global = Scope::new_global();
        Self {
            scope: global.clone(),
            global,
        }
    }

    fn get_nth_scope(&self, depth: i32) -> &Rc<Scope> {
        let mut current = &self.scope;
        for _ in 0..depth {
            current = current.parent.as_ref().unwrap();
        }
        current
    }

    fn define_symbol(&self, name: String, value: Value) {
        self.scope.define_symbol(name, value);
    }

    fn get_symbol(&self, var: &Var) -> Option<Value> {
        match var.scope.get() {
            VarScope::Global => self.global.get_symbol(&var.name),
            VarScope::Relative(depth) => self.get_nth_scope(depth).get_symbol(&var.name),
        }
    }

    fn set_var(&mut self, loc: Loc, var: &Var, value: Value) -> Result<()> {
        match var.scope.get() {
            VarScope::Global => self.global.set_var(loc, &var.name, value),
            VarScope::Relative(depth) => self.get_nth_scope(depth).set_var(loc, &var.name, value),
        }
    }

    fn eval_unary(&mut self, unary: &Unary) -> Result<Value> {
        let value = self.eval_expr(&unary.expr)?;

        match unary.op {
            TokenKind::Minus => Ok(Value::Number(-1.0 * value.to_number(unary.expr.loc)?)),
            TokenKind::Bang => Ok(Value::Bool(!value.is_truthy())),
            _ => panic!("Unexpected unary operand: {:?}", unary.op),
        }
    }

    fn eval_binary(&mut self, binary: &Binary) -> Result<Value> {
        let left = self.eval_expr(&binary.left)?;
        let right = self.eval_expr(&binary.right)?;

        match binary.op {
            TokenKind::EqualEqual => Ok(Value::Bool(left.equals(&right))),
            TokenKind::BangEqual => Ok(Value::Bool(!left.equals(&right))),
            TokenKind::Greater => Ok(Value::Bool(
                left.to_number(binary.left.loc)? > right.to_number(binary.right.loc)?,
            )),
            TokenKind::GreaterEqual => Ok(Value::Bool(
                left.to_number(binary.left.loc)? >= right.to_number(binary.right.loc)?,
            )),
            TokenKind::Less => Ok(Value::Bool(
                left.to_number(binary.left.loc)? < right.to_number(binary.right.loc)?,
            )),
            TokenKind::LessEqual => Ok(Value::Bool(
                left.to_number(binary.left.loc)? <= right.to_number(binary.right.loc)?,
            )),
            TokenKind::Plus => {
                if left.is_string() || right.is_string() {
                    Ok(Value::String(left.convert_to_string(false) + &right.convert_to_string(false)))
                } else {
                    Ok(Value::Number(
                        left.to_number(binary.left.loc)? + right.to_number(binary.right.loc)?,
                    ))
                }
            },
            TokenKind::Minus => Ok(Value::Number(
                left.to_number(binary.left.loc)? - right.to_number(binary.right.loc)?,
            )),
            TokenKind::Star => Ok(Value::Number(
                left.to_number(binary.left.loc)? * right.to_number(binary.right.loc)?,
            )),
            TokenKind::Slash => Ok(Value::Number(
                left.to_number(binary.left.loc)? / right.to_number(binary.right.loc)?,
            )),
            _ => panic!("Unexpected binary operand: {:?}", binary.op),
        }
    }

    fn eval_logical(&mut self, binary: &Binary) -> Result<Value> {
        let left = self.eval_expr(&binary.left)?;

        match binary.op {
            TokenKind::And => {
                if left.is_truthy() {
                    self.eval_expr(&binary.right)
                } else {
                    Ok(left)
                }
            },
            TokenKind::Or => {
                if left.is_truthy() {
                    Ok(left)
                } else {
                    self.eval_expr(&binary.right)
                }
            },
            _ => panic!("Unexpected logical operand: {:?}", binary.op),
        }
    }

    fn eval_lox_fun(&mut self, fun: &LoxFun, args: Vec<Value>) -> Result<Value> {
        let new_scope = Scope::new(fun.closure.clone());
        let prev_scope = mem::replace(&mut self.scope, new_scope);

        assert!(fun.params.len() == args.len());
        for (param, arg) in fun.params.iter().zip(args.into_iter()) {
            self.define_symbol(param.name.clone(), arg);
        }

        let result = match self.eval_stmts(&fun.body) {
            Err(Error::Return(value)) => value,
            Ok(_) => Value::Nil,
            Err(err) => {
                self.scope = prev_scope;
                return Err(err);
            },
        };

        self.scope = prev_scope;
        if fun.is_initializer {
            Ok(fun.closure.get_symbol(Class::THIS).unwrap())
        } else {
            Ok(result)
        }
    }

    fn eval_callable(&mut self, call: &Call, callable: &Callable) -> Result<Value> {
        if call.args.len() != callable.arity {
            error(
                call.callee.loc,
                &format!("Expected {} arguments but got {}", callable.arity, call.args.len()),
            );
            return Err(Error::WrongArity);
        }

        let mut arg_values = Vec::new();
        for arg in &call.args {
            arg_values.push(self.eval_expr(arg)?);
        }

        match &callable.fun {
            Function::Native(fun) => Ok(fun(arg_values)),
            Function::Lox(fun) => self.eval_lox_fun(fun, arg_values),
        }
    }

    fn eval_constructor(&mut self, call: &Call, class: Rc<Class>) -> Result<Value> {
        let instance = Rc::new(Instance {
            class: class.clone(),
            fields: RefCell::new(AHashMap::new()),
        });

        if let Some(initializer) = class.get_method(Class::INITIALIZER_METHOD) {
            self.eval_callable(call, &initializer.bind(instance.clone()))?;
        } else if !call.args.is_empty() {
            error(
                call.callee.loc,
                &format!(
                    "Class doesn't have an initializer, expected no arguments but got {}",
                    call.args.len()
                ),
            );
            return Err(Error::WrongArity);
        }

        Ok(Value::Instance(instance))
    }

    fn eval_call(&mut self, call: &Call) -> Result<Value> {
        let value = self.eval_expr(&call.callee)?;
        match value {
            Value::Callable(callable) => self.eval_callable(call, &callable),
            Value::Class(class) => self.eval_constructor(call, class),
            _ => {
                error(
                    call.callee.loc,
                    &format!("Expected function or constructor but found '{}'", value.convert_to_string(true)),
                );
                Err(Error::WrongType)
            },
        }
    }

    fn eval_get(&mut self, loc: Loc, get: &GetProp) -> Result<Value> {
        let value = self.eval_expr(&get.instance)?;
        let instance = value.to_instance().ok_or_else(|| {
            error(
                get.instance.loc,
                &format!(
                    "Property access expected instance but found '{}'",
                    value.convert_to_string(true)
                ),
            );
            Error::WrongType
        })?;
        let property = instance.get(instance.clone(), loc, &get.property)?;
        Ok(property.clone())
    }

    fn eval_set(&mut self, set: &SetProp) -> Result<Value> {
        let value = self.eval_expr(&set.instance)?;
        let instance = value.to_instance().ok_or_else(|| {
            error(
                set.instance.loc,
                &format!(
                    "Property assigning expected instance but found '{}'",
                    value.convert_to_string(true)
                ),
            );
            Error::WrongType
        })?;

        let set_value = self.eval_expr(&set.expr)?;
        instance.set(set.property.clone(), set_value.clone());

        Ok(set_value)
    }

    fn eval_expr(&mut self, expr: &LocExpr) -> Result<Value> {
        match &expr.expr {
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Grouping(expr) => self.eval_expr(expr),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            Expr::Logical(binary) => self.eval_logical(binary),
            Expr::Variable(var) | Expr::This(var) => self.get_symbol(&var).ok_or_else(|| {
                error(expr.loc, &format!("Undefined symbol '{}'", var.name));
                Error::UndefinedSymbol
            }),
            Expr::Assign(assign) => {
                let value = self.eval_expr(&assign.expr)?;
                self.set_var(expr.loc, &assign.var, value.clone())?;
                Ok(value)
            },
            Expr::Call(call) => self.eval_call(call),
            Expr::GetProp(get) => self.eval_get(expr.loc, get),
            Expr::SetProp(set) => self.eval_set(set),
            Expr::Super(Super { var, method }) => {
                let value = self.get_symbol(var).unwrap();
                let superclass = value.to_class().unwrap();

                if let Some(method) = superclass.get_method(&method) {
                    // Scopes are structured such that 'this' is always one scope closer than 'super',
                    // which makes this hack possible.
                    let VarScope::Relative(depth) = var.scope.get() else {
                        panic!("'super' mustn't be in global scope");
                    };

                    let value = self.get_nth_scope(depth - 1).get_symbol(Class::THIS).unwrap();
                    let this = value.to_instance().unwrap().clone();

                    Ok(Value::Callable(Rc::new(method.bind(this))))
                } else {
                    error(expr.loc, &format!("Undefined superclass method '{}'", method));
                    Err(Error::UndefinedSymbol)
                }
            },
        }
    }

    fn eval_fun_decl(&self, fun: &FunDecl, is_method: bool) -> Rc<Callable> {
        Rc::new(Callable {
            name: fun.name.clone(),
            arity: fun.params.len(),
            fun: Function::Lox(LoxFun {
                is_initializer: is_method && fun.name == Class::INITIALIZER_METHOD,
                params: fun.params.clone(),
                body: fun.body.clone(),
                closure: self.scope.clone(),
            }),
        })
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
            },
            Stmt::Print(expr) => println!("{}", self.eval_expr(expr)?.convert_to_string(false)),
            Stmt::Block(stmts) => {
                let new_scope = Scope::new(self.scope.clone());
                let prev_scope = mem::replace(&mut self.scope, new_scope);

                for stmt in stmts {
                    if let Err(err) = self.eval_stmt(stmt) {
                        // Pop the scope on error and propagate it.
                        self.scope = prev_scope;
                        return Err(err);
                    }
                }

                self.scope = prev_scope;
            },
            Stmt::If(If {
                condition,
                then_branch,
                else_branch,
            }) => {
                let result = self.eval_expr(condition)?;
                if result.is_truthy() {
                    self.eval_stmt(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.eval_stmt(else_branch)?;
                }
            },
            Stmt::While(While { condition, body }) => {
                while self.eval_expr(condition)?.is_truthy() {
                    self.eval_stmt(body)?;
                }
            },
            Stmt::VarDecl(var) => {
                let value = self.eval_expr(&var.init)?;
                self.define_symbol(var.name.clone(), value);
            },
            Stmt::FunDecl(fun) => {
                let value = Value::Callable(self.eval_fun_decl(fun, false));
                self.define_symbol(fun.name.clone(), value);
            },
            Stmt::Return(Return { loc: _, expr }) => {
                let value = if let Some(expr) = expr {
                    self.eval_expr(expr)?
                } else {
                    Value::Nil
                };
                return Err(Error::Return(value));
            },
            Stmt::ClassDecl(decl) => {
                let prev_scope = self.scope.clone();
                let superclass;
                if let Some(Superclass { loc, var }) = &decl.superclass {
                    let value = self.get_symbol(var).ok_or_else(|| {
                        error(*loc, &format!("Undefined class '{}'", var.name));
                        Error::UndefinedSymbol
                    })?;
                    let superclass_ref = value.to_class().ok_or_else(|| {
                        error(
                            *loc,
                            &format!(
                                "Expected superclass to be a class but found '{}'",
                                value.convert_to_string(true)
                            ),
                        );
                        Error::WrongType
                    })?;

                    self.scope = Scope::new(self.scope.clone()); // add 'super' scope
                    self.define_symbol(Class::SUPER.to_owned(), Value::Class(superclass_ref.clone()));

                    superclass = Some(superclass_ref.clone());
                } else {
                    superclass = None;
                }

                let mut methods = AHashMap::new();
                for method in &decl.methods {
                    let value = self.eval_fun_decl(method, true);
                    methods.insert(method.name.clone(), value);
                }

                if decl.superclass.is_some() {
                    self.scope = prev_scope; // remove 'super' scope
                }

                let class = Value::Class(Rc::new(Class {
                    decl: decl.clone(),
                    methods,
                    superclass,
                }));
                self.define_symbol(decl.name.clone(), class);
            },
        };

        Ok(())
    }

    fn eval_stmts(&mut self, stmts: &Vec<Stmt>) -> Result<()> {
        for stmt in stmts {
            self.eval_stmt(stmt)?;
        }

        Ok(())
    }

    pub fn eval(&mut self, stmts: &Vec<Stmt>) -> Result<()> {
        match self.eval_stmts(stmts) {
            Err(Error::Return(_)) => panic!("Return outside of function."),
            Err(err) => return Err(err),
            Ok(_) => Ok(()),
        }
    }
}
