use std::{cell::RefCell, collections::HashMap, fmt::Debug, mem, rc::Rc};

use crate::{
    error::{print_error, Loc},
    lexer::TokenKind,
    native::get_native_functions_as_symbols,
    parser::{
        Binary, Call, Expr, FunDecl, GetProp, If, LocExpr, Return, SetProp, Stmt, Unary, Var, VarDecl, VarScope, While,
    },
    value::{Callable, Class, Constructor, Function, LoxFun, Object, Value},
};

pub enum Error {
    MismatchingType,
    SymbolRedeclaration,
    UndefinedSymbol,
    DivisionByZero,
    WrongArity,

    // The following aren't actual errors, and are used to quickly return from a deeply nested call:
    Return(Value),
}

/// Debug trait is required in order to panic.
impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MismatchingType => write!(f, "MismatchingType"),
            Self::SymbolRedeclaration => write!(f, "SymbolRedeclaration"),
            Self::UndefinedSymbol => write!(f, "UndefinedSymbol"),
            Self::DivisionByZero => write!(f, "DivisionByZero"),
            Self::WrongArity => write!(f, "WrongArity"),
            Self::Return(_) => panic!("Panic on 'Return' must be impossible."),
        }
    }
}

type Result<V, E = Error> = std::result::Result<V, E>;

impl Callable {
    fn bind(&self, object: Rc<Object>) -> Callable {
        match &self.fun {
            Function::Lox(fun) => {
                let instance_closure = Scope::new(fun.closure.clone());
                instance_closure
                    .borrow_mut()
                    .define_symbol(Loc::none(), "this".to_owned(), Value::Object(object))
                    .unwrap(); // TODO: shouldn't fail. expect?

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

impl Object {
    // TODO: Self? don't pass this.
    fn get(&self, this: Rc<Object>, loc: Loc, property: &str) -> Result<Value> {
        if let Some(value) = self.fields.borrow().get(property) {
            Ok(value.clone())
        } else if let Some(method) = self.class.methods.get(property) {
            Ok(Value::Callable(Rc::new(method.bind(this))))
        } else {
            print_error(loc, &format!("Undefined property '{}'", property));
            Err(Error::UndefinedSymbol)
        }
    }

    fn try_get_method(&self, this: Rc<Object>, method: &str) -> Option<Callable> {
        if let Some(method) = self.class.methods.get(method) {
            Some(method.bind(this))
        } else {
            None
        }
    }

    fn set(&self, property: String, value: Value) {
        self.fields.borrow_mut().insert(property, value);
    }
}

impl Value {
    fn type_expected_error<T>(&self, loc: Loc, expected: &str) -> Result<T> {
        print_error(
            loc,
            &format!("Expected {expected} but found '{}'", self.convert_to_string(true)),
        );
        Err(Error::MismatchingType)
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
            Value::Null => false,
            _ => true,
        }
    }

    fn to_number(&self, loc: Loc) -> Result<f64> {
        match self {
            Value::Number(num) => Ok(*num),
            _ => self.type_expected_error(loc, "number"),
        }
    }

    fn to_callable(&self, loc: Loc) -> Result<&Callable> {
        match self {
            Value::Callable(callable) => Ok(callable),
            Value::Class(class) => Ok(&class.constructor),
            _ => self.type_expected_error(loc, "function or constructor"),
        }
    }

    fn to_instance(&self, loc: Loc) -> Result<&Rc<Object>> {
        match self {
            Value::Object(object) => Ok(object),
            _ => self.type_expected_error(loc, "object"),
        }
    }
}

pub struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    symbols: HashMap<String, Value>,
}

impl Scope {
    fn new(parent: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            symbols: HashMap::new(),
        }))
    }

    fn new_global() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: None,
            symbols: get_native_functions_as_symbols(),
        }))
    }

    fn define_symbol(&mut self, loc: Loc, name: String, value: Value) -> Result<()> {
        if !self.symbols.contains_key(&name) {
            self.symbols.insert(name, value);
            Ok(())
        } else {
            print_error(loc, &format!("Redeclaration of symbol '{}'", name));
            Err(Error::SymbolRedeclaration)
        }
    }

    fn get_symbol(&self, loc: Loc, name: &str) -> Result<Value> {
        if let Some(value) = self.symbols.get(name) {
            Ok(value.clone())
        } else {
            print_error(loc, &format!("Undefined symbol '{}'", name));
            Err(Error::UndefinedSymbol)
        }
    }

    fn get_symbol_at(&self, loc: Loc, name: &str, depth: i32) -> Result<Value> {
        if depth == 0 {
            self.get_symbol(loc, name)
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_symbol_at(loc, name, depth - 1)
        } else {
            panic!("Scope has no parent.");
        }
    }

    fn set_var(&mut self, loc: Loc, name: &str, value: Value) -> Result<()> {
        if let Some(var) = self.symbols.get_mut(name) {
            *var = value;
            Ok(())
        } else {
            print_error(loc, &format!("Assigning to undefined variable '{}'", name));
            Err(Error::UndefinedSymbol)
        }
    }

    fn set_var_at(&mut self, loc: Loc, name: &str, value: Value, depth: i32) -> Result<()> {
        if depth == 0 {
            self.set_var(loc, name, value)
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().set_var_at(loc, name, value, depth - 1)
        } else {
            panic!("Scope has no parent.");
        }
    }
}

pub struct Interpreter {
    global: Rc<RefCell<Scope>>,
    scope: Rc<RefCell<Scope>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let global = Scope::new_global();
        Self {
            scope: global.clone(),
            global,
        }
    }

    fn define_symbol(&self, loc: Loc, name: String, value: Value) -> Result<()> {
        self.scope.borrow_mut().define_symbol(loc, name, value)
    }

    fn get_symbol(&self, loc: Loc, var: &Var) -> Result<Value> {
        match var.scope.get() {
            VarScope::Global => self.global.borrow().get_symbol(loc, &var.name),
            VarScope::Relative(depth) => self.scope.borrow().get_symbol_at(loc, &var.name, depth),
        }
    }

    fn set_var(&mut self, loc: Loc, var: &Var, value: Value) -> Result<()> {
        match var.scope.get() {
            VarScope::Global => self.global.borrow_mut().set_var(loc, &var.name, value),
            VarScope::Relative(depth) => self.scope.borrow_mut().set_var_at(loc, &var.name, value, depth),
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
            TokenKind::Slash => {
                let denom = right.to_number(binary.right.loc)?;
                if denom == 0.0 {
                    print_error(binary.right.loc, "Division by 0");
                    Err(Error::DivisionByZero)
                } else {
                    Ok(Value::Number(left.to_number(binary.left.loc)? / denom))
                }
            },
            _ => panic!("Unexpected binary operand: {:?}", binary.op),
        }
    }

    fn eval_logical(&mut self, binary: &Binary) -> Result<Value> {
        let left = self.eval_expr(&binary.left)?;

        match binary.op {
            TokenKind::AndAnd => {
                if left.is_truthy() {
                    self.eval_expr(&binary.right)
                } else {
                    Ok(left)
                }
            },
            TokenKind::PipePipe => {
                if left.is_truthy() {
                    Ok(left)
                } else {
                    self.eval_expr(&binary.right)
                }
            },
            _ => panic!("Unexpected logical operand: {:?}", binary.op),
        }
    }

    fn call_callable(&mut self, callable: &Callable, args: Vec<Value>) -> Result<Value> {
        match &callable.fun {
            Function::Native(fun) => Ok(fun(args)),
            Function::Lox(LoxFun {
                is_initializer,
                params,
                body,
                closure,
            }) => {
                let new_scope = Scope::new(closure.clone());
                let prev_scope = mem::replace(&mut self.scope, new_scope);

                assert!(params.len() == args.len());
                for (param, arg) in params.iter().zip(args.into_iter()) {
                    self.define_symbol(param.loc, param.name.clone(), arg)?;
                }

                let result = match self.eval_stmts(body) {
                    Err(Error::Return(value)) => Ok(value),
                    Ok(_) => Ok(Value::Null),
                    Err(err) => Err(err),
                };

                self.scope = prev_scope;
                if *is_initializer {
                    Ok(closure.borrow().get_symbol(Loc::none(), "this").unwrap())
                } else {
                    result
                }
            },
            Function::Constructor(constructor) => {
                let object = Rc::new(Object {
                    class: constructor.class.upgrade().unwrap(), // TODO: is unwrapping ok?
                    fields: RefCell::new(HashMap::new()),
                });

                if let Some(initializer) = object.try_get_method(object.clone(), Class::INITIALIZER_METHOD) {
                    self.call_callable(&initializer, args)?;
                }

                Ok(Value::Object(object))
            },
        }
    }

    fn eval_call(&mut self, call: &Call) -> Result<Value> {
        let value = self.eval_expr(&call.callee)?;
        let callable = value.to_callable(call.callee.loc)?;

        if callable.arity != call.args.len() {
            print_error(
                call.callee.loc,
                &format!(
                    "Wrong number of arguments, expected {} but got {}",
                    callable.arity,
                    call.args.len()
                ),
            );
            return Err(Error::WrongArity);
        }

        let mut arg_values = Vec::new();
        for arg in &call.args {
            arg_values.push(self.eval_expr(arg)?);
        }

        self.call_callable(callable, arg_values)
    }

    fn eval_get(&mut self, get: &GetProp) -> Result<Value> {
        let value = self.eval_expr(&get.object)?;
        let object = value.to_instance(get.object.loc)?;
        let property = object.get(object.clone(), get.object.loc, &get.property)?;
        Ok(property.clone())
    }

    fn eval_set(&mut self, set: &SetProp) -> Result<Value> {
        let value = self.eval_expr(&set.object)?;
        let object = value.to_instance(set.object.loc)?;

        let set_value = self.eval_expr(&set.expr)?;
        object.set(set.property.clone(), set_value.clone());

        Ok(set_value)
    }

    fn eval_expr(&mut self, expr: &LocExpr) -> Result<Value> {
        match &expr.expr {
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            Expr::Logical(binary) => self.eval_logical(binary),
            Expr::Variable(var) | Expr::This(var) => Ok(self.get_symbol(expr.loc, &var)?),
            Expr::Assign(assign) => {
                let value = self.eval_expr(&assign.expr)?;
                self.set_var(expr.loc, &assign.var, value.clone())?;
                Ok(value)
            },
            Expr::Call(call) => self.eval_call(call),
            Expr::GetProp(get) => self.eval_get(get),
            Expr::SetProp(set) => self.eval_set(set),
        }
    }

    fn fun_to_callable(&self, fun: &FunDecl, is_method: bool) -> Rc<Callable> {
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
                let _ = self.eval_expr(expr)?;
            },
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
            Stmt::VarDecl(VarDecl { name_loc, name, init }) => {
                let value = self.eval_expr(init)?;
                self.define_symbol(*name_loc, name.clone(), value)?;
            },
            Stmt::FunDecl(fun) => {
                let value = Value::Callable(self.fun_to_callable(fun, false));
                self.define_symbol(fun.name_loc, fun.name.clone(), value)?;
            },
            Stmt::Return(Return { loc: _, expr }) => {
                let value = if let Some(expr) = expr {
                    self.eval_expr(expr)?
                } else {
                    Value::Null
                };
                return Err(Error::Return(value));
            },
            Stmt::ClassDecl(decl) => {
                let mut methods = HashMap::new();
                for method in &decl.methods {
                    let value = self.fun_to_callable(method, true);
                    methods.insert(method.name.clone(), value);
                }

                let arity = if let Some(initializer) = methods.get(Class::INITIALIZER_METHOD) {
                    initializer.arity
                } else {
                    0
                };

                let class = Value::Class(Rc::new_cyclic(|s| Class {
                    decl: decl.clone(),
                    methods,
                    constructor: Rc::new(Callable {
                        name: decl.name.clone(),
                        arity,
                        fun: Function::Constructor(Constructor { class: s.clone() }),
                    }),
                }));
                self.define_symbol(decl.name_loc, decl.name.clone(), class)?;
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
            Ok(_) => {},
        };

        Ok(())
    }
}
