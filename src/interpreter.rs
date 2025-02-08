use std::{cell::RefCell, mem, rc::Rc};

use ahash::AHashMap;

use crate::{
    error::{error, Loc},
    lexer::TokenKind,
    native::{create_hashmap_of_native_funs, LocArg},
    parser::{
        Binary, Call, ClassDecl, Expr, For, FunDecl, GetElement, GetProp, If, LocExpr, Return, SetElement, SetProp,
        Stmt, Super, Superclass, Unary, Var, VarScope, While,
    },
    value::{Callable, Class, Function, Instance, LoxArray, LoxFun, Value},
};

pub enum Error {
    WrongType,
    UndefinedSymbol,
    WrongArity,
    IndexOutOfBounds,

    // The following aren't actual errors, and are used to quickly return from a deeply nested call:
    Return(Value),
    Break,
    Continue,
}

pub type Result<V, E = Error> = std::result::Result<V, E>;

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
            symbols: RefCell::new(create_hashmap_of_native_funs()),
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
            error!(loc, "Assigning to undefined variable '{name}'");
            Err(Error::UndefinedSymbol)
        }
    }
}

impl LoxFun {
    fn bind(&self, instance: Rc<Instance>) -> Self {
        let instance_closure = Scope::new(self.closure.clone());
        instance_closure.define_symbol("this".to_owned(), Value::Instance(instance));

        Self {
            is_initializer: self.is_initializer,
            is_getter: self.is_getter,
            params: self.params.clone(),
            body: self.body.clone(),
            closure: instance_closure,
        }
    }
}

impl Callable {
    fn bind(&self, instance: Rc<Instance>) -> Self {
        match &self.fun {
            Function::Native(_) => panic!("Cannot bind native function."),
            Function::Lox(fun) => Self {
                name: self.name.clone(),
                arity: self.arity,
                fun: Function::Lox(fun.bind(instance)),
            },
        }
    }
}

impl Value {
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
            _ => {
                error!(loc, "Expected number but found '{}'", self.error_to_string());
                Err(Error::WrongType)
            },
        }
    }

    fn get_instance(self, loc: Loc) -> Result<Rc<Instance>> {
        match self {
            Value::Instance(instance) => Ok(instance),
            Value::Class(class) => {
                if let Some(instance) = &class.static_instance {
                    Ok(instance.clone())
                } else {
                    error!(
                        loc,
                        "Properties/methods on class are only allowed for static methods, but '{}' doesn't have any",
                        class.name
                    );
                    Err(Error::WrongType)
                }
            },
            Value::Array(vec) => Ok(Rc::new(Instance {
                class: Rc::new(Class {
                    name: "Array".to_owned(),
                    methods: AHashMap::new(),
                    static_instance: None,
                    superclass: None,
                }),
                fields: RefCell::new(AHashMap::from([(
                    "length".to_owned(),
                    Value::Number(vec.borrow().len() as f64),
                )])),
            })),
            _ => {
                error!(
                    loc,
                    "Properties/methods only exist on instances but found '{}'",
                    self.error_to_string()
                );
                Err(Error::WrongType)
            },
        }
    }

    fn get_array(self, loc: Loc) -> Result<Rc<RefCell<LoxArray>>> {
        match self {
            Value::Array(array) => Ok(array),
            _ => {
                error!(loc, "Expected an array but found '{}'", self.error_to_string());
                Err(Error::WrongType)
            },
        }
    }

    /// Converts value (if it is `Value::Number`) to `usize` for length/index.
    pub fn get_index(&self, loc: Loc, is_index: bool) -> Result<usize> {
        if let Value::Number(len) = *self {
            if len >= 0.0 && len.fract() == 0.0 {
                return Ok(len as usize);
            }
        }

        error!(
            loc,
            "Expected {} to be a non-negative integer but found '{}'",
            if is_index { "index" } else { "length" },
            self.error_to_string(),
        );
        Err(Error::WrongType)
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
                    Ok(Value::String(left.convert_to_string() + &right.convert_to_string()))
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
            // Force `init` to always return the current instance.
            Ok(fun.closure.get_symbol("this").unwrap())
        } else {
            Ok(result)
        }
    }

    fn eval_callable(&mut self, call: &Call, callable: &Callable) -> Result<Value> {
        if call.args.len() != callable.arity {
            error!(
                call.callee.loc,
                "Expected {} arguments but got {}",
                callable.arity,
                call.args.len(),
            );
            return Err(Error::WrongArity);
        }

        match &callable.fun {
            Function::Native(fun) => {
                let mut args = Vec::with_capacity(call.args.len() + 1);
                for arg in &call.args {
                    args.push(LocArg {
                        loc: arg.loc,
                        value: self.eval_expr(arg)?,
                    });
                }
                fun(args)
            },
            Function::Lox(fun) => {
                let mut args = Vec::with_capacity(call.args.len() + 1);
                for arg in &call.args {
                    args.push(self.eval_expr(arg)?);
                }
                self.eval_lox_fun(fun, args)
            },
        }
    }

    fn eval_constructor(&mut self, call: &Call, class: Rc<Class>) -> Result<Value> {
        let instance = Rc::new(Instance {
            class: class.clone(),
            fields: RefCell::new(AHashMap::new()),
        });

        if let Some(initializer) = class.get_method(Class::INIT_METHOD) {
            self.eval_callable(call, &initializer.bind(instance.clone()))?;
        } else if !call.args.is_empty() {
            error!(
                call.callee.loc,
                "Class doesn't have an initializer, expected no arguments but got {}",
                call.args.len()
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
                error!(
                    call.callee.loc,
                    "Expected function or constructor but found '{}'",
                    value.error_to_string(),
                );
                Err(Error::WrongType)
            },
        }
    }

    fn eval_get(&mut self, loc: Loc, get: &GetProp) -> Result<Value> {
        let instance = self.eval_expr(&get.instance)?.get_instance(get.instance.loc)?;

        let fields = instance.fields.borrow();
        if let Some(value) = fields.get(&get.property) {
            Ok(value.clone())
        } else if let Some(method) = instance.class.get_method(&get.property) {
            match &method.fun {
                Function::Native(_) => panic!("Method cannot be a native function."),
                Function::Lox(fun) => {
                    if fun.is_getter {
                        self.eval_lox_fun(&fun.bind(instance.clone()), Vec::new())
                    } else {
                        Ok(Value::Callable(Rc::new(method.bind(instance.clone()))))
                    }
                },
            }
        } else {
            error!(loc, "Undefined property/method '{}'", &get.property);
            Err(Error::UndefinedSymbol)
        }
    }

    fn eval_set(&mut self, set: &SetProp) -> Result<Value> {
        let instance = self.eval_expr(&set.instance)?.get_instance(set.instance.loc)?;

        let set_value = self.eval_expr(&set.expr)?;
        instance.fields.borrow_mut().insert(set.property.clone(), set_value.clone());

        Ok(set_value)
    }

    fn eval_get_elem(&mut self, get: &GetElement) -> Result<Value> {
        let array = self.eval_expr(&get.array)?.get_array(get.array.loc)?;

        let index = self.eval_expr(&get.index)?.get_index(get.index.loc, true)?;
        if index < array.borrow().len() {
            Ok(array.borrow()[index].clone())
        } else {
            error(get.index.loc, "Index out of bounds");
            Err(Error::IndexOutOfBounds)
        }
    }

    fn eval_set_elem(&mut self, set: &SetElement) -> Result<Value> {
        let value = self.eval_expr(&set.expr)?;
        let array = self.eval_expr(&set.array)?.get_array(set.array.loc)?;

        let index = self.eval_expr(&set.index)?.get_index(set.index.loc, true)?;
        if index < array.borrow().len() {
            array.borrow_mut()[index] = value.clone();
            Ok(value)
        } else {
            error(set.index.loc, "Index out of bounds");
            Err(Error::IndexOutOfBounds)
        }
    }

    pub fn eval_expr(&mut self, LocExpr { loc, expr }: &LocExpr) -> Result<Value> {
        match expr {
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Grouping(expr) => self.eval_expr(expr),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            Expr::Ternary(ternary) => {
                let value = self.eval_expr(&ternary.condition)?;
                if value.is_truthy() {
                    self.eval_expr(&ternary.then_expr)
                } else {
                    self.eval_expr(&ternary.else_expr)
                }
            },
            Expr::Logical(binary) => self.eval_logical(binary),
            Expr::Variable(var) | Expr::This(var) => self.get_symbol(&var).ok_or_else(|| {
                error!(*loc, "Undefined symbol '{}'", var.name);
                Error::UndefinedSymbol
            }),
            Expr::Assign(assign) => {
                let value = self.eval_expr(&assign.expr)?;
                self.set_var(*loc, &assign.var, value.clone())?;
                Ok(value)
            },
            Expr::Call(call) => self.eval_call(call),
            Expr::GetProp(get) => self.eval_get(*loc, get),
            Expr::SetProp(set) => self.eval_set(set),
            Expr::Super(Super { var, method }) => {
                let Some(Value::Class(superclass)) = self.get_symbol(var) else {
                    // Resolver should handle this.
                    panic!("Super class must be a valid class.");
                };

                let Some(method) = superclass.get_method(&method) else {
                    error!(*loc, "Undefined superclass method '{method}'");
                    return Err(Error::UndefinedSymbol);
                };

                // Scopes are structured such that 'this' is always one scope closer than 'super',
                // which makes this hack possible.
                let VarScope::Relative(depth) = var.scope.get() else {
                    panic!("'super' mustn't be in global scope");
                };

                let value = self.get_nth_scope(depth - 1).get_symbol("this").unwrap();
                let Value::Instance(this) = value else {
                    panic!("'{}' must be an instance.", "this")
                };

                Ok(Value::Callable(Rc::new(method.bind(this))))
            },
            Expr::Lambda(lambda) => Ok(Value::Callable(Rc::new(Callable {
                name: None,
                arity: lambda.params.len(),
                fun: Function::Lox(LoxFun {
                    is_initializer: false,
                    is_getter: false,
                    params: lambda.params.clone(),
                    body: lambda.body.clone(),
                    closure: self.scope.clone(),
                }),
            }))),
            Expr::GetElement(get) => self.eval_get_elem(get),
            Expr::SetElement(set) => self.eval_set_elem(set),
            Expr::ArrayLiteral(exprs) => {
                let mut arr = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    arr.push(self.eval_expr(expr)?);
                }
                Ok(Value::Array(Rc::new(RefCell::new(arr))))
            },
        }
    }

    fn eval_fun_decl(&self, fun: &FunDecl, is_initializer: bool) -> Rc<Callable> {
        Rc::new(Callable {
            name: Some(fun.name.clone()),
            arity: fun.params.len(),
            fun: Function::Lox(LoxFun {
                is_initializer,
                is_getter: fun.is_getter,
                params: fun.params.clone(),
                body: fun.body.clone(),
                closure: self.scope.clone(),
            }),
        })
    }

    fn eval_class_decl(&mut self, decl: &ClassDecl) -> Result<()> {
        let superclass = if let Some(Superclass { loc, var }) = &decl.superclass {
            let super_value = self.get_symbol(var).ok_or_else(|| {
                error!(*loc, "Undefined class '{}'", var.name);
                Error::UndefinedSymbol
            })?;

            let Value::Class(super_class) = &super_value else {
                error!(
                    *loc,
                    "Expected superclass to be a class but found '{}'",
                    super_value.error_to_string()
                );
                return Err(Error::WrongType);
            };

            self.scope = Scope::new(self.scope.clone()); // add 'super' scope
            self.define_symbol("super".to_owned(), super_value.clone());

            Some(super_class.clone())
        } else {
            None
        };

        let mut methods = AHashMap::new();
        for method in &decl.methods {
            let value = self.eval_fun_decl(method, method.name == Class::INIT_METHOD);
            methods.insert(method.name.clone(), value);
        }

        if decl.superclass.is_some() {
            self.scope = self.scope.parent.clone().unwrap(); // remove 'super' scope
        }

        let static_instance = if !decl.static_methods.is_empty() {
            // Create an instance of metaclass which contains static methods of the current class.

            let mut static_methods = AHashMap::new();
            for method in &decl.static_methods {
                let value = self.eval_fun_decl(method, false);
                static_methods.insert(method.name.clone(), value);
            }

            let metaclass = Rc::new(Class {
                name: decl.name.clone() + " metaclass",
                methods: static_methods,
                static_instance: None,
                superclass: None,
            });

            Some(Rc::new(Instance {
                class: metaclass,
                fields: RefCell::new(AHashMap::new()),
            }))
        } else {
            None
        };

        let class = Value::Class(Rc::new(Class {
            name: decl.name.clone(),
            methods,
            static_instance,
            superclass,
        }));

        self.define_symbol(decl.name.clone(), class);

        Ok(())
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
            },
            Stmt::Print(expr) => println!("{}", self.eval_expr(expr)?.convert_to_string()),
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
                    match self.eval_stmt(body) {
                        Err(Error::Break) => break,
                        Err(Error::Continue) => {},
                        Ok(_) => {},
                        Err(err) => return Err(err),
                    };
                }
            },
            Stmt::For(For {
                initializer,
                condition,
                update,
                body,
            }) => {
                let new_scope = Scope::new(self.scope.clone());
                let prev_scope = mem::replace(&mut self.scope, new_scope);

                if let Some(initializer) = initializer {
                    self.eval_stmt(&initializer)?;
                }
                while self.eval_expr(condition)?.is_truthy() {
                    match self.eval_stmt(body) {
                        Err(Error::Break) => break,
                        Err(Error::Continue) => {},
                        Ok(_) => {},
                        Err(err) => return Err(err),
                    };

                    if let Some(update) = update {
                        self.eval_expr(update)?;
                    }
                }

                self.scope = prev_scope;
            },
            Stmt::Break(_) => return Err(Error::Break),
            Stmt::Continue(_) => return Err(Error::Continue),
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
            Stmt::ClassDecl(decl) => self.eval_class_decl(decl)?,
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
            Err(Error::Break) => panic!("Break outside of loop."),
            Err(Error::Continue) => panic!("Continue outside of loop."),
            Err(err) => return Err(err),
            Ok(_) => Ok(()),
        }
    }
}
