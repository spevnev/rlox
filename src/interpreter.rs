use std::{cell::RefCell, collections::HashMap, mem, rc::Rc};

use crate::{
    error::{print_error, Loc},
    lexer::{Callable, Function, LoxFunction, TokenKind, Value},
    native::get_native_functions_as_symbols,
    parser::{Binary, Call, Expr, LocExpr, Stmt, Unary},
};

pub enum Error {
    MismatchingType,
    SymbolRedefinition,
    UndefinedSymbol,
    DivisionByZero,
    WrongArity,

    // The following aren't actual errors, and are used to quickly return from a deeply nested call:
    Return(Value),
}

type Result<V, E = Error> = std::result::Result<V, E>;

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

    fn to_number(&self, loc: Loc) -> Result<&f64> {
        match self {
            Value::Number(num) => Ok(num),
            _ => self.type_expected_error(loc, "number"),
        }
    }

    fn to_callable(&self, loc: Loc) -> Result<&Callable> {
        match self {
            Value::Callable(callable) => Ok(callable),
            _ => self.type_expected_error(loc, "function or constructor"),
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

    fn global() -> Rc<RefCell<Self>> {
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
            print_error(loc, &format!("Redefinition of symbol '{}'", name));
            Err(Error::SymbolRedefinition)
        }
    }

    fn get_symbol(&self, loc: Loc, name: &str) -> Result<Value> {
        if let Some(value) = self.symbols.get(name) {
            Ok(value.clone())
        } else if let Some(parent) = &self.parent {
            RefCell::borrow(parent).get_symbol(loc, name)
        } else {
            print_error(loc, &format!("Undefined symbol '{}'", name));
            Err(Error::UndefinedSymbol)
        }
    }

    fn set_var(&mut self, loc: Loc, name: &str, value: Value) -> Result<()> {
        if let Some(var) = self.symbols.get_mut(name) {
            *var = value;
            Ok(())
        } else if let Some(parent) = &self.parent {
            RefCell::borrow_mut(parent).set_var(loc, name, value)
        } else {
            print_error(loc, &format!("Assigning to undefined variable '{}'", name));
            Err(Error::UndefinedSymbol)
        }
    }
}

pub struct Interpreter {
    scope: Rc<RefCell<Scope>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scope: Scope::global(),
        }
    }

    fn eval_unary(&mut self, unary: &Unary) -> Result<Value> {
        let value = self.eval_expr(&unary.expr)?;

        match unary.op {
            TokenKind::Minus => Ok(Value::Number(-1.0 * value.to_number(unary.expr.loc)?.clone())),
            TokenKind::Bang => Ok(Value::Bool(!value.is_truthy())),
            _ => panic!("Unexpected unary operand: {:?}", unary.op),
        }
    }

    fn eval_binary(&mut self, binary: &Binary) -> Result<Value> {
        let left = self.eval_expr(&binary.left)?;
        let right = self.eval_expr(&binary.right)?;

        match binary.op {
            TokenKind::EqualEqual => Ok(Value::Bool(left == right)),
            TokenKind::BangEqual => Ok(Value::Bool(left != right)),
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
                    Ok(Value::String(
                        left.convert_to_string(false) + &right.convert_to_string(false),
                    ))
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
                if *denom == 0.0 {
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

        match &callable.fun {
            Function::Native(fun) => Ok(fun(arg_values)),
            Function::Lox(LoxFunction { decl, closure }) => {
                let new_scope = Scope::new(closure.clone());
                let prev_scope = mem::replace(&mut self.scope, new_scope);

                assert!(decl.params.len() == call.args.len());
                for (param_token, arg) in decl.params.iter().zip(call.args.iter()) {
                    let value = self.eval_expr(arg)?;
                    let param = param_token.to_identifier().expect("Parameter name must be an identifier.");
                    RefCell::borrow_mut(&self.scope).define_symbol(param_token.loc, param, value)?;
                }

                let result = match self.eval(&decl.body) {
                    Err(Error::Return(value)) => Ok(value),
                    Ok(_) => Ok(Value::Null),
                    Err(err) => Err(err),
                };

                self.scope = prev_scope;
                result
            },
        }
    }

    fn eval_expr(&mut self, expr: &LocExpr) -> Result<Value> {
        match &expr.expr {
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            Expr::Logical(binary) => self.eval_logical(binary),
            Expr::Var(var) => Ok(RefCell::borrow(&self.scope).get_symbol(expr.loc, &var)?),
            Expr::Assign(assign) => {
                let value = self.eval_expr(&assign.expr)?;
                RefCell::borrow_mut(&self.scope).set_var(expr.loc, &assign.var, value.clone())?;
                Ok(value)
            },
            Expr::Call(call) => self.eval_call(call),
        }
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
                        // Pop the scope on error and propagate it
                        self.scope = prev_scope;
                        return Err(err);
                    }
                }

                self.scope = prev_scope;
            },
            Stmt::If(condition, then_branch, else_branch) => {
                let result = self.eval_expr(condition)?;
                if result.is_truthy() {
                    self.eval_stmt(then_branch)?;
                } else if else_branch.is_some() {
                    self.eval_stmt(else_branch.as_ref().unwrap())?;
                }
            },
            Stmt::While(condition, body) => {
                while self.eval_expr(condition)?.is_truthy() {
                    self.eval_stmt(body)?;
                }
            },
            Stmt::VarDecl(name_token, init) => {
                let name = name_token.to_identifier().expect("Variable name must be an identifier.");
                let value = self.eval_expr(init)?;
                RefCell::borrow_mut(&self.scope).define_symbol(name_token.loc, name, value)?;
            },
            Stmt::FunDecl(name_token, decl) => {
                let name = name_token.to_identifier().expect("Function name must be an identifier.");
                let callable = Value::Callable(Rc::new(Callable {
                    name: name.clone(),
                    arity: decl.params.len(),
                    fun: Function::Lox(LoxFunction {
                        decl: decl.clone(),
                        closure: self.scope.clone(),
                    }),
                }));

                RefCell::borrow_mut(&self.scope).define_symbol(name_token.loc, name, callable)?;
            },
            Stmt::Return(expr) => {
                let value = self.eval_expr(expr)?;
                return Err(Error::Return(value));
            },
        };

        Ok(())
    }

    pub fn eval(&mut self, stmts: &Vec<Stmt>) -> Result<()> {
        for stmt in stmts {
            self.eval_stmt(stmt)?;
        }

        Ok(())
    }
}
