use std::collections::HashMap;

use crate::{
    error::{error, Loc},
    lexer::{Callable, TokenKind, Value},
    native::NATIVE_FUNCTIONS,
    parser::{Binary, Call, Expr, LocExpr, Stmt, Unary},
};

impl Value {
    fn type_expected_error<T>(&self, loc: Loc, expected: &str) -> Result<T, ()> {
        error(
            loc,
            &format!(
                "Expected {expected} but found '{}'",
                self.convert_to_string(true)
            ),
        )
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
            Value::Null(()) => false,
            _ => true,
        }
    }

    fn to_number(&self, loc: Loc) -> Result<f64, ()> {
        match self {
            Value::Number(num) => Ok(*num),
            _ => self.type_expected_error(loc, "number"),
        }
    }

    fn to_callable(&self, loc: Loc) -> Result<Callable, ()> {
        match self {
            Value::Callable(callable) => Ok(callable.clone()),
            // TODO: function?
            _ => self.type_expected_error(loc, "callable"),
        }
    }
}

struct Scope {
    symbols: HashMap<String, Value>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            symbols: HashMap::new(),
        }
    }

    fn global() -> Scope {
        Scope {
            symbols: HashMap::from(
                NATIVE_FUNCTIONS
                    .map(|(name, callable)| (name.to_owned(), Value::Callable(callable))),
            ),
        }
    }
}

pub struct Interpreter {
    scopes: Vec<Scope>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            scopes: vec![Scope::global()],
        }
    }

    fn define_symbol(&mut self, loc: Loc, var: String, value: Value) -> Result<(), ()> {
        let scope = self.scopes.last_mut().unwrap();
        if !scope.symbols.contains_key(&var) {
            scope.symbols.insert(var, value);
            Ok(())
        } else {
            error(loc, &format!("Redefinition of variable/function '{}'", var))
        }
    }

    fn get_symbol(&self, loc: Loc, var: &str) -> Result<&Value, ()> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.symbols.get(var) {
                return Ok(value);
            }
        }

        error(loc, &format!("Undefined variable/function '{}'", var))
    }

    fn set_var(&mut self, loc: Loc, var: &str, value: Value) -> Result<(), ()> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(var) = scope.symbols.get_mut(var) {
                *var = value;
                return Ok(());
            }
        }

        error(loc, &format!("Assigning to undefined variable '{}'", var))
    }

    fn eval_unary(&mut self, unary: &Unary) -> Result<Value, ()> {
        let value = self.eval_expr(&unary.expr)?;

        match unary.op {
            TokenKind::Minus => Ok(Value::Number(-value.to_number(unary.expr.loc)?)),
            TokenKind::Bang => Ok(Value::Bool(!value.is_truthy())),
            _ => panic!("Unexpected unary operand: {:?}", unary.op),
        }
    }

    fn eval_binary(&mut self, binary: &Binary) -> Result<Value, ()> {
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
                if denom == 0.0 {
                    error(binary.right.loc, "Division by 0")
                } else {
                    Ok(Value::Number(left.to_number(binary.left.loc)? / denom))
                }
            },
            _ => panic!("Unexpected binary operand: {:?}", binary.op),
        }
    }

    fn eval_logical(&mut self, binary: &Binary) -> Result<Value, ()> {
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

    fn eval_call(&mut self, call: &Call) -> Result<Value, ()> {
        let callable = self.eval_expr(&call.callee)?.to_callable(call.callee.loc)?;

        if call.args.len() != usize::from(callable.arity) {
            return error(
                call.callee.loc,
                &format!(
                    "Wrong number of arguments, expected {} but got {}",
                    callable.arity,
                    call.args.len()
                ),
            );
        }

        let mut arg_values = Vec::new();
        for arg in &call.args {
            arg_values.push(self.eval_expr(arg)?);
        }

        Ok((callable.fun)(arg_values))
    }

    fn eval_expr(&mut self, expr: &LocExpr) -> Result<Value, ()> {
        match &expr.expr {
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            Expr::Logical(binary) => self.eval_logical(binary),
            Expr::Var(var) => Ok(self.get_symbol(expr.loc, &var)?.clone()),
            Expr::Assign(assign) => {
                let value = self.eval_expr(&assign.expr)?;
                self.set_var(expr.loc, &assign.var, value.clone())?;
                Ok(value)
            },
            Expr::Call(call) => self.eval_call(call),
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<(), ()> {
        match stmt {
            Stmt::Expr(expr) => {
                let _ = self.eval_expr(expr)?;
            },
            Stmt::VarDecl(var, expr) => {
                let value = self.eval_expr(expr)?;
                self.define_symbol(var.loc, var.to_identifier()?, value)?
            },
            Stmt::Block(stmts) => {
                self.scopes.push(Scope::new());

                for stmt in stmts {
                    self.eval_stmt(stmt)?;
                }

                self.scopes.pop();
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
        };

        Ok(())
    }

    pub fn eval(&mut self, stmts: Vec<Stmt>) -> Result<(), ()> {
        for stmt in stmts {
            self.eval_stmt(&stmt)?;
        }

        Ok(())
    }
}
