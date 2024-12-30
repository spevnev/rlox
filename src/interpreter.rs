use std::collections::HashMap;

use crate::{
    error::{error, Loc},
    lexer::{TokenKind, Value},
    parser::{Binary, Expr, LocExpr, Stmt, Unary},
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

    fn is_true(&self) -> bool {
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
}

struct Scope {
    vars: HashMap<String, Value>,
}

pub struct Interpreter {
    scopes: Vec<Scope>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            scopes: vec![Scope { vars: HashMap::new() }],
        }
    }

    fn define_var(&mut self, loc: Loc, var: String, value: Value) -> Result<(), ()> {
        let scope = self.scopes.last_mut().unwrap();
        if !scope.vars.contains_key(&var) {
            scope.vars.insert(var, value);
            Ok(())
        } else {
            error(loc, &format!("Redefinition of variable '{}'", var))
        }
    }

    fn set_var(&mut self, loc: Loc, var: &str, value: Value) -> Result<(), ()> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(var) = scope.vars.get_mut(var) {
                *var = value;
                return Ok(());
            }
        }

        error(loc, &format!("Assigning to undefined variable '{}'", var))
    }

    fn get_var(&self, loc: Loc, var: &str) -> Result<&Value, ()> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.vars.get(var) {
                return Ok(value);
            }
        }

        error(loc, &format!("Undefined variable '{}'", var))
    }

    fn eval_unary(&mut self, unary: Unary) -> Result<Value, ()> {
        let loc = unary.expr.loc;
        let value = self.eval_expr(*unary.expr)?;

        match unary.op {
            TokenKind::Minus => Ok(Value::Number(-value.to_number(loc)?)),
            TokenKind::Bang => Ok(Value::Bool(!value.is_true())),
            _ => panic!("Unexpected unary operand: {:?}", unary.op),
        }
    }

    fn eval_binary(&mut self, binary: Binary) -> Result<Value, ()> {
        let left_loc = binary.left.loc;
        let left = self.eval_expr(*binary.left)?;
        let right_loc = binary.right.loc;
        let right = self.eval_expr(*binary.right)?;

        match binary.op {
            TokenKind::EqualEqual => Ok(Value::Bool(left == right)),
            TokenKind::BangEqual => Ok(Value::Bool(left != right)),
            TokenKind::Greater => Ok(Value::Bool(
                left.to_number(left_loc)? > right.to_number(right_loc)?,
            )),
            TokenKind::GreaterEqual => Ok(Value::Bool(
                left.to_number(left_loc)? >= right.to_number(right_loc)?,
            )),
            TokenKind::Less => Ok(Value::Bool(
                left.to_number(left_loc)? < right.to_number(right_loc)?,
            )),
            TokenKind::LessEqual => Ok(Value::Bool(
                left.to_number(left_loc)? <= right.to_number(right_loc)?,
            )),
            TokenKind::Plus => {
                if left.is_string() || right.is_string() {
                    Ok(Value::String(
                        left.convert_to_string(false) + &right.convert_to_string(false),
                    ))
                } else {
                    Ok(Value::Number(
                        left.to_number(left_loc)? + right.to_number(right_loc)?,
                    ))
                }
            },
            TokenKind::Minus => Ok(Value::Number(
                left.to_number(left_loc)? - right.to_number(right_loc)?,
            )),
            TokenKind::Star => Ok(Value::Number(
                left.to_number(left_loc)? * right.to_number(right_loc)?,
            )),
            TokenKind::Slash => {
                let denom = right.to_number(right_loc)?;
                if denom == 0.0 {
                    error(right_loc, "Division by 0")
                } else {
                    Ok(Value::Number(left.to_number(left_loc)? / denom))
                }
            },
            _ => panic!("Unexpected binary operand: {:?}", binary.op),
        }
    }

    fn eval_expr(&mut self, expr: LocExpr) -> Result<Value, ()> {
        match expr.expr {
            Expr::Literal(value) => Ok(value),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            Expr::Var(var) => Ok(self.get_var(expr.loc, &var)?.clone()),
            Expr::Assign(assign) => {
                let value = self.eval_expr(*assign.expr)?;
                self.set_var(expr.loc, &assign.var, value.clone())?;
                Ok(value)
            },
        }
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Result<(), ()> {
        match stmt {
            Stmt::Expr(expr) => {
                let _ = self.eval_expr(expr)?;
            },
            Stmt::Print(expr) => {
                let result = self.eval_expr(expr)?;
                println!("{}", result.convert_to_string(false));
            },
            Stmt::VarDecl(var, expr) => {
                let value = self.eval_expr(expr)?;
                self.define_var(var.loc, var.to_identifier()?, value)?
            },
            Stmt::Block(stmts) => {
                self.scopes.push(Scope { vars: HashMap::new() });

                for stmt in stmts {
                    self.eval_stmt(stmt)?;
                }

                self.scopes.pop();
            },
        };

        Ok(())
    }

    pub fn eval(&mut self, stmts: Vec<Stmt>) -> Result<(), ()> {
        for stmt in stmts {
            self.eval_stmt(stmt)?;
        }

        Ok(())
    }
}
