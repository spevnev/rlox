use std::collections::HashMap;

use crate::{
    error::{error_expected, print_error, Loc},
    lexer::{TokenKind, Value},
    parser::{Binary, Expr, LocExpr, Stmt, Unary},
};

impl Value {
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
            Value::Number(num) => Ok(num.clone()),
            _ => error_expected("number", self, loc),
        }
    }

    pub fn to_identifier(&self, loc: Loc) -> Result<String, ()> {
        match self {
            Value::Identifier(id) => Ok(id.clone()),
            _ => error_expected("identifier", self, loc),
        }
    }

    pub fn convert_to_string(&self, quote_string: bool) -> String {
        match self {
            Value::Number(number) => number.to_string(),
            Value::String(string) => {
                if quote_string {
                    format!("\"{string}\"")
                } else {
                    string.to_owned()
                }
            }
            Value::Identifier(identifier) => identifier.to_owned(),
            Value::Bool(bool) => bool.to_string(),
            Value::Null(()) => "null".to_owned(),
        }
    }
}

pub struct Interpreter {
    vars: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            vars: HashMap::new(),
        }
    }

    fn define_var(&mut self, loc: Loc, id: String, value: Value) -> Result<(), ()> {
        if let None = self.vars.get(&id) {
            self.vars.insert(id, value);
            Ok(())
        } else {
            print_error(loc, &format!("Redefinition of variable '{}'", id));
            Err(())
        }
    }

    fn set_var(&mut self, loc: Loc, id: String, value: Value) -> Result<(), ()> {
        if let Some(var) = self.vars.get_mut(&id) {
            *var = value;
            Ok(())
        } else {
            print_error(loc, &format!("Assigning to undefined variable '{}'", id));
            Err(())
        }
    }

    fn get_var(&self, loc: Loc, id: &str) -> Result<&Value, ()> {
        if let Some(value) = self.vars.get(id) {
            Ok(value)
        } else {
            print_error(loc, &format!("Undefined variable '{}'", id));
            Err(())
        }
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
            }
            TokenKind::Minus => Ok(Value::Number(
                left.to_number(left_loc)? - right.to_number(right_loc)?,
            )),
            TokenKind::Star => Ok(Value::Number(
                left.to_number(left_loc)? * right.to_number(right_loc)?,
            )),
            TokenKind::Slash => {
                let denom = right.to_number(right_loc)?;
                if denom == 0.0 {
                    print_error(right_loc, "Division by 0");
                    Err(())
                } else {
                    Ok(Value::Number(left.to_number(left_loc)? / denom))
                }
            }
            _ => panic!("Unexpected binary operand: {:?}", binary.op),
        }
    }

    fn eval_expr(&mut self, expr: LocExpr) -> Result<Value, ()> {
        match expr.expr {
            Expr::Literal(value) => Ok(value),
            Expr::Var(id) => Ok(self.get_var(expr.loc, &id)?.clone()),
            Expr::Assign(assign) => {
                let value = self.eval_expr(*assign.expr)?;
                self.set_var(expr.loc, assign.var, value.clone())?;
                Ok(value)
            }
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
        }
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Result<(), ()> {
        match stmt {
            Stmt::Expr(expr) => {
                let _ = self.eval_expr(expr)?;
            }
            Stmt::Print(expr) => {
                let result = self.eval_expr(expr)?;
                println!("{}", result.convert_to_string(false));
            }
            Stmt::VarDecl(loc, id, expr) => {
                let value = self.eval_expr(expr)?;
                self.define_var(loc, id, value)?
            }
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
