use crate::{
    error::{print_error, Loc},
    lexer::{TokenKind, Value},
    parser::{Binary, Expr, LocExpr, Unary},
};

impl Value {
    fn expected_error<T>(&self, expected_type: &str, loc: Loc) -> Result<T, ()> {
        print_error(
            loc,
            format!(
                "Expected {expected_type} but found '{}'",
                self.convert_to_string()
            ),
        );
        Err(())
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
            Value::Number(num) => Ok(num.clone()),
            _ => self.expected_error("number", loc),
        }
    }

    pub fn convert_to_string(&self) -> String {
        match self {
            Value::Number(number) => number.to_string(),
            Value::String(string) => string.to_owned(),
            Value::Identifier(identifier) => identifier.to_owned(),
            Value::Bool(bool) => bool.to_string(),
            Value::Null(()) => "null".to_owned(),
        }
    }
}

fn eval_unary(unary: Unary) -> Result<Value, ()> {
    let loc = unary.expr.loc;
    let value = eval(*unary.expr)?;

    match unary.op {
        TokenKind::Minus => Ok(Value::Number(-value.to_number(loc)?)),
        TokenKind::Bang => Ok(Value::Bool(!value.is_true())),
        _ => panic!("Unexpected unary operand: {:?}", unary.op),
    }
}

fn eval_binary(binary: Binary) -> Result<Value, ()> {
    let left_loc = binary.left.loc;
    let left = eval(*binary.left)?;
    let right_loc = binary.right.loc;
    let right = eval(*binary.right)?;

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
                    left.convert_to_string() + &right.convert_to_string(),
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
                print_error(right_loc, "Division by 0.".to_owned());
                Err(())
            } else {
                Ok(Value::Number(left.to_number(left_loc)? / denom))
            }
        }
        _ => panic!("Unexpected binary operand: {:?}", binary.op),
    }
}

pub fn eval(expr: LocExpr) -> Result<Value, ()> {
    match expr.expr {
        Expr::Literal(value) => Ok(value),
        Expr::Unary(unary) => eval_unary(unary),
        Expr::Binary(binary) => eval_binary(binary),
    }
}
