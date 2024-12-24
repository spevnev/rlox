use crate::{
    error::{print_error, Loc},
    lexer::{TokenKind, TokenValue},
    parser::{Binary, Expr, Unary},
};

impl TokenValue {
    fn expected_error(&self, expected_type: &str, loc: Loc) {
        print_error(
            loc,
            format!(
                "Expected {expected_type} but found '{}'",
                self.convert_to_string()
            ),
        );
    }

    fn is_string(&self) -> bool {
        match self {
            TokenValue::String(_) => true,
            _ => false,
        }
    }

    fn to_number(&self, loc: Loc) -> Result<f64, ()> {
        match self {
            TokenValue::Number(num) => Ok(num.clone()),
            _ => {
                self.expected_error("number", loc);
                Err(())
            }
        }
    }

    fn is_true(&self) -> bool {
        match self {
            TokenValue::Bool(bool) => *bool,
            TokenValue::Null(()) => false,
            _ => true,
        }
    }
}

fn eval_unary(unary: Unary) -> Result<TokenValue, ()> {
    let loc = unary.expr.loc();
    let value = eval(*unary.expr)?;

    match unary.op {
        TokenKind::Minus => Ok(TokenValue::Number(-value.to_number(loc)?)),
        TokenKind::Bang => Ok(TokenValue::Bool(!value.is_true())),
        _ => panic!("Unexpected unary operand: {:?}", unary.op),
    }
}

fn eval_binary(binary: Binary) -> Result<TokenValue, ()> {
    let left_loc = binary.left.loc();
    let left = eval(*binary.left)?;
    let right_loc = binary.right.loc();
    let right = eval(*binary.right)?;

    match binary.op {
        TokenKind::EqualEqual => Ok(TokenValue::Bool(left == right)),
        TokenKind::BangEqual => Ok(TokenValue::Bool(left != right)),
        TokenKind::Greater => Ok(TokenValue::Bool(
            left.to_number(left_loc)? > right.to_number(right_loc)?,
        )),
        TokenKind::GreaterEqual => Ok(TokenValue::Bool(
            left.to_number(left_loc)? >= right.to_number(right_loc)?,
        )),
        TokenKind::Less => Ok(TokenValue::Bool(
            left.to_number(left_loc)? < right.to_number(right_loc)?,
        )),
        TokenKind::LessEqual => Ok(TokenValue::Bool(
            left.to_number(left_loc)? <= right.to_number(right_loc)?,
        )),
        TokenKind::Plus => {
            if left.is_string() || right.is_string() {
                Ok(TokenValue::String(
                    left.convert_to_string() + &right.convert_to_string(),
                ))
            } else {
                Ok(TokenValue::Number(
                    left.to_number(left_loc)? + right.to_number(right_loc)?,
                ))
            }
        }
        TokenKind::Minus => Ok(TokenValue::Number(
            left.to_number(left_loc)? - right.to_number(right_loc)?,
        )),
        TokenKind::Star => Ok(TokenValue::Number(
            left.to_number(left_loc)? * right.to_number(right_loc)?,
        )),
        TokenKind::Slash => Ok(TokenValue::Number(
            left.to_number(left_loc)? / right.to_number(right_loc)?,
        )),
        _ => panic!("Unexpected binary operand: {:?}", binary.op),
    }
}

pub fn eval(expr: Expr) -> Result<TokenValue, ()> {
    match expr {
        Expr::Literal(literal) => Ok(literal.value),
        Expr::Unary(unary) => eval_unary(unary),
        Expr::Binary(binary) => eval_binary(binary),
    }
}
