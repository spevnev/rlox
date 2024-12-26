use crate::parser::{Expr, LocExpr};

fn print_ast_rec(expr: &LocExpr) {
    match &expr.expr {
        Expr::Unary(unary) => {
            print!("({:?} ", unary.op);
            print_ast_rec(&unary.expr);
            print!(")");
        }
        Expr::Binary(binary) => {
            print!("(");
            print_ast_rec(&binary.left);
            print!(" {:?} ", binary.op);
            print_ast_rec(&binary.right);
            print!(")");
        }
        Expr::Literal(value) => print!("{}", value.convert_to_string()),
    };
}

pub fn print_ast(expr: &LocExpr) {
    print_ast_rec(expr);
    print!("\n");
}
