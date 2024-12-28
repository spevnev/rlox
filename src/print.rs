use crate::parser::{Expr, LocExpr, Stmt};

fn print_ast_rec(expr: &Expr) {
    match expr {
        Expr::Unary(unary) => {
            print!("({:?} ", unary.op);
            print_ast_rec(&unary.expr.expr);
            print!(")");
        }
        Expr::Binary(binary) => {
            print!("(");
            print_ast_rec(&binary.left.expr);
            print!(" {:?} ", binary.op);
            print_ast_rec(&binary.right.expr);
            print!(")");
        }
        Expr::Literal(value) => print!("{}", value.convert_to_string(true)),
    };
}

pub fn print_ast(stmts: &Vec<Stmt>) {
    for stmt in stmts {
        print!("{:?}", stmt.kind);
        print_ast_rec(&stmt.expr.expr);
        print!("\n");
    }
}
