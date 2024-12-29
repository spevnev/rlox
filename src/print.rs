use crate::parser::{Expr, LocExpr, Stmt};

fn print_expr(expr: &LocExpr) {
    match &expr.expr {
        Expr::Literal(value) => print!("{}", value.convert_to_string(true)),
        Expr::Var(id) => print!("{}", id),
        Expr::Unary(unary) => {
            print!("({:?} ", unary.op);
            print_expr(&unary.expr);
            print!(")");
        }
        Expr::Binary(binary) => {
            print!("(");
            print_expr(&binary.left);
            print!(" {:?} ", binary.op);
            print_expr(&binary.right);
            print!(")");
        }
    };
}

fn print_stmt(stmt: &Stmt) {
    match stmt {
        Stmt::Expr(expr) => print_expr(expr),
        Stmt::Print(expr) => {
            print!("Print ");
            print_expr(expr);
        }
        Stmt::Var(_, id, init_expr) => {
            print!("Var {} = ", id);
            print_expr(init_expr);
        }
    }
}

pub fn print_ast(stmts: &Vec<Stmt>) {
    for stmt in stmts {
        print_stmt(&stmt);
        print!("\n");
    }
}
