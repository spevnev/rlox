use crate::parser::{Expr, LocExpr, Stmt};

fn print_expr(expr: &LocExpr) {
    match &expr.expr {
        Expr::Literal(value) => print!("{}", value.convert_to_string(true)),
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
        Expr::Var(id) => print!("{}", id),
        Expr::Assign(assign) => {
            print!("({} = ", assign.var);
            print_expr(&assign.expr);
            print!(")")
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
        Stmt::VarDecl(_, id, expr) => {
            print!("Var {} = ", id);
            print_expr(expr);
        }
        Stmt::Block(stmts) => {
            println!("{{");
            print_stmts(stmts);
            print!("}}");
        }
    }
}

pub fn print_stmts(stmts: &Vec<Stmt>) {
    for stmt in stmts {
        print_stmt(&stmt);
        print!("\n");
    }
}
