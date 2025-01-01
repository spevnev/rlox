use std::time::{SystemTime, UNIX_EPOCH};

use crate::lexer::{Callable, Value};

fn print(args: Vec<Value>) -> Value {
    assert!(args.len() == 1);
    println!("{}", args[0].convert_to_string(false));
    Value::Null(())
}

fn clock(args: Vec<Value>) -> Value {
    assert!(args.len() == 0);
    let secs = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64();
    Value::Number(secs)
}

pub const NATIVE_FUNCTIONS: [(&'static str, Callable); 2] = [
    ("print", Callable { arity: 1, fun: print }),
    ("clock", Callable { arity: 0, fun: clock }),
];
