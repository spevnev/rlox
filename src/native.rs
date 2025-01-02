use std::time::{SystemTime, UNIX_EPOCH};

use crate::lexer::{NativeFunction, Value};

fn print(args: Vec<Value>) -> Value {
    assert!(args.len() == 1);
    println!("{}", args[0].convert_to_string(false));
    Value::Null
}

fn clock(args: Vec<Value>) -> Value {
    assert!(args.len() == 0);
    let secs = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64();
    Value::Number(secs)
}

pub const NATIVE_FUNCTIONS: [(&'static str, usize, NativeFunction); 2] = [
    // name, arity, function
    ("print", 1, print),
    ("clock", 0, clock),
];
