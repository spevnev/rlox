use std::time::{SystemTime, UNIX_EPOCH};

use crate::lexer::{NativeFunction, Value};

pub struct NativeFunDef {
    pub arity: usize,
    pub fun: NativeFunction,
}

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

pub const NATIVE_FUNCTIONS: [(&'static str, NativeFunDef); 2] = [
    ("print", NativeFunDef { arity: 1, fun: print }),
    ("clock", NativeFunDef { arity: 0, fun: clock }),
];
