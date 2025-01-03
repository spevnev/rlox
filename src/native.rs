use std::{
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::lexer::{Callable, Function, NativeFunction, Value};

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

const NATIVE_FUNCTIONS: [(&'static str, usize, NativeFunction); 2] = [
    // name, arity, function
    ("print", 1, print),
    ("clock", 0, clock),
];

pub fn get_native_functions_as_symbols() -> HashMap<String, Value> {
    HashMap::from(NATIVE_FUNCTIONS.map(|(name, arity, function)| {
        (
            name.to_owned(),
            Value::Callable(Rc::new(Callable {
                name: name.to_owned(),
                arity,
                fun: Function::Native(function),
            })),
        )
    }))
}
