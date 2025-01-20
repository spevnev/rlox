use std::{
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use ahash::AHashMap;

use crate::value::{Callable, Function, NativeFun, Value};

fn clock(args: Vec<Value>) -> Value {
    assert!(args.len() == 0);
    let secs = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64();
    Value::Number(secs)
}

const NATIVE_FUNCTIONS: [(&'static str, usize, NativeFun); 1] = [
    // name, arity, function
    ("clock", 0, clock),
];

pub fn get_native_functions_as_symbols() -> AHashMap<String, Value> {
    AHashMap::from(NATIVE_FUNCTIONS.map(|(name, arity, fun)| {
        (
            name.to_owned(),
            Value::Callable(Rc::new(Callable {
                name: Some(name.to_owned()),
                arity,
                fun: Function::Native(fun),
            })),
        )
    }))
}
