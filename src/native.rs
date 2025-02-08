use std::{
    cell::RefCell,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use ahash::AHashMap;

use crate::{
    error::Loc,
    interpreter::Result,
    value::{Callable, Function, Value},
};

pub struct LocArg {
    pub loc: Loc,
    pub value: Value,
}

pub type NativeFun = fn(args: Vec<LocArg>) -> Result<Value>;

fn clock(args: Vec<LocArg>) -> Result<Value> {
    assert!(args.len() == 0);
    let secs = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64();
    Ok(Value::Number(secs))
}

fn copy_array(value: &Value) -> Value {
    match value {
        Value::Array(array) => {
            let mut copy = Vec::with_capacity(array.borrow().len());
            for elem in array.borrow().iter() {
                copy.push(copy_array(elem));
            }
            Value::Array(Rc::new(RefCell::new(copy)))
        },
        _ => value.clone(),
    }
}

fn new_array(args: Vec<LocArg>) -> Result<Value> {
    assert!(args.len() == 2);

    let len = args[0].value.get_index(args[0].loc, false)?;
    let mut arr = Vec::with_capacity(len);
    for _ in 0..len {
        arr.push(copy_array(&args[1].value));
    }
    Ok(Value::Array(Rc::new(RefCell::new(arr))))
}

const NATIVE_FUNCTIONS: [(&'static str, usize, NativeFun); 2] = [
    // name, arity, function
    ("clock", 0, clock),
    ("Array", 2, new_array),
];

pub fn create_hashmap_of_native_funs() -> AHashMap<String, Value> {
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
