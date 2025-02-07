use std::{
    cell::RefCell,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use ahash::AHashMap;

use crate::{
    error::{error, Loc},
    interpreter::{Error, Result},
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

fn new_array(args: Vec<LocArg>) -> Result<Value> {
    assert!(args.len() == 2);

    let Value::Number(len) = args[0].value else {
        error!(
            args[0].loc,
            "Expected length to be a non-negative integer but found '{}'",
            args[0].value.error_to_string(),
        );
        return Err(Error::WrongType);
    };
    if len < 0.0 || len.fract() != 0.0 {
        error!(
            args[0].loc,
            "Expected length to be a non-negative integer but found '{}'",
            args[0].value.error_to_string(),
        );
        return Err(Error::WrongType);
    }

    let value = args[1].value.clone();
    Ok(Value::Array(Rc::new(RefCell::new(vec![value; len as usize]))))
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
