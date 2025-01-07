use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use crate::{
    interpreter::Scope,
    parser::{ClassDecl, FunParam, Stmt},
};

pub type NativeFun = fn(args: Vec<Value>) -> Value;

pub struct LoxFun {
    pub is_initializer: bool,
    pub params: Vec<FunParam>,
    pub body: Rc<Vec<Stmt>>,
    pub closure: Rc<RefCell<Scope>>,
}

pub struct Constructor {
    pub class: Weak<Class>,
}

pub enum Function {
    Native(NativeFun),
    Lox(LoxFun),
    Constructor(Constructor),
}

pub struct Callable {
    pub name: String,
    /// number of arguments
    pub arity: usize,
    pub fun: Function,
}

pub struct Class {
    pub decl: Rc<ClassDecl>,
    pub methods: HashMap<String, Rc<Callable>>,
    pub constructor: Rc<Callable>,
}

impl Class {
    pub const INITIALIZER_METHOD: &'static str = "init";
}

pub struct Object {
    pub class: Rc<Class>,
    pub fields: RefCell<HashMap<String, Value>>,
}

#[derive(Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Identifier(String),
    Callable(Rc<Callable>),
    Class(Rc<Class>),
    Object(Rc<Object>),
}

impl Value {
    pub fn convert_to_string(&self, quote_string: bool) -> String {
        match self {
            Value::Number(number) => number.to_string(),
            Value::String(string) => {
                // Quoting differentiates identifier and string in error messages.
                if quote_string {
                    format!("\"{string}\"")
                } else {
                    string.clone()
                }
            },
            Value::Identifier(identifier) => identifier.clone(),
            Value::Bool(bool) => bool.to_string(),
            Value::Null => "null".to_owned(),
            Value::Callable(callable) => format!("<fun {}>", callable.name),
            Value::Class(class) => class.decl.name.clone(),
            Value::Object(object) => format!("<{} object>", object.class.decl.name),
        }
    }

    pub fn equals(&self, other: &Self) -> bool {
        match (self, other) {
            // by value
            (Value::Number(this), Value::Number(other)) => this == other,
            (Value::String(this), Value::String(other)) => this == other,
            (Value::Identifier(this), Value::Identifier(other)) => this == other,
            (Value::Bool(this), Value::Bool(other)) => this == other,
            (Value::Null, Value::Null) => true,
            // by pointer
            (Value::Callable(this), Value::Callable(other)) => Rc::ptr_eq(this, other),
            (Value::Class(this), Value::Class(other)) => Rc::ptr_eq(this, other),
            (Value::Object(this), Value::Object(other)) => Rc::ptr_eq(this, other),
            // Values of different types can't be equal.
            _ => false,
        }
    }
}
