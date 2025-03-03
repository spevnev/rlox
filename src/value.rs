use std::{cell::RefCell, rc::Rc};

use ahash::AHashMap;

use crate::{
    interpreter::Scope,
    native::NativeFun,
    parser::{FunParam, Stmt},
};

pub struct LoxFun {
    pub is_initializer: bool,
    pub is_getter: bool,
    pub params: Vec<FunParam>,
    pub body: Rc<Vec<Stmt>>,
    pub closure: Rc<Scope>,
}

pub enum Function {
    Lox(LoxFun),
    Native(NativeFun),
}

pub struct Callable {
    pub name: Option<String>,
    /// number of arguments
    pub arity: usize,
    pub fun: Function,
}

pub struct Class {
    pub name: String,
    pub methods: AHashMap<String, Rc<Callable>>,
    /// Instance of metaclass that contains static methods.
    pub static_instance: Option<Rc<Instance>>,
    pub superclass: Option<Rc<Class>>,
}

impl Class {
    pub const INIT_METHOD: &'static str = "init";

    pub fn get_method(&self, name: &str) -> Option<Rc<Callable>> {
        if let Some(method) = self.methods.get(name) {
            Some(method.clone())
        } else if let Some(superclass) = &self.superclass {
            superclass.get_method(name)
        } else {
            None
        }
    }
}

pub struct Instance {
    pub class: Rc<Class>,
    pub fields: RefCell<AHashMap<String, Value>>,
}

pub type LoxArray = Vec<Value>;

#[derive(Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Identifier(String),
    Callable(Rc<Callable>),
    Class(Rc<Class>),
    Instance(Rc<Instance>),
    Array(Rc<RefCell<LoxArray>>),
}

impl Value {
    pub fn convert_to_string(&self) -> String {
        match self {
            Value::Number(number) => number.to_string(),
            Value::String(string) => string.clone(),
            Value::Identifier(identifier) => identifier.clone(),
            Value::Bool(bool) => bool.to_string(),
            Value::Nil => "nil".to_owned(),
            Value::Callable(callable) => match &callable.name {
                Some(name) => format!("<fn {name}>"),
                None => "<fn>".to_owned(),
            },
            Value::Class(class) => class.name.clone(),
            Value::Instance(instance) => format!("{} instance", instance.class.name),
            Value::Array(values) => format!(
                "[{}]",
                values
                    .borrow()
                    .iter()
                    .map(|value| value.convert_to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }

    pub fn error_to_string(&self) -> String {
        match self {
            Value::String(string) => format!("\"{string}\""),
            _ => self.convert_to_string(),
        }
    }

    pub fn equals(&self, other: &Self) -> bool {
        match (self, other) {
            // by value
            (Value::Number(this), Value::Number(other)) => this == other,
            (Value::String(this), Value::String(other)) => this == other,
            (Value::Identifier(this), Value::Identifier(other)) => this == other,
            (Value::Bool(this), Value::Bool(other)) => this == other,
            (Value::Nil, Value::Nil) => true,
            // by pointer
            (Value::Callable(this), Value::Callable(other)) => Rc::ptr_eq(this, other),
            (Value::Class(this), Value::Class(other)) => Rc::ptr_eq(this, other),
            (Value::Instance(this), Value::Instance(other)) => Rc::ptr_eq(this, other),
            // Values of different types can't be equal.
            _ => false,
        }
    }
}
