use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use color_eyre::eyre::{eyre, Result};

use crate::{
    lexer::{Literal, Token},
    parser::Statement,
};

type EnvPtr = Rc<RefCell<Environment>>;

#[derive(Clone, Debug)]
pub enum Value {
    Literal(Literal),
    Function(Token, Vec<Token>, Box<Statement>),
    BuiltInFunction(String, Vec<String>, fn(usize, &[Value]) -> Result<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Literal(literal) => write!(f, "{}", literal),
            Value::Function(name, params, _) => {
                write!(f, "Function {}({:?})", name.lexeme, params)
            }
            Value::BuiltInFunction(name, params, _) => {
                write!(f, "BuiltInFunction {}({:?})", name, params)
            }
        }
    }
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        Value::Literal(literal)
    }
}

pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    variables: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> EnvPtr {
        Rc::new(RefCell::new(Environment {
            parent: None,
            variables: HashMap::new(),
        }))
    }

    pub fn with_parent(parent: &EnvPtr) -> EnvPtr {
        Rc::new(RefCell::new(Environment {
            parent: Some(Rc::clone(parent)),
            variables: HashMap::new(),
        }))
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<()> {
        if let std::collections::hash_map::Entry::Occupied(mut e) =
            self.variables.entry(name.to_owned())
        {
            e.insert(value);
            return Ok(());
        }

        if let Some(ref mut enclosing) = self.parent {
            return enclosing.borrow_mut().assign(name, value);
        }

        Err(eyre!("Undefined variable '{}'", name))
    }

    /// Walk up `distance` steps of `parent` pointers and return that `EnvPtr`.
    fn ancestor(env: &EnvPtr, distance: usize) -> EnvPtr {
        let mut current = Rc::clone(env);
        for _ in 0..distance {
            // borrow just to get the parent Rc, then drop the borrow
            let next = {
                let borrowed = current.borrow();
                borrowed
                    .parent
                    .as_ref()
                    .expect("No ancestor at that distance")
                    .clone()
            };
            current = next;
        }
        current
    }

    pub fn assign_at(env: &EnvPtr, distance: usize, name: &str, value: Value) -> Result<()> {
        let target_env = Environment::ancestor(env, distance);
        let mut targ = target_env.borrow_mut();

        let name = name.to_owned();
        if let std::collections::hash_map::Entry::Occupied(mut e) =
            targ.variables.entry(name.clone())
        {
            e.insert(value);
            Ok(())
        } else {
            Err(eyre!("Undefined variable '{}'", name))
        }
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.variables.insert(name.to_owned(), value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.variables.get(name) {
            Some(value.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }
}
