use std::{
    clone::Clone,
    convert::TryFrom,
    fmt::{self, Debug, Display},
    rc::Rc,
};

use crate::{error as e, value::Value};

#[derive(PartialEq, Eq, Hash)]
pub enum HashMapKey {
    Keyword(Rc<String>),
    String(Rc<String>),
    Symbol(Rc<String>),
    Number(i128),
    Bool(bool),
    Nil,
}

impl Clone for HashMapKey {
    fn clone(&self) -> Self {
        match self {
            Self::Keyword(k) => Self::Keyword(Rc::clone(k)),
            Self::String(s) => Self::String(Rc::clone(s)),
            Self::Symbol(s) => Self::Symbol(Rc::clone(s)),
            Self::Number(n) => Self::Number(*n),
            Self::Bool(b) => Self::Bool(*b),
            Self::Nil => Self::Nil,
        }
    }
}

impl Display for HashMapKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Keyword(k) => write!(f, ":{}", k),
            Self::String(s) => write!(f, "{}", s),
            Self::Symbol(s) => write!(f, "{}", s),
            Self::Number(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
        }
    }
}

impl Debug for HashMapKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::String(s) => {
                write!(f, "\"")?;
                for char in s.chars() {
                    match char {
                        '\n' => write!(f, "\\n")?,
                        '\\' => write!(f, "\\\\")?,
                        '"' => write!(f, "\\\"")?,
                        c => write!(f, "{}", c)?,
                    }
                }
                write!(f, "\"")
            }
            other => write!(f, "{}", other),
        }
    }
}

impl TryFrom<Value> for HashMapKey {
    type Error = Value;
    fn try_from(source: Value) -> Result<Self, Self::Error> {
        match source {
            Value::Keyword(k) => Ok(HashMapKey::Keyword(k)),
            Value::String(s) => Ok(HashMapKey::String(s)),
            Value::Symbol(s) => Ok(HashMapKey::Symbol(s)),
            Value::Number(n) => Ok(HashMapKey::Number(n)),
            Value::Bool(b) => Ok(HashMapKey::Bool(b)),
            Value::Nil => Ok(HashMapKey::Nil),
            provided => e::invalid_hash_key(provided),
        }
    }
}

impl From<HashMapKey> for Value {
    fn from(source: HashMapKey) -> Self {
        match source {
            HashMapKey::Keyword(k) => Value::Keyword(k),
            HashMapKey::String(s) => Value::String(s),
            HashMapKey::Symbol(s) => Value::Symbol(s),
            HashMapKey::Number(n) => Value::Number(n),
            HashMapKey::Bool(b) => Value::Bool(b),
            HashMapKey::Nil => Value::Nil,
        }
    }
}
