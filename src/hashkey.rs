use std::{
    clone::Clone,
    convert::TryFrom,
    fmt::{self, Debug, Display},
    rc::Rc,
};

use crate::{error as e, value::Value};

#[derive(PartialEq, Eq, Hash)]
pub enum LispHashKey {
    Keyword(Rc<String>),
    String(Rc<String>),
    Symbol(Rc<String>),
    Number(i128),
    Bool(bool),
    Nil,
}

impl Clone for LispHashKey {
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

impl Display for LispHashKey {
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

impl Debug for LispHashKey {
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

impl TryFrom<Value> for LispHashKey {
    type Error = Value;
    fn try_from(source: Value) -> Result<Self, Self::Error> {
        match source {
            Value::Keyword(k) => Ok(LispHashKey::Keyword(k)),
            Value::String(s) => Ok(LispHashKey::String(s)),
            Value::Symbol(s) => Ok(LispHashKey::Symbol(s)),
            Value::Number(n) => Ok(LispHashKey::Number(n)),
            Value::Bool(b) => Ok(LispHashKey::Bool(b)),
            Value::Nil => Ok(LispHashKey::Nil),
            provided => e::invalid_hash_key(provided),
        }
    }
}

impl From<LispHashKey> for Value {
    fn from(source: LispHashKey) -> Self {
        match source {
            LispHashKey::Keyword(k) => Value::Keyword(k),
            LispHashKey::String(s) => Value::String(s),
            LispHashKey::Symbol(s) => Value::Symbol(s),
            LispHashKey::Number(n) => Value::Number(n),
            LispHashKey::Bool(b) => Value::Bool(b),
            LispHashKey::Nil => Value::Nil,
        }
    }
}
