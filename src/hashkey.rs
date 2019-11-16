use std::{
    convert::TryFrom,
    fmt::{self, Debug, Display},
};

use crate::{error::ReadError, value::Value};

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum LispHashKey {
    Keyword(String),
    String(String),
    Symbol(String),
    Number(i128),
    Bool(bool),
    Nil,
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
    type Error = ReadError;
    fn try_from(source: Value) -> Result<Self, Self::Error> {
        match source {
            Value::Keyword(k) => Ok(LispHashKey::Keyword(k)),
            Value::String(s) => Ok(LispHashKey::String(s)),
            Value::Symbol(s) => Ok(LispHashKey::Symbol(s)),
            Value::Number(n) => Ok(LispHashKey::Number(n)),
            Value::Bool(b) => Ok(LispHashKey::Bool(b)),
            Value::Nil => Ok(LispHashKey::Nil),
            _ => Err(ReadError::InvalidHashKey),
        }
    }
}
