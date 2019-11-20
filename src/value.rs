use std::{
    cell::RefCell,
    clone::Clone,
    cmp::PartialEq,
    collections::{HashMap, VecDeque},
    fmt::{self, Debug, Display},
    rc::Rc,
};

use crate::{core::EvalResult, env::Env, error as e, hashkey::LispHashKey};

pub enum Value {
    Nil,
    Bool(bool),
    Number(i128),
    Symbol(Rc<String>),
    Keyword(Rc<String>),
    String(Rc<String>),
    List(Rc<VecDeque<Value>>),
    Vector(Rc<Vec<Value>>),
    HashMap(Rc<HashMap<LispHashKey, Value>>),
    Function(fn(Rc<VecDeque<Value>>) -> EvalResult),
    Closure {
        env: Env,
        binds: Rc<Vec<Rc<String>>>,
        body: Rc<Value>,
        is_macro: bool,
    },
    Comment,
    Atom(Rc<RefCell<Value>>),
}

impl Value {
    pub fn number<F, N>(&self, func: F, pos: N) -> Result<i128, Value>
    where
        F: Display,
        N: Display,
    {
        match self {
            Value::Number(n) => Ok(*n),
            _ => e::arg_type(func, "number", pos),
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Nil => Self::Nil,
            Self::Bool(b) => Self::Bool(*b),
            Self::Number(n) => Self::Number(*n),
            Self::Symbol(s) => Self::Symbol(Rc::clone(s)),
            Self::Keyword(s) => Self::Keyword(Rc::clone(s)),
            Self::String(s) => Self::String(Rc::clone(s)),
            Self::List(l) => Self::List(Rc::clone(l)),
            Self::Vector(v) => Self::Vector(Rc::clone(v)),
            Self::HashMap(h) => Self::HashMap(Rc::clone(h)),
            Self::Function(fp) => Self::Function(*fp),
            Self::Closure {
                env,
                binds,
                body,
                is_macro,
            } => Self::Closure {
                env: env.clone(),
                binds: Rc::clone(binds),
                body: Rc::clone(body),
                is_macro: *is_macro,
            },
            Self::Comment => Self::Comment,
            Self::Atom(a) => Self::Atom(Rc::clone(a)),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Nil => match other {
                Self::Nil => true,
                _ => false,
            },
            Self::Bool(b) => match other {
                Self::Bool(o) => b == o,
                _ => false,
            },
            Self::Number(num) => match other {
                Self::Number(o) => num == o,
                _ => false,
            },
            Self::Symbol(string) => match other {
                Self::Symbol(o) => string == o,
                _ => false,
            },
            Self::Keyword(string) => match other {
                Self::Keyword(o) => string == o,
                _ => false,
            },
            Self::String(string) => match other {
                Self::String(o) => string == o,
                _ => false,
            },
            Self::List(list) => match other {
                Self::List(o) => list == o,
                Self::Vector(v) => list.as_ref() == v.as_ref(),
                _ => false,
            },
            Self::Vector(vector) => match other {
                Self::Vector(o) => vector == o,
                Self::List(l) => {
                    vector.len() == l.len() && vector.iter().zip(l.iter()).all(|(a, b)| a == b)
                }
                _ => false,
            },
            Self::HashMap(hash_map) => match other {
                Self::HashMap(o) => hash_map == o,
                _ => false,
            },
            Self::Function(fn_ptr) => match other {
                Self::Function(o) => fn_ptr == o,
                _ => false,
            },
            Self::Closure { .. } => false,
            Self::Atom(atom) => match other {
                Self::Atom(o) => atom == o,
                _ => false,
            },
            Self::Comment => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => write!(f, "{}", n),
            Self::Symbol(s) => write!(f, "{}", s),
            Self::Keyword(k) => write!(f, ":{}", k),
            Self::String(s) => write!(f, "{}", s),
            Self::Function(func) => write!(f, "#<function {:?}>", func),
            Self::Closure {
                binds,
                body,
                is_macro: false,
                ..
            } => write!(f, "#<closure {:?} {}>", binds, body),
            Self::Closure {
                binds,
                body,
                is_macro: true,
                ..
            } => write!(f, "#<macro {:?} {}>", binds, body),
            Self::HashMap(h) => {
                write!(f, "{{")?;
                display_seq(h.iter().map(HashKeyVal), f)?;
                write!(f, "}}")
            }
            Self::Vector(v) => {
                write!(f, "[")?;
                display_seq(v.iter(), f)?;
                write!(f, "]")
            }
            Self::List(l) => {
                write!(f, "(")?;
                display_seq(l.iter(), f)?;
                write!(f, ")")
            }
            Self::Atom(a) => write!(f, "{}", a.borrow()),
            Self::Comment => Ok(()),
        }
    }
}

impl Debug for Value {
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
            Self::Vector(v) => {
                write!(f, "[")?;
                debug_seq(v.iter(), f)?;
                write!(f, "]")
            }
            Self::List(l) => {
                write!(f, "(")?;
                debug_seq(l.iter(), f)?;
                write!(f, ")")
            }
            Self::HashMap(h) => {
                write!(f, "{{")?;
                debug_seq(h.iter().map(HashKeyVal), f)?;
                write!(f, "}}")
            }
            other => write!(f, "{}", other),
        }
    }
}

fn display_seq<P: Display, I: Iterator<Item = P>>(mut i: I, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(e) = i.next() {
        write!(f, "{}", e)?;
    }

    for e in i {
        write!(f, " {}", e)?;
    }

    Ok(())
}

fn debug_seq<P: Debug, I: Iterator<Item = P>>(mut i: I, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(e) = i.next() {
        write!(f, "{:?}", e)?;
    }

    for e in i {
        write!(f, " {:?}", e)?;
    }

    Ok(())
}

struct HashKeyVal<'a>((&'a LispHashKey, &'a Value));

impl<'a> Display for HashKeyVal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", (self.0).0, (self.0).1)
    }
}

impl<'a> Debug for HashKeyVal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {:?}", (self.0).0, (self.0).1)
    }
}

#[cfg(test)]
mod lisp_value {
    use super::*;

    #[test]
    fn print() {
        assert_eq!(Value::Number(123).to_string(), "123".to_string());
        assert_eq!(Value::Symbol("sym".into()).to_string(), "sym".to_string(),);
        assert_eq!(
            Value::List(
                vec![Value::Number(123), Value::Symbol("sym".into())]
                    .into_iter()
                    .collect()
            )
            .to_string(),
            "(123 sym)".to_string()
        );

        assert_eq!(
            Value::List(
                vec![
                    Value::Number(123),
                    Value::Symbol("sym".into()),
                    Value::List(
                        vec![Value::Number(123), Value::Symbol("sym".into())]
                            .into_iter()
                            .collect()
                    )
                ]
                .into_iter()
                .collect()
            )
            .to_string(),
            "(123 sym (123 sym))".to_string()
        );
    }
}
