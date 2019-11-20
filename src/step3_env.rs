use std::{
    collections::{HashMap, VecDeque},
    io::{stderr, stdin, stdout, BufRead, Write},
};

use crate::{
    env::Env,
    error::{EvalError, MalError},
    hashmapkey::HashMapKey,
    reader::read,
    value::Value,
};

fn print(t: Value) -> String {
    format!("{:?}", t)
}

fn rep(s: &str, repl_env: Env) -> Result<String, MalError> {
    Ok(print(eval(read(s)?, repl_env)?))
}

pub fn main() -> Result<(), MalError> {
    let repl_env = Env::with_builtins();
    let mut line = String::new();

    write!(stdout().lock(), "user> ")?;
    stdout().lock().flush()?;

    loop {
        line.clear();
        stdin().lock().read_line(&mut line)?;

        match rep(&line, repl_env.share()) {
            Ok(val) => {
                write!(stdout().lock(), "{}\nuser> ", val)?;
                stdout().lock().flush()?;
            }
            Err(e) => {
                write!(stderr().lock(), "{}\nuser> ", e)?;
                stderr().lock().flush()?;
            }
        }
    }
}

pub fn eval(ast: Value, env: Env) -> Result<Value, EvalError> {
    match ast {
        Value::List(mut l) => match l.pop_front() {
            Some(Value::Symbol(ref s)) if s == "def!" => {
                if l.len() == 2 {
                    if let Value::Symbol(sym) = l.pop_front().unwrap() {
                        Ok(env.set(sym, eval(l.pop_front().unwrap(), env.share())?))
                    } else {
                        Err(EvalError::InvalidArgumentType)
                    }
                } else {
                    Err(EvalError::InvalidNumberOfArguments)
                }
            }

            Some(Value::Symbol(ref s)) if s == "let*" => {
                if l.len() == 2 {
                    let bindings = l.pop_front().unwrap();
                    let form = l.pop_front().unwrap();
                    let new_env = match bindings {
                        Value::List(binds) => bind(binds.into_iter(), Env::with_env(env.share())),
                        Value::Vector(binds) => bind(binds.into_iter(), Env::with_env(env.share())),
                        _ => Err(EvalError::InvalidArgumentType),
                    }?;

                    eval(form, new_env)
                } else {
                    Err(EvalError::InvalidNumberOfArguments)
                }
            }

            Some(sym @ Value::Symbol(_)) => match eval_ast(sym, env.share())? {
                Value::Function(func) => match eval_ast(Value::List(l), env)? {
                    Value::List(list) => func(list),
                    _ => panic!("eval bug: expected list got something else"),
                },
                _ => Err(EvalError::FormIsNotCallable),
            },

            Some(_) => Err(EvalError::FormIsNotCallable),
            None => Ok(Value::List(l)),
        },
        other => eval_ast(other, env),
    }
}

fn eval_ast(ast: Value, env: Env) -> Result<Value, EvalError> {
    match ast {
        Value::Symbol(s) => env.get(s.as_ref()).map(|c| c.clone()),
        Value::List(l) => Ok(Value::List(
            l.into_iter()
                .map(|elm| eval(elm, env.share()))
                .collect::<Result<VecDeque<Value>, EvalError>>()?,
        )),
        Value::Vector(v) => Ok(Value::Vector(
            v.into_iter()
                .map(|elm| eval(elm, env.share()))
                .collect::<Result<Vec<Value>, EvalError>>()?,
        )),
        Value::HashMap(h) => Ok(Value::HashMap(
            h.into_iter()
                .map(|(k, v)| Ok((k, eval(v, env.share())?)))
                .collect::<Result<HashMap<HashMapKey, Value>, EvalError>>()?,
        )),
        other => Ok(other),
    }
}

fn bind<I: Iterator<Item = Value>>(mut i: I, env: Env) -> Result<Env, EvalError> {
    while let Some(var) = i.next() {
        if let Value::Symbol(let_var) = var {
            let value = i.next();
            let value = value.ok_or_else(|| EvalError::InvalidNumberOfArguments)?;
            let value = eval(value, env.share())?;
            env.set(let_var, value);
        } else {
            return Err(EvalError::InvalidArgumentType);
        }
    }

    Ok(env)
}
