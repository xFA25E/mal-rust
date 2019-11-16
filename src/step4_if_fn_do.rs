use std::{
    collections::{HashMap, VecDeque},
    io::{stderr, stdin, stdout, BufRead, Write},
};

use crate::{
    env::Env,
    error::{EvalError, MalError},
    hashkey::LispHashKey,
    reader::read,
    value::Value,
};

fn rep(s: &str, repl_env: Env) -> Result<Value, MalError> {
    Ok(eval(read(s)?, repl_env)?)
}

pub fn main() -> Result<(), MalError> {
    let repl_env = Env::new().with_core().make();
    rep("(def! not (fn* (a) (if a false true)))", repl_env.clone())
        .expect("eval bug: in core mal function");

    let mut line = String::new();

    write!(stdout().lock(), "user> ")?;
    stdout().lock().flush()?;

    loop {
        line.clear();
        stdin().lock().read_line(&mut line)?;

        match rep(&line, repl_env.clone()) {
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
        Value::List(mut l) => match l.front() {
            Some(Value::Symbol(s)) if s == "def!" => {
                if l.len() == 3 {
                    l.pop_front();
                    if let Value::Symbol(sym) = l.pop_front().unwrap() {
                        Ok(env.set(sym, eval(l.pop_front().unwrap(), env.clone())?))
                    } else {
                        Err(EvalError::InvalidArgumentType)
                    }
                } else {
                    Err(EvalError::InvalidNumberOfArguments)
                }
            }

            Some(Value::Symbol(s)) if s == "let*" => {
                if l.len() == 3 {
                    l.pop_front();
                    let bindings = l.pop_front().unwrap();
                    let form = l.pop_front().unwrap();
                    let new_env = match bindings {
                        Value::List(binds) => {
                            bind(binds.into_iter(), Env::new().env(env.clone()).make())
                        }
                        Value::Vector(binds) => {
                            bind(binds.into_iter(), Env::new().env(env.clone()).make())
                        }
                        _ => Err(EvalError::InvalidArgumentType),
                    }?;

                    eval(form, new_env)
                } else {
                    Err(EvalError::InvalidNumberOfArguments)
                }
            }

            Some(Value::Symbol(s)) if s == "do" => {
                l.pop_front();
                l.into_iter()
                    .try_fold(Value::Nil, |_, next| eval(next, env.clone()))
            }

            Some(Value::Symbol(ref s)) if s == "if" => {
                if l.len() == 3 || l.len() == 4 {
                    l.pop_front();
                    let cond = l.pop_front().unwrap();
                    let then = l.pop_front().unwrap();
                    let otherwise = l.pop_front().unwrap_or_else(|| Value::Nil);
                    match eval(cond, env.clone())? {
                        Value::Nil | Value::Bool(false) => eval(otherwise, env),
                        _ => eval(then, env),
                    }
                } else {
                    Err(EvalError::InvalidNumberOfArguments)
                }
            }

            Some(Value::Symbol(ref s)) if s == "fn*" => {
                if l.len() == 3 {
                    l.pop_front();

                    let binds = match l.pop_front().unwrap() {
                        Value::List(bs) => {
                            let mut binds = Vec::with_capacity(bs.len());
                            for bind in bs.into_iter() {
                                match bind {
                                    Value::Symbol(s) => binds.push(s),
                                    _ => return Err(EvalError::InvalidArgumentType),
                                }
                            }
                            binds
                        }
                        Value::Vector(bs) => {
                            let mut binds = Vec::with_capacity(bs.len());
                            for bind in bs.into_iter() {
                                match bind {
                                    Value::Symbol(s) => binds.push(s),
                                    _ => return Err(EvalError::InvalidArgumentType),
                                }
                            }
                            binds
                        }
                        _ => return Err(EvalError::InvalidArgumentType),
                    };

                    let mut pos_iter = binds.iter();
                    if let Some(pos) = pos_iter.position(|p| p == "&") {
                        if pos != binds.len() - 2 {
                            return Err(EvalError::InvalidFnParameters);
                        }
                        if let Some("&") = pos_iter.next().map(|s| s.as_str()) {
                            return Err(EvalError::InvalidFnParameters);
                        }
                    }

                    let env = env.clone();
                    let body = Box::new(l.pop_front().unwrap());
                    Ok(Value::Closure { env, binds, body })
                } else {
                    Err(EvalError::InvalidNumberOfArguments)
                }
            }

            Some(Value::Symbol(_)) | Some(Value::List(_)) => {
                match eval_ast(Value::List(l), env.clone())? {
                    Value::List(mut list) => match list.pop_front().unwrap() {
                        Value::Function(func) => func(list),

                        Value::Closure {
                            env: cenv,
                            binds,
                            body,
                        } => {
                            if binds.len() != list.len() {
                                match binds.get(binds.len() - 2).map(|s| s.as_str()) {
                                    Some("&") => (),
                                    _ => return Err(EvalError::InvalidNumberOfArguments),
                                }
                            }

                            let closure_env =
                                Env::new().env(cenv.clone()).binds(&binds, list).make();
                            eval(*body, closure_env)
                        }

                        _ => Err(EvalError::FormIsNotCallable),
                    },
                    _ => panic!("eval bug: expected list got something else"),
                }
            }

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
                .map(|elm| eval(elm, env.clone()))
                .collect::<Result<VecDeque<Value>, EvalError>>()?,
        )),
        Value::Vector(v) => Ok(Value::Vector(
            v.into_iter()
                .map(|elm| eval(elm, env.clone()))
                .collect::<Result<Vec<Value>, EvalError>>()?,
        )),
        Value::HashMap(h) => Ok(Value::HashMap(
            h.into_iter()
                .map(|(k, v)| Ok((k, eval(v, env.clone())?)))
                .collect::<Result<HashMap<LispHashKey, Value>, EvalError>>()?,
        )),
        other => Ok(other),
    }
}

fn bind<I: Iterator<Item = Value>>(mut i: I, env: Env) -> Result<Env, EvalError> {
    while let Some(var) = i.next() {
        if let Value::Symbol(let_var) = var {
            let value = i.next();
            let value = value.ok_or_else(|| EvalError::InvalidNumberOfArguments)?;
            let value = eval(value, env.clone())?;
            env.set(let_var, value);
        } else {
            return Err(EvalError::InvalidArgumentType);
        }
    }

    Ok(env)
}
