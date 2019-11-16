use std::{
    collections::{HashMap, VecDeque},
    io::{stderr, stdin, stdout, BufRead, Write},
    rc::Rc,
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
                write!(stdout().lock(), "{:?}\nuser> ", val)?;
                stdout().lock().flush()?;
            }
            Err(e) => {
                write!(stderr().lock(), "{}\nuser> ", e)?;
                stderr().lock().flush()?;
            }
        }
    }
}

pub fn eval(mut ast: Value, mut env: Env) -> Result<Value, EvalError> {
    loop {
        match ast {
            Value::List(form) => match form.front() {
                Some(Value::Symbol(s)) if s.as_ref() == "def!" => {
                    return if form.len() == 3 {
                        let mut form_iter = form.iter();
                        form_iter.next();
                        if let Value::Symbol(sym) = form_iter.next().unwrap() {
                            Ok(env.set(
                                Rc::clone(sym),
                                eval(form_iter.next().unwrap().clone(), env.clone())?,
                            ))
                        } else {
                            Err(EvalError::InvalidArgumentType)
                        }
                    } else {
                        Err(EvalError::InvalidNumberOfArguments)
                    };
                }

                Some(Value::Symbol(s)) if s.as_ref() == "let*" => {
                    if form.len() == 3 {
                        let mut form_iter = form.iter();
                        form_iter.next();
                        let bindings = form_iter.next().unwrap();
                        let body = form_iter.next().unwrap().clone();

                        let new_env = match bindings {
                            Value::List(binds) => {
                                bind(binds.iter(), Env::new().env(env.clone()).make())
                            }
                            Value::Vector(binds) => {
                                bind(binds.iter(), Env::new().env(env.clone()).make())
                            }
                            _ => Err(EvalError::InvalidArgumentType),
                        }?;

                        env = new_env;
                        ast = body;
                    } else {
                        return Err(EvalError::InvalidNumberOfArguments);
                    }
                }

                Some(Value::Symbol(s)) if s.as_ref() == "do" => {
                    let mut iter = form.iter().skip(1);
                    for _ in 0..form.len().saturating_sub(2) {
                        eval(iter.next().unwrap().clone(), env.clone())?;
                    }
                    ast = iter.next().map(|e| e.clone()).unwrap_or_else(|| Value::Nil);
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "if" => {
                    if form.len() == 3 || form.len() == 4 {
                        let mut form_iter = form.iter();
                        form_iter.next();
                        let cond = form_iter.next().unwrap().clone();
                        ast = match eval(cond, env.clone())? {
                            Value::Nil | Value::Bool(false) => form_iter
                                .nth(1)
                                .map(|v| v.clone())
                                .unwrap_or_else(|| Value::Nil),
                            _ => form_iter.next().unwrap().clone(),
                        };
                    } else {
                        return Err(EvalError::InvalidNumberOfArguments);
                    }
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "fn*" => {
                    return if form.len() == 3 {
                        let mut form_iter = form.iter();
                        form_iter.next();

                        let binds = match form_iter.next().unwrap() {
                            Value::List(bs) => {
                                let mut binds = Vec::with_capacity(bs.len());
                                for bind in bs.iter() {
                                    match bind {
                                        Value::Symbol(s) => binds.push(s.clone()),
                                        _ => return Err(EvalError::InvalidArgumentType),
                                    }
                                }
                                binds
                            }
                            Value::Vector(bs) => {
                                let mut binds = Vec::with_capacity(bs.len());
                                for bind in bs.iter() {
                                    match bind {
                                        Value::Symbol(s) => binds.push(s.clone()),
                                        _ => return Err(EvalError::InvalidArgumentType),
                                    }
                                }
                                binds
                            }
                            _ => return Err(EvalError::InvalidArgumentType),
                        };

                        let mut pos_iter = binds.iter();
                        if let Some(pos) = pos_iter.position(|p| p.as_ref() == "&") {
                            if pos != binds.len() - 2 {
                                return Err(EvalError::InvalidFnParameters);
                            }
                            if let Some("&") = pos_iter.next().map(|s| s.as_str()) {
                                return Err(EvalError::InvalidFnParameters);
                            }
                        }

                        Ok(Value::Closure {
                            env: env.clone(),
                            binds: Rc::new(binds),
                            body: Rc::new(form_iter.next().unwrap().clone()),
                        })
                    } else {
                        Err(EvalError::InvalidNumberOfArguments)
                    };
                }

                Some(Value::Symbol(_)) | Some(Value::List(_)) => {
                    match eval_ast(Value::List(form), env.clone())? {
                        Value::List(mut list) => {
                            let list_ref = Rc::make_mut(&mut list);
                            match list_ref.pop_front().unwrap() {
                                Value::Function(func) => return func(list),

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

                                    ast = body.as_ref().clone();
                                    env = closure_env;
                                }

                                _ => return Err(EvalError::FormIsNotCallable),
                            }
                        }
                        _ => panic!("eval bug: expected list got something else"),
                    }
                }

                Some(_) => return Err(EvalError::FormIsNotCallable),
                None => return Ok(Value::List(form)),
            },
            other => return eval_ast(other, env),
        }
    }
}

fn eval_ast(ast: Value, env: Env) -> Result<Value, EvalError> {
    match ast {
        Value::Symbol(s) => env.get(&s),
        Value::List(l) => Ok(Value::List(
            l.iter()
                .map(|elm| eval(elm.clone(), env.clone()))
                .collect::<Result<VecDeque<Value>, EvalError>>()
                .map(Rc::new)?,
        )),
        Value::Vector(v) => Ok(Value::Vector(
            v.iter()
                .map(|elm| eval(elm.clone(), env.clone()))
                .collect::<Result<Vec<Value>, EvalError>>()
                .map(Rc::new)?,
        )),
        Value::HashMap(h) => Ok(Value::HashMap(
            h.iter()
                .map(|(k, v)| Ok((k.clone(), eval(v.clone(), env.clone())?)))
                .collect::<Result<HashMap<LispHashKey, Value>, EvalError>>()
                .map(Rc::new)?,
        )),
        other => Ok(other),
    }
}

fn bind<'a, I: Iterator<Item = &'a Value>>(mut i: I, env: Env) -> Result<Env, EvalError> {
    while let Some(var) = i.next() {
        if let Value::Symbol(let_var) = var {
            let value = i.next();
            let value = value.ok_or_else(|| EvalError::InvalidNumberOfArguments)?;
            let value = eval(value.clone(), env.clone())?;
            env.set(Rc::clone(let_var), value);
        } else {
            return Err(EvalError::InvalidArgumentType);
        }
    }

    Ok(env)
}
