use std::{
    collections::HashMap,
    io::{stderr, stdin, stdout, BufRead, Write},
};

use crate::{
    reader::read_str,
    types::{EvalError, HashMapKey, MalError, ReadError, Value},
};

type LispReplEnv = HashMap<String, Value>;

fn read(s: &str) -> Result<Value, ReadError> {
    read_str(s)
}

fn eval(ast: Value, repl_env: &LispReplEnv) -> Result<Value, EvalError> {
    match ast {
        Value::List(l) => {
            if l.len() == 0 {
                Ok(Value::List(l))
            } else {
                let evaluated_ast = eval_ast(Value::List(l), repl_env)?;
                if let Value::List(list) = evaluated_ast {
                    if let Value::Function(func) = list[0] {
                        func(list[1..].to_vec())
                    } else {
                        Err(EvalError::FormIsNotCallable)
                    }
                } else {
                    panic!("during eval: expected list got something else")
                }
            }
        }
        other => eval_ast(other, repl_env),
    }
}

fn print(t: Value) -> String {
    format!("{:?}", t)
}

fn rep(s: &str, repl_env: &LispReplEnv) -> Result<String, MalError> {
    Ok(print(eval(read(s)?, repl_env)?))
}

pub fn main() -> Result<(), MalError> {
    let mut repl_env: LispReplEnv = HashMap::with_capacity(4);
    repl_env.insert("+".into(), Value::Function(plus));
    repl_env.insert("-".into(), Value::Function(minus));
    repl_env.insert("*".into(), Value::Function(multiply));
    repl_env.insert("/".into(), Value::Function(divide));

    let mut line = String::new();

    write!(stdout().lock(), "user> ")?;
    stdout().lock().flush()?;

    loop {
        line.clear();
        stdin().lock().read_line(&mut line)?;

        match rep(&line, &repl_env) {
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

fn eval_ast(ast: Value, repl_env: &LispReplEnv) -> Result<Value, EvalError> {
    match ast {
        Value::Symbol(ref s) => repl_env
            .get(s)
            .map(|c| c.clone())
            .ok_or_else(|| EvalError::SymbolNotFound),
        Value::List(l) => Ok(Value::List(
            l.into_iter()
                .map(|elm| eval(elm, repl_env))
                .collect::<Result<Vec<Value>, EvalError>>()?,
        )),
        Value::Vector(v) => Ok(Value::Vector(
            v.into_iter()
                .map(|elm| eval(elm, repl_env))
                .collect::<Result<Vec<Value>, EvalError>>()?,
        )),
        Value::HashMap(h) => Ok(Value::HashMap(
            h.into_iter()
                .map(|(k, v)| Ok((k, eval(v, repl_env)?)))
                .collect::<Result<HashMap<HashMapKey, Value>, EvalError>>()?,
        )),
        other => Ok(other),
    }
}

fn plus(args: Vec<Value>) -> Result<Value, EvalError> {
    if args.len() == 0 {
        Err(EvalError::InvalidNumberOfArguments)
    } else {
        args.iter()
            .try_fold(0, |acc, num| {
                if let Value::Number(n) = num {
                    Ok(acc + n)
                } else {
                    Err(EvalError::InvalidArgumentType)
                }
            })
            .map(Value::Number)
    }
}

fn minus(args: Vec<Value>) -> Result<Value, EvalError> {
    match args.len() {
        0 => Err(EvalError::InvalidNumberOfArguments),
        1 => {
            if let Value::Number(n) = args[0] {
                Ok(Value::Number(0 - n))
            } else {
                Err(EvalError::InvalidArgumentType)
            }
        }
        _ => {
            let first = if let Value::Number(n) = args[0] {
                n
            } else {
                return Err(EvalError::InvalidArgumentType);
            };

            args.iter()
                .skip(1)
                .try_fold(first, |acc, num| {
                    if let Value::Number(n) = num {
                        Ok(acc - n)
                    } else {
                        Err(EvalError::InvalidArgumentType)
                    }
                })
                .map(Value::Number)
        }
    }
}

fn multiply(args: Vec<Value>) -> Result<Value, EvalError> {
    if args.len() == 0 {
        Err(EvalError::InvalidNumberOfArguments)
    } else {
        args.iter()
            .try_fold(1, |acc, num| {
                if let Value::Number(n) = num {
                    Ok(acc * n)
                } else {
                    Err(EvalError::InvalidArgumentType)
                }
            })
            .map(Value::Number)
    }
}

fn divide(args: Vec<Value>) -> Result<Value, EvalError> {
    match args.len() {
        0 => Err(EvalError::InvalidNumberOfArguments),
        1 => {
            if let Value::Number(n) = args[0] {
                Ok(Value::Number(0 / n))
            } else {
                Err(EvalError::InvalidArgumentType)
            }
        }
        _ => {
            let first = if let Value::Number(n) = args[0] {
                n
            } else {
                return Err(EvalError::InvalidArgumentType);
            };

            args.iter()
                .skip(1)
                .try_fold(first, |acc, num| {
                    if let Value::Number(n) = num {
                        Ok(acc / n)
                    } else {
                        Err(EvalError::InvalidArgumentType)
                    }
                })
                .map(Value::Number)
        }
    }
}

#[cfg(test)]
mod eval_tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut repl_env = HashMap::with_capacity(4);
        repl_env.insert("+".to_string(), plus);

        match repl_env.get("+".into()).unwrap()(vec![Value::Number(1)]) {
            Ok(o) => println!("{:?}", o),
            Err(e) => println!("{}", e),
        }
    }
}
