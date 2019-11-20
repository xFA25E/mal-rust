use im_rc::{HashMap, Vector};

use std::{
    env::args,
    io::{stderr, stdin, stdout, BufRead, Write},
    rc::Rc,
};

use crate::{
    core::ensure_len,
    env::Env,
    error as e,
    hashmapkey::HashMapKey,
    reader::read,
    value::{EvalResult, Value},
};

fn rep(s: &str, repl_env: Env) -> EvalResult {
    eval(read(s)?, repl_env)
}

pub fn main() -> std::io::Result<()> {
    let repl_env = Env::new().with_core().make();
    let defs = &[
        "(def! not (fn* (a) (if a false true)))",
        "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))",
        "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))",
    ];
    for def in defs {
        rep(*def, repl_env.clone()).expect("eval bug: in core mal function");
    }

    let mut args = args().skip(1);

    if let Some(file) = args.next() {
        repl_env.set(
            Rc::new("*ARGS*".into()),
            Value::List(args.map(Rc::new).map(Value::String).collect()),
        );

        match rep(&format!("(load-file \"{}\")", file), repl_env) {
            Ok(val) => {
                println!("{:?}", val);
            }
            Err(e) => {
                eprintln!("{}", e);
            }
        }
        Ok(())
    } else {
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
}

pub fn eval(mut ast: Value, mut env: Env) -> EvalResult {
    loop {
        match ast.clone() {
            Value::List(_) => (),
            other => return eval_ast(other, env.clone()),
        }

        ast = macroexpand(ast, env.clone())?;

        match ast {
            Value::List(form) => match form.front() {
                Some(Value::Symbol(s)) if s.as_ref() == "defmacro!" => {
                    ensure_len(form.len(), |n| n == 3, 2, "defmacro!")?;
                    let mut form_iter = form.iter();
                    form_iter.next();
                    if let Value::Symbol(sym) = form_iter.next().unwrap() {
                        let value = match eval(form_iter.next().unwrap().clone(), env.clone())? {
                            Value::Closure {
                                env, binds, body, ..
                            } => Value::Closure {
                                env,
                                binds,
                                body,
                                is_macro: true,
                            },
                            value => value,
                        };
                        return Ok(env.set(Rc::clone(sym), value));
                    } else {
                        return e::arg_type("defmacro!", "symbol", 0);
                    }
                }

                Some(Value::Symbol(s)) if s.as_ref() == "def!" => {
                    ensure_len(form.len(), |n| n == 3, 2, "def!")?;
                    let mut form_iter = form.iter();
                    form_iter.next();
                    if let Value::Symbol(sym) = form_iter.next().unwrap() {
                        return Ok(env.set(
                            Rc::clone(sym),
                            eval(form_iter.next().unwrap().clone(), env.clone())?,
                        ));
                    } else {
                        return e::arg_type("def!", "symbol", 0);
                    }
                }

                Some(Value::Symbol(s)) if s.as_ref() == "let*" => {
                    ensure_len(form.len(), |n| n == 3, 2, "let*")?;

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
                        _ => e::arg_type("let*", "list or vector", 0),
                    }?;

                    env = new_env;
                    ast = body;
                }

                Some(Value::Symbol(s)) if s.as_ref() == "do" => {
                    ensure_len(form.len(), |n| n > 1, "1 or more", "do")?;

                    let mut iter = form.iter().skip(1);
                    for _ in 0..form.len() - 2 {
                        eval(iter.next().unwrap().clone(), env.clone())?;
                    }
                    ast = iter.next().unwrap().clone();
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "if" => {
                    ensure_len(form.len(), |n| n == 3 || n == 4, "3 or 4", "if")?;

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
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "fn*" => {
                    ensure_len(form.len(), |n| n == 3, 2, "fn*")?;

                    let mut form_iter = form.iter();
                    form_iter.next();

                    let binds = match form_iter.next().unwrap() {
                        Value::List(bs) => {
                            let mut binds = Vec::with_capacity(bs.len());
                            for (i, bind) in bs.iter().enumerate() {
                                match bind {
                                    Value::Symbol(s) => binds.push(s.clone()),
                                    _ => return e::arg_type("fn* args", "symbol", i),
                                }
                            }
                            binds
                        }
                        Value::Vector(bs) => {
                            let mut binds = Vec::with_capacity(bs.len());
                            for (i, bind) in bs.iter().enumerate() {
                                match bind {
                                    Value::Symbol(s) => binds.push(s.clone()),
                                    _ => return e::arg_type("fn* args", "symbol", i),
                                }
                            }
                            binds
                        }
                        _ => return e::arg_type("fn*", "list or vector", 0),
                    };

                    let mut pos_iter = binds.iter();
                    if let Some(pos) = pos_iter.position(|p| p.as_ref() == "&") {
                        if pos != binds.len() - 2 {
                            return e::rest_parameter();
                        }
                        if let Some("&") = pos_iter.next().map(|s| s.as_str()) {
                            return e::rest_parameter();
                        }
                    }

                    return Ok(Value::Closure {
                        env,
                        binds: Rc::new(binds),
                        body: Rc::new(form_iter.next().unwrap().clone()),
                        is_macro: false,
                    });
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "eval" => {
                    ensure_len(form.len(), |n| n == 2, 1, "eval")?;
                    ast = eval(form.get(1).unwrap().clone(), env.clone())?;
                    env = env.most_outer();
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "quote" => {
                    ensure_len(form.len(), |n| n == 2, 2, "quote")?;
                    return Ok(form.get(1).unwrap().clone());
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "quasiquote" => {
                    ensure_len(form.len(), |n| n == 2, 1, "quasiquote")?;
                    ast = quasiquote(form.get(1).unwrap().clone());
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "macroexpand" => {
                    ensure_len(form.len(), |n| n == 2, 1, "macroexpand")?;
                    return macroexpand(form.get(1).unwrap().clone(), env);
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "try*" => {
                    ensure_len(form.len(), |n| n == 3, 2, "try*")?;

                    match eval(form.get(1).unwrap().clone(), env.clone()) {
                        Err(e) => match form.get(2).unwrap() {
                            Value::List(catch) => {
                                ensure_len(catch.len(), |n| n == 3, 2, "catch*")?;

                                match catch.get(0).unwrap() {
                                    Value::Symbol(s) if s.as_ref() == "catch*" => {
                                        match catch.get(1).unwrap() {
                                            Value::Symbol(bind) => {
                                                let new_env = Env::new().env(env).make();
                                                new_env.set(bind.clone(), e);
                                                env = new_env;
                                                ast = catch.get(2).unwrap().clone();
                                            }
                                            _ => return e::arg_type("catch*", "symbol", 0),
                                        }
                                    }
                                    _ => return e::catch_block(),
                                }
                            }
                            _ => return e::arg_type("try*", "list", 1),
                        },
                        other => return other,
                    }
                }

                Some(Value::Symbol(_)) | Some(Value::List(_)) => {
                    match eval_ast(Value::List(form.clone()), env.clone())? {
                        Value::List(list) => {
                            let mut list = list.clone();
                            match list.pop_front().unwrap() {
                                Value::Function(func) => return func(list),

                                Value::Closure {
                                    env: cenv,
                                    binds,
                                    body,
                                    ..
                                } => {
                                    if binds.len() != list.len() {
                                        match binds.get(binds.len() - 2).map(|s| s.as_str()) {
                                            Some("&") => (),
                                            _ => {
                                                return e::rest_parameter();
                                            }
                                        }
                                    }

                                    let closure_env = Env::new()
                                        .env(cenv.clone())
                                        .binds(
                                            &binds,
                                            list.iter()
                                                .map(|e| eval(e.clone(), env.clone()))
                                                .collect::<Result<_, _>>()?,
                                        )
                                        .make();

                                    ast = body.as_ref().clone();
                                    env = closure_env;
                                }

                                _ => return e::not_function(form.get(0).unwrap()),
                            }
                        }
                        _ => panic!("eval bug: expected list got something else"),
                    }
                }

                Some(_) => return e::not_function(form.get(0).unwrap()),
                None => return Ok(Value::List(form)),
            },
            other => return eval_ast(other, env),
        }
    }
}

fn eval_ast(ast: Value, env: Env) -> EvalResult {
    match ast {
        Value::Symbol(s) => env.get(&s).ok_or_else(|| e::symbol_not_found(s)),
        Value::List(l) => l
            .iter()
            .map(|elm| eval(elm.clone(), env.clone()))
            .collect::<Result<Vector<Value>, Value>>()
            .map(Value::List),
        Value::Vector(v) => v
            .iter()
            .map(|elm| eval(elm.clone(), env.clone()))
            .collect::<Result<Vector<Value>, Value>>()
            .map(Value::Vector),
        Value::HashMap(h) => h
            .iter()
            .map(|(k, v)| Ok((k.clone(), eval(v.clone(), env.clone())?)))
            .collect::<Result<HashMap<HashMapKey, Value>, Value>>()
            .map(Value::HashMap),
        other => Ok(other),
    }
}

fn bind<'a, I: Iterator<Item = &'a Value>>(mut i: I, env: Env) -> Result<Env, Value> {
    let mut inx = 0;
    while let Some(var) = i.next() {
        if let Value::Symbol(let_var) = var {
            let value = match i.next() {
                Some(value) => eval(value.clone(), env.clone())?,
                None => return e::arg_count("_", "_", "_"),
            };

            env.set(Rc::clone(let_var), value);
        } else {
            return e::arg_type("_", "symbol", inx * 2);
        }
        inx += 1;
    }

    Ok(env)
}

// if ast = (ast-first ast-rest...)
// then
//     if ast = (unquote usomething)
//     then return usomething
//     else
//         if ast = ((splice-unquote something) rest...)
//         then return (concat something (quasiquote rest))
//         else return (cons (quasiquote ast-first) (quasiquote ast-rest))
// else return (quote ast)
fn quasiquote(ast: Value) -> Value {
    if is_pair(&ast) {
        let mut ast_iter: Box<dyn Iterator<Item = Value>> = match ast {
            Value::List(l) => Box::new(l.clone().into_iter()),
            Value::Vector(v) => Box::new(v.clone().into_iter()),
            _ => unreachable!(),
        };

        match ast_iter.next().unwrap() {
            Value::Symbol(ref s) if s.as_ref() == "unquote" => ast_iter.next().unwrap(),
            ast_first => {
                if is_pair(&ast_first) {
                    let mut first_iter: Box<dyn Iterator<Item = Value>> = match ast_first.clone() {
                        Value::List(l) => Box::new(l.clone().into_iter()),
                        Value::Vector(v) => Box::new(v.clone().into_iter()),
                        _ => unreachable!(),
                    };

                    if let Value::Symbol(s) = first_iter.next().unwrap() {
                        if s.as_ref() == "splice-unquote" {
                            let mut res = Vector::new();
                            res.push_back(Value::Symbol(Rc::new("concat".into())));
                            res.push_back(first_iter.next().unwrap().clone());
                            res.push_back(quasiquote(Value::List(ast_iter.collect())));
                            return Value::List(res);
                        }
                    }
                }
                let mut res = Vector::new();
                res.push_back(Value::Symbol(Rc::new("cons".into())));
                res.push_back(quasiquote(ast_first));
                res.push_back(quasiquote(Value::List(ast_iter.collect())));
                Value::List(res)
            }
        }
    } else {
        let mut res = Vector::new();
        res.push_back(Value::Symbol(Rc::new("quote".into())));
        res.push_back(ast);
        Value::List(res)
    }
}

fn is_pair(ast: &Value) -> bool {
    match ast {
        Value::List(l) => !l.is_empty(),
        Value::Vector(v) => !v.is_empty(),
        _ => false,
    }
}

fn macroexpand(mut ast: Value, env: Env) -> EvalResult {
    loop {
        if let Value::List(list) = ast.clone() {
            if let Some(Value::Symbol(ref s)) = list.get(0) {
                if let Some(Value::Closure {
                    env: cenv,
                    binds,
                    body,
                    is_macro: true,
                }) = env.get(s)
                {
                    let mut list = list.clone();
                    list.pop_front();

                    if binds.len() != list.len() {
                        match binds.get(binds.len() - 2).map(|s| s.as_str()) {
                            Some("&") => (),
                            _ => return e::rest_parameter(),
                        }
                    }

                    let closure_env = Env::new().env(cenv.clone()).binds(&binds, list).make();

                    ast = eval(body.as_ref().clone(), closure_env)?;
                    continue;
                }
            }
        }
        break;
    }
    Ok(ast)
}
