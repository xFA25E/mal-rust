use im_rc::{vector, Vector};

use std::{
    env::args,
    io::{stderr, stdin, stdout, BufRead, Write},
    rc::Rc,
};

use crate::{
    core::ensure_len,
    env::Env,
    error as e,
    reader::read,
    value::{EvalResult, Value},
};

fn rep(s: &str, repl_env: Env) -> EvalResult {
    eval(read(s)?, repl_env)
}

pub fn main() -> std::io::Result<()> {
    let repl_env = Env::new().with_core();
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
            Value::List(args.map(Value::make_string).collect()),
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
                    writeln!(stdout().lock(), "{:?}", val).and_then(|_| stdout().lock().flush())?
                }
                Err(e) => {
                    writeln!(stderr().lock(), "{}", e).and_then(|_| stderr().lock().flush())?
                }
            }

            write!(stdout().lock(), "user> ").and_then(|_| stdout().lock().flush())?;
        }
    }
}

pub fn eval(mut ast: Value, mut env: Env) -> EvalResult {
    loop {
        if ast.list().is_none() {
            return eval_ast(ast, env);
        }

        ast = macroexpand(ast, env.clone())?;

        match ast {
            Value::List(form) => match form.get(0) {
                Some(Value::Symbol(s)) if s.as_ref() == "defmacro!" => {
                    ensure_len(form.len(), |n| n == 3, 2, "defmacro!")?;
                    let mut form_iter = form.iter();
                    form_iter.next();
                    if let Value::Symbol(sym) = form_iter.next().unwrap() {
                        let value = match eval(form_iter.next().unwrap().clone(), env.clone())? {
                            Value::Closure {
                                env,
                                binds,
                                body,
                                is_rest,
                            } => Value::Macro {
                                env,
                                binds,
                                body,
                                is_rest,
                            },
                            _ => return Err(e::arg_type("defmacro!", "function", 1)),
                        };
                        return Ok(env.set(Rc::clone(sym), value));
                    } else {
                        return Err(e::arg_type("defmacro!", "symbol", 0));
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
                        return Err(e::arg_type("def!", "symbol", 0));
                    }
                }

                Some(Value::Symbol(s)) if s.as_ref() == "let*" => {
                    ensure_len(form.len(), |n| n == 3, 2, "let*")?;

                    let mut form_iter = form.iter();
                    form_iter.next();
                    let bindings = form_iter.next().unwrap();
                    let body = form_iter.next().unwrap().clone();

                    let new_env = match bindings {
                        Value::Vector(binds) | Value::List(binds) => {
                            Env::with_env(env.clone()).let_binds(binds.clone())?
                        }
                        _ => return Err(e::arg_type("let*", "list or vector", 0)),
                    };

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
                            .map(Clone::clone)
                            .unwrap_or_else(|| Value::Nil),
                        _ => form_iter.next().unwrap().clone(),
                    };
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "fn*" => {
                    ensure_len(form.len(), |n| n == 3, 2, "fn*")?;

                    let mut form_iter = form.iter();
                    form_iter.next();

                    let binds_ref = form_iter
                        .next()
                        .and_then(Value::sequence)
                        .ok_or_else(|| e::arg_type("fn*", "list or vector", 0))?;

                    let mut iter = binds_ref.iter().enumerate();
                    let mut binds = Vector::new();
                    let mut is_rest = false;

                    while let Some((i, bind)) = iter.next() {
                        let sym = bind
                            .symbol()
                            .ok_or_else(|| e::arg_type("fn* args", "symbol", i))?;
                        if sym.as_ref() == "&" {
                            if binds_ref.len() - i != 2 {
                                return Err(e::rest_parameter());
                            }
                            is_rest = true;
                        } else {
                            binds.push_back(Rc::clone(sym));
                        }
                    }

                    return Ok(Value::Closure {
                        env,
                        binds,
                        body: Rc::new(form_iter.next().unwrap().clone()),
                        is_rest,
                    });
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "eval" => {
                    ensure_len(form.len(), |n| n == 2, 1, "eval")?;
                    ast = eval(form[1].clone(), env.clone())?;
                    env = env.most_outer();
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "quote" => {
                    ensure_len(form.len(), |n| n == 2, 2, "quote")?;
                    return Ok(form[1].clone());
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "quasiquote" => {
                    ensure_len(form.len(), |n| n == 2, 1, "quasiquote")?;
                    ast = quasiquote(form[1].clone());
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "macroexpand" => {
                    ensure_len(form.len(), |n| n == 2, 1, "macroexpand")?;
                    return macroexpand(form[1].clone(), env);
                }

                Some(Value::Symbol(ref s)) if s.as_ref() == "try*" => {
                    ensure_len(form.len(), |n| n == 3, 2, "try*")?;

                    match eval(form[1].clone(), env.clone()) {
                        Err(e) => match &form[2] {
                            Value::List(catch) => {
                                ensure_len(catch.len(), |n| n == 3, 2, "catch*")?;

                                match &catch[0] {
                                    Value::Symbol(s) if s.as_ref() == "catch*" => match &catch[1] {
                                        Value::Symbol(bind) => {
                                            env = Env::with_env(env);
                                            env.set(bind.clone(), e);
                                            ast = catch[2].clone();
                                        }
                                        _ => return Err(e::arg_type("catch*", "symbol", 0)),
                                    },
                                    _ => return e::catch_block(),
                                }
                            }
                            _ => return Err(e::arg_type("try*", "list", 1)),
                        },
                        other => return other,
                    }
                }

                Some(Value::Symbol(_)) | Some(Value::List(_)) => {
                    match eval_ast(Value::List(form.clone()), env.clone())? {
                        Value::List(mut list) => match list.pop_front().unwrap() {
                            Value::Function(func) => return func(list),
                            Value::Closure {
                                env: cenv,
                                binds,
                                body,
                                is_rest,
                            } => {
                                env = Env::with_env(cenv).fn_binds(binds, list, is_rest)?;
                                ast = body.as_ref().clone();
                            }

                            _ => return Err(e::not_function(&form[0])),
                        },
                        _ => panic!("eval bug: expected list got something else"),
                    }
                }

                Some(_) => return Err(e::not_function(&form[0])),
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
            .collect::<Result<_, _>>()
            .map(Value::List),
        Value::Vector(v) => v
            .iter()
            .map(|elm| eval(elm.clone(), env.clone()))
            .collect::<Result<_, _>>()
            .map(Value::Vector),
        Value::HashMap(h) => h
            .iter()
            .map(|(k, v)| Ok((k.clone(), eval(v.clone(), env.clone())?)))
            .collect::<Result<_, _>>()
            .map(Value::HashMap),
        other => Ok(other),
    }
}

// if ast = (ast-first ast-rest...)
// then
//     if ast = ("unquote" usomething)
//     then return usomething
//     else
//         if ast = (("splice-unquote" something) rest...)
//         then return (concat something (quasiquote rest))
//         else return (cons (quasiquote ast-first) (quasiquote ast-rest))
// else return (quote ast)
fn quasiquote(ast: Value) -> Value {
    if let Some(mut seq) = is_pair(&ast).map(Clone::clone) {
        match seq.pop_front().unwrap() {
            Value::Symbol(ref s) if s.as_ref() == "unquote" => seq.pop_front().unwrap(),
            ast_first => {
                if let Some(mut seq_first) = is_pair(&ast_first).map(|s| s.iter()) {
                    if let Value::Symbol(s) = seq_first.next().unwrap() {
                        if s.as_ref() == "splice-unquote" {
                            return Value::List(vector![
                                Value::make_symbol("concat"),
                                seq_first.next().unwrap().clone(),
                                quasiquote(Value::List(seq))
                            ]);
                        }
                    }
                }
                Value::List(vector![
                    Value::make_symbol("cons"),
                    quasiquote(ast_first),
                    quasiquote(Value::List(seq))
                ])
            }
        }
    } else {
        Value::List(vector![Value::make_symbol("quote"), ast])
    }
}

fn is_pair(ast: &Value) -> Option<&Vector<Value>> {
    ast.sequence().filter(|v| !v.is_empty())
}

fn macroexpand(mut ast: Value, env: Env) -> EvalResult {
    loop {
        if let Value::List(mut l) = ast.clone() {
            if let Some(Value::Macro {
                env: cenv,
                binds,
                body,
                is_rest,
            }) = l.get(0).and_then(Value::symbol).and_then(|s| env.get(s))
            {
                l.pop_front();
                let cenv = Env::with_env(cenv.clone()).fn_binds(binds, l, is_rest)?;
                ast = eval(body.as_ref().clone(), cenv)?;
                continue;
            }
        }
        break;
    }
    Ok(ast)
}
