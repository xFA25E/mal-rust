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
            Value::make_list(args.map(Value::make_string).collect()),
        );

        match rep(&format!("(load-file \"{}\")", file), repl_env) {
            Ok(val) => {
                println!("{:?}", val);
            }
            Err(e) => {
                eprintln!("Exception: {}", e);
            }
        }
        Ok(())
    } else {
        rep(
            "(println (str \"Mal [\" *host-language* \"]\"))",
            repl_env.clone(),
        )
        .unwrap();

        let mut line = String::new();
        loop {
            write!(stdout().lock(), "user> ").and_then(|_| stdout().lock().flush())?;

            line.clear();
            match stdin().lock().read_line(&mut line) {
                res @ Ok(0) | res @ Err(_) => return res.map(|_| ()),
                Ok(1) => continue,
                _ => (),
            }

            match rep(&line, repl_env.clone()) {
                Ok(val) => {
                    writeln!(stdout().lock(), "{:?}", val).and_then(|_| stdout().lock().flush())?
                }
                Err(e) => writeln!(stderr().lock(), "Exception: {}", e)
                    .and_then(|_| stderr().lock().flush())?,
            }
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
            Value::List(mut form, _) => match form.pop_front() {
                None => return Ok(Value::make_list(Vector::new())),

                Some(Value::Symbol(ref s)) if s.as_str() == "defmacro!" => {
                    ensure_len(form.len(), |n| n == 2, 2, "defmacro!")?;
                    let sym = form
                        .pop_front()
                        .unwrap()
                        .owned_symbol()
                        .ok_or_else(|| e::arg_type("defmacro!", "symbol", 0))?;

                    let value = eval(form.pop_front().unwrap(), env.clone())?
                        .closure_to_macro()
                        .ok_or_else(|| e::arg_type("defmacro!", "function", 1))?;

                    return Ok(env.set(sym, value));
                }

                Some(Value::Symbol(ref s)) if s.as_str() == "def!" => {
                    ensure_len(form.len(), |n| n == 2, 2, "def!")?;
                    return form
                        .pop_front()
                        .unwrap()
                        .owned_symbol()
                        .ok_or_else(|| e::arg_type("def!", "symbol", 0))
                        .and_then(|sym| {
                            Ok(env.set(sym, eval(form.pop_front().unwrap(), env.clone())?))
                        });
                }

                Some(Value::Symbol(ref s)) if s.as_str() == "let*" => {
                    ensure_len(form.len(), |n| n == 2, 2, "let*")?;

                    env = form
                        .pop_front()
                        .unwrap()
                        .owned_sequence()
                        .ok_or_else(|| e::arg_type("let*", "list or vector", 0))
                        .and_then(|seq| Env::with_env(env.clone()).let_binds(seq))?;

                    ast = form.pop_front().unwrap();
                }

                Some(Value::Symbol(ref s)) if s.as_str() == "do" => {
                    ensure_len(form.len(), |n| n >= 1, "1 or more", "do")?;
                    ast = form.pop_back().unwrap();
                    for value in form {
                        eval(value, env.clone())?;
                    }
                }

                Some(Value::Symbol(ref s)) if s.as_str() == "if" => {
                    ensure_len(form.len(), |n| n == 2 || n == 3, "2 or 3", "if")?;

                    let cond = form.pop_front().unwrap();
                    let then = form.pop_front().unwrap();
                    let otherwise = form.pop_front().unwrap_or_else(|| Value::Nil);

                    match eval(cond, env.clone())? {
                        Value::Nil | Value::Bool(false) => ast = otherwise,
                        _ => ast = then,
                    }
                }

                Some(Value::Symbol(ref s)) if s.as_str() == "fn*" => {
                    ensure_len(form.len(), |n| n == 2, 2, "fn*")?;

                    let (len, binds) = form
                        .pop_front()
                        .unwrap()
                        .owned_sequence()
                        .map(|v| (v.len(), v))
                        .ok_or_else(|| e::arg_type("fn*", "list or vector", 0))?;

                    let mut syms = Vector::new();
                    let mut is_rest = false;

                    for (i, bind) in binds.into_iter().enumerate() {
                        let sym = bind
                            .owned_symbol()
                            .ok_or_else(|| e::arg_type("fn* args", "symbol", i))?;

                        if sym.as_str() == "&" {
                            if len - i != 2 {
                                return Err(e::rest_parameter());
                            }
                            is_rest = true;
                        } else {
                            syms.push_back(sym);
                        }
                    }

                    return Ok(Value::make_closure(
                        env,
                        syms,
                        Rc::new(form.pop_front().unwrap()),
                        is_rest,
                    ));
                }

                Some(Value::Symbol(ref s)) if s.as_str() == "eval" => {
                    ensure_len(form.len(), |n| n == 1, 1, "eval")?;
                    ast = eval(form.pop_front().unwrap(), env.clone())?;
                    env = env.most_outer();
                }

                Some(Value::Symbol(ref s)) if s.as_str() == "quote" => {
                    ensure_len(form.len(), |n| n == 1, 1, "quote")?;
                    return Ok(form.pop_front().unwrap());
                }

                Some(Value::Symbol(ref s)) if s.as_str() == "quasiquote" => {
                    ensure_len(form.len(), |n| n == 1, 1, "quasiquote")?;
                    ast = quasiquote(form.pop_front().unwrap());
                }

                Some(Value::Symbol(ref s)) if s.as_str() == "macroexpand" => {
                    ensure_len(form.len(), |n| n == 1, 1, "macroexpand")?;
                    return macroexpand(form.pop_front().unwrap(), env);
                }

                Some(Value::Symbol(ref s)) if s.as_str() == "try*" => {
                    ensure_len(form.len(), |n| n == 2, 2, "try*")?;

                    match eval(form.pop_front().unwrap(), env.clone()) {
                        Err(val) => {
                            let mut catch = form
                                .pop_front()
                                .unwrap()
                                .owned_list()
                                .ok_or_else(|| e::arg_type("try*", "list", 1))?;

                            ensure_len(catch.len(), |n| n == 3, 2, "catch*")?;

                            catch
                                .pop_front()
                                .unwrap()
                                .owned_symbol()
                                .filter(|s| s.as_str() == "catch*")
                                .ok_or_else(e::catch_block)?;

                            let bind = catch
                                .pop_front()
                                .unwrap()
                                .owned_symbol()
                                .ok_or_else(|| e::arg_type("catch*", "symbol", 0))?;

                            env = Env::with_env(env);
                            env.set(bind, val);
                            ast = catch.pop_front().unwrap();
                        }
                        other => return other,
                    }
                }

                Some(mut func @ Value::List(_, _)) | Some(mut func @ Value::Symbol(_)) => {
                    let is_apply = func.symbol().unwrap().as_str() == "apply";

                    if is_apply {
                        ensure_len(form.len(), |n| n >= 1, "1 or more", "apply")?;
                        func = form.pop_front().unwrap();
                    }

                    let func = eval(func, env.clone())?;

                    let mut list = eval_ast(Value::make_list(form), env.clone())?
                        .owned_list()
                        .unwrap();

                    if is_apply && !list.is_empty() {
                        let seq = list.pop_back().unwrap().owned_sequence().ok_or_else(|| {
                            e::arg_type("apply", "list or vector", list.len() + 1)
                        })?;
                        list.append(seq);
                    }

                    match func {
                        Value::Function(func, _) => return func(list),
                        Value::Closure {
                            env: cenv,
                            binds,
                            body,
                            is_rest,
                            ..
                        } => {
                            env = Env::with_env(cenv).fn_binds(binds, list, is_rest)?;
                            ast = body.as_ref().clone();
                        }
                        s => return Err(e::not_function(s)),
                    }
                }

                Some(_) => return Err(e::not_function(&form[0])),
            },
            other => return eval_ast(other, env),
        }
    }
}

fn eval_ast(ast: Value, env: Env) -> EvalResult {
    match ast {
        Value::Symbol(s) => env.get(&s).ok_or_else(|| e::symbol_not_found(s)),
        Value::List(l, _) => l
            .into_iter()
            .map(|elm| eval(elm, env.clone()))
            .collect::<Result<_, _>>()
            .map(Value::make_list),
        Value::Vector(v, _) => v
            .into_iter()
            .map(|elm| eval(elm, env.clone()))
            .collect::<Result<_, _>>()
            .map(Value::make_vector),
        Value::HashMap(h, _) => h
            .into_iter()
            .map(|(k, v)| Ok((k, eval(v, env.clone())?)))
            .collect::<Result<_, _>>()
            .map(Value::make_hashmap),
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
                            return Value::make_list(vector![
                                Value::make_symbol("concat"),
                                seq_first.next().unwrap().clone(),
                                quasiquote(Value::make_list(seq))
                            ]);
                        }
                    }
                }
                Value::make_list(vector![
                    Value::make_symbol("cons"),
                    quasiquote(ast_first),
                    quasiquote(Value::make_list(seq))
                ])
            }
        }
    } else {
        Value::make_list(vector![Value::make_symbol("quote"), ast])
    }
}

fn is_pair(ast: &Value) -> Option<&Vector<Value>> {
    ast.sequence().filter(|v| !v.is_empty())
}

fn macroexpand(mut ast: Value, env: Env) -> EvalResult {
    loop {
        if let Value::List(mut l, _) = ast.clone() {
            if let Some(Value::Macro {
                env: cenv,
                binds,
                body,
                is_rest,
                ..
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
