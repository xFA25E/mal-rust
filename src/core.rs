use im_rc::{HashMap, Vector};

use std::{
    cell::RefCell,
    convert::TryInto,
    fmt::{Display, Write},
    fs::read_to_string,
    rc::Rc,
};

use crate::{
    env::Env,
    error as e, eval,
    reader::read,
    value::{Args, EvalResult, Value},
};

pub fn keys(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "keys")?;

    match &args[0] {
        Value::HashMap(h) => Ok(Value::List(h.keys().map(|c| c.clone().into()).collect())),
        _ => e::arg_type("assoc", "hash-map", 0),
    }
}

pub fn vals(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "vals")?;

    match &args[0] {
        Value::HashMap(h) => Ok(Value::List(h.values().map(|c| c.clone()).collect())),
        _ => e::arg_type("assoc", "hash-map", 0),
    }
}

pub fn containsp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "contains?")?;

    match &args[0] {
        Value::HashMap(h) => Ok(Value::Bool(h.contains_key(&args[1].clone().try_into()?))),
        _ => e::arg_type("assoc", "hash-map", 0),
    }
}

pub fn get(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "get")?;

    match &args[0] {
        Value::HashMap(h) => Ok(h
            .get(&args[1].clone().try_into()?)
            .map(|c| c.clone())
            .unwrap_or_else(|| Value::Nil)),
        Value::Nil => Ok(Value::Nil),
        _ => e::arg_type("assoc", "hash-map or nil", 0),
    }
}

pub fn dissoc(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n >= 1, "1 or more", "dissoc")?;

    match &args[0] {
        Value::HashMap(map) => {
            let mut map = map.clone();
            for key in args.iter().skip(1) {
                map.remove(&key.clone().try_into()?);
            }
            Ok(Value::HashMap(map))
        }
        _ => e::arg_type("assoc", "hash-map", 0),
    }
}

pub fn assoc(args: Args) -> EvalResult {
    ensure_len(
        args.len(),
        |n| n >= 1 && n % 2 != 0,
        "1 or more odd number of arguments",
        "assoc",
    )?;

    match &args[0] {
        Value::HashMap(map) => {
            let mut map = map.clone();

            for (k, v) in evens(args.iter().skip(1)).zip(odds(args.iter().skip(1))) {
                map.insert(k.clone().try_into()?, v.clone());
            }

            Ok(Value::HashMap(map))
        }
        _ => e::arg_type("assoc", "hash-map", 0),
    }
}

pub fn mapp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "map?")?;

    match args[0] {
        Value::HashMap(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn hash_map(args: Args) -> EvalResult {
    ensure_len(
        args.len(),
        |n| n % 2 == 0,
        "even number of arguments",
        "hash-map",
    )?;

    evens(args.iter())
        .zip(odds(args.iter()))
        .map(|(k, v)| Ok((k.clone().try_into()?, v.clone())))
        .collect::<Result<HashMap<_, _>, _>>()
        .map(Value::HashMap)
}

pub fn sequentialp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "sequential?")?;

    match args[0] {
        Value::Vector(_) | Value::List(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn vectorp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "vector?")?;

    match args[0] {
        Value::Vector(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn vector(args: Args) -> EvalResult {
    Ok(Value::Vector(args))
}

pub fn keyword(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "keyword")?;

    match &args[0] {
        keyword @ Value::Keyword(_) => Ok(keyword.clone()),
        Value::String(s) => Ok(Value::Keyword(Rc::clone(s))),
        _ => e::arg_type("keyword", "string or keyword", 0),
    }
}

pub fn keywordp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "keyword?")?;

    match args[0] {
        Value::Keyword(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn symbol(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "symbol")?;

    match &args[0] {
        Value::String(s) => Ok(Value::Symbol(Rc::clone(s))),
        _ => e::arg_type("symbol", "string", 0),
    }
}

pub fn truep(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "true?")?;

    match args[0] {
        Value::Bool(b) => Ok(Value::Bool(b)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn falsep(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "false?")?;

    match args[0] {
        Value::Bool(b) => Ok(Value::Bool(!b)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn symbolp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "symbol?")?;

    match args[0] {
        Value::Symbol(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn nilp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "nil?")?;

    match args[0] {
        Value::Nil => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn apply(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n >= 1, "1 or more", "apply")?;

    let mut f_args: Vector<_> = args
        .iter()
        .take(args.len().saturating_sub(1))
        .skip(1)
        .map(|c| c.clone())
        .collect();

    if args.len() > 1 {
        match args.back().unwrap() {
            Value::List(v) | Value::Vector(v) => f_args.append(v.clone()),
            _ => return e::arg_type("apply", "list or vector", args.len() - 1),
        }
    }
    match args.get(0).unwrap() {
        Value::Closure {
            binds, body, env, ..
        } => {
            if binds.len() != f_args.len() {
                match binds.get(binds.len().saturating_sub(2)).map(|s| s.as_str()) {
                    Some("&") => (),
                    _ => return e::rest_parameter(),
                }
            }

            let cenv = Env::new().env(env.clone()).binds(&binds, f_args).make();
            eval(body.as_ref().clone(), cenv)
        }
        Value::Function(func) => func(f_args),

        _ => e::arg_type("apply", "function", 0),
    }
}

pub fn map(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "map")?;

    let iter: Box<dyn Iterator<Item = &Value>> = match args.get(1).unwrap() {
        Value::List(v) | Value::Vector(v) => Box::new(v.iter()),
        _ => return e::arg_type("map", "list or vector", 1),
    };

    match args.get(0).unwrap() {
        Value::Closure {
            binds, body, env, ..
        } => {
            if binds.len() != 1 && (binds.len() != 2 || binds.get(0).unwrap().as_ref() != "&") {
                return e::rest_parameter();
            }

            Ok(Value::List(
                iter.map(|c| {
                    let cenv = Env::new()
                        .env(env.clone())
                        .binds(&binds, {
                            let mut list = Vector::new();
                            list.push_back(c.clone());
                            list
                        })
                        .make();
                    eval(body.as_ref().clone(), cenv)
                })
                .collect::<Result<_, _>>()?,
            ))
        }
        Value::Function(func) => Ok(Value::List(
            iter.map(|c| {
                let mut list = Vector::new();
                list.push_back(c.clone());
                func(list)
            })
            .collect::<Result<_, _>>()?,
        )),
        _ => e::arg_type("map", "function", 0),
    }
}

pub fn throw(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "throw")?;

    Err(args[0].clone())
}

pub fn rest(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "rest")?;

    match args[0].clone() {
        Value::List(v) | Value::Vector(v) => {
            let mut v = v.clone();
            v.pop_front();
            Ok(Value::List(v))
        }
        Value::Nil => Ok(Value::List(Vector::new())),
        _ => e::arg_type("rest", "list, vector or nil", 0),
    }
}

pub fn first(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "first")?;

    match &args[0] {
        Value::List(v) | Value::Vector(v) => {
            Ok(v.get(0).map(|c| c.clone()).unwrap_or_else(|| Value::Nil))
        }
        Value::Nil => Ok(Value::Nil),
        _ => e::arg_type("first", "list, vector or nil", 0),
    }
}

pub fn nth(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "nth")?;

    let mut iter = args.iter();
    let seq = iter.next().unwrap();
    match iter.next().unwrap() {
        Value::Number(n) => match seq {
            Value::List(v) | Value::Vector(v) => v
                .get(*n as usize)
                .map(|e| e.clone())
                .ok_or_else(|| Value::String(Rc::new("Index out of range".into()))),
            _ => e::arg_type("nth", "list or vector", 1),
        },
        _ => e::arg_type("nth", "number", 0),
    }
}

pub fn concat(args: Args) -> EvalResult {
    let mut result = Vector::new();
    for (i, elm) in args.iter().enumerate() {
        match elm {
            Value::List(vector) | Value::Vector(vector) => {
                for item in vector.iter() {
                    result.push_back(item.clone());
                }
            }
            _ => return e::arg_type("cons", "list or vector", i),
        }
    }
    Ok(Value::List(result))
}

pub fn cons(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "cons")?;

    let mut iter = args.iter();
    let elm = iter.next().unwrap();
    match iter.next().unwrap() {
        Value::List(vector) | Value::Vector(vector) => {
            let mut list = vector.clone();
            list.push_front(elm.clone());
            Ok(Value::List(list))
        }
        _ => e::arg_type("cons", "list or vector", 1),
    }
}

pub fn swap(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n >= 2, "2 or more", "swap!")?;

    let mut args = args.clone();
    match args.pop_front().unwrap() {
        Value::Atom(atom) => match args.pop_front().unwrap() {
            Value::Function(func) => {
                args.push_front(atom.borrow().clone());
                let new_val = func(args)?;
                atom.replace(new_val.clone());
                Ok(new_val)
            }

            Value::Closure {
                env, binds, body, ..
            } => {
                args.push_front(atom.borrow().clone());

                if binds.len() != args.len() {
                    match binds.get(binds.len() - 2).map(|s| s.as_str()) {
                        Some("&") => (),
                        _ => return e::arg_count("function in swap!", binds.len(), args.len()),
                    }
                }

                let closure_env = Env::new().env(env).binds(&binds, args).make();
                let new_val = eval(body.as_ref().clone(), closure_env)?;
                atom.replace(new_val.clone());
                Ok(new_val)
            }

            _ => e::arg_type("swap!", "function", 1),
        },
        _ => e::arg_type("swap!", "atom", 0),
    }
}

pub fn reset(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "reset!")?;

    let mut iter = args.iter();
    match iter.next().unwrap() {
        Value::Atom(atom) => {
            let value = iter.next().unwrap();
            atom.replace(value.clone());
            Ok(value.clone())
        }
        _ => e::arg_type("reset!", "atom", 0),
    }
}

pub fn deref(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "deref")?;

    match &args[0] {
        Value::Atom(a) => Ok(a.borrow().clone()),
        _ => e::arg_type("deref", "atom", 0),
    }
}

pub fn atomp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "atom?")?;

    match args[0] {
        Value::Atom(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn atom(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "atom")?;

    Ok(Value::Atom(Rc::new(RefCell::new(args[0].clone()))))
}

pub fn read_string(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "read-string")?;

    match &args[0] {
        Value::String(s) => Ok(read(s)?),
        _ => e::arg_type("read-string", "string", 0),
    }
}

pub fn slurp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "slurp")?;

    match &args[0] {
        Value::String(s) => Ok(Value::String(Rc::new(
            read_to_string(s.as_ref()).or_else(|err| e::io(err))?,
        ))),
        _ => e::arg_type("slurp", "string", 0),
    }
}

pub fn println(args: Args) -> EvalResult {
    let mut iter = args.iter();
    if let Some(v) = iter.next() {
        print!("{}", v);
    }

    for v in iter {
        print!(" {}", v);
    }
    println!();
    Ok(Value::Nil)
}

pub fn prn(args: Args) -> EvalResult {
    let mut iter = args.iter();
    if let Some(v) = iter.next() {
        print!("{:?}", v);
    }

    for v in iter {
        print!(" {:?}", v);
    }
    println!();
    Ok(Value::Nil)
}

pub fn str(args: Args) -> EvalResult {
    let mut result = String::new();
    for s in args.iter() {
        write!(result, "{}", s).unwrap();
    }
    Ok(Value::String(Rc::new(result)))
}

pub fn pr_str(args: Args) -> EvalResult {
    let mut result = String::new();
    for s in args.iter() {
        write!(result, "{:?} ", s).unwrap();
    }
    result.pop();
    Ok(Value::String(Rc::new(result)))
}

pub fn list(args: Args) -> EvalResult {
    Ok(Value::List(args))
}

pub fn listp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "list?")?;

    match args[0] {
        Value::List(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn emptyp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "empty?")?;

    match &args[0] {
        Value::List(v) | Value::Vector(v) => Ok(Value::Bool(v.is_empty())),
        _ => e::arg_type("empty?", "list or vector", 0),
    }
}

pub fn count(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "count")?;

    match &args[0] {
        Value::List(v) | Value::Vector(v) => Ok(Value::Number(v.len() as i128)),
        Value::Nil => Ok(Value::Number(0)),
        _ => e::arg_type("count", "list, vector or nil", 0),
    }
}

pub fn equal(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, '=')?;

    Ok(Value::Bool(args[0] == args[1]))
}

pub fn less(args: Args) -> EvalResult {
    arithmetic_compare(args, i128::lt, '<')
}

pub fn less_equal(args: Args) -> EvalResult {
    arithmetic_compare(args, i128::le, "<=")
}

pub fn greater(args: Args) -> EvalResult {
    arithmetic_compare(args, i128::gt, '>')
}

pub fn greater_equal(args: Args) -> EvalResult {
    arithmetic_compare(args, i128::ge, ">=")
}

pub fn add(args: Args) -> EvalResult {
    arithmetic_operation(args, i128::checked_add, '+')
}

pub fn subtract(args: Args) -> EvalResult {
    arithmetic_operation(args, i128::checked_sub, '-')
}

pub fn multiply(args: Args) -> EvalResult {
    arithmetic_operation(args, i128::checked_mul, '*')
}

pub fn divide(args: Args) -> EvalResult {
    arithmetic_operation(args, i128::checked_div, '/')
}

#[inline]
fn arithmetic_compare<D>(args: Args, operation: fn(&i128, &i128) -> bool, name: D) -> EvalResult
where
    D: Display,
{
    ensure_len(args.len(), |n| n == 2, 2, &name)?;

    Ok(Value::Bool(operation(
        &args[0].number(&name, 0)?,
        &args[1].number(name, 1)?,
    )))
}

#[inline]
fn arithmetic_operation<D: Display>(
    args: Args,
    operation: fn(i128, i128) -> Option<i128>,
    name: D,
) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, &name)?;

    let first = args[0].number(&name, 0)?;
    let second = args[1].number(&name, 1)?;

    operation(first, second)
        .map(Value::Number)
        .ok_or_else(|| e::numeric_overflow(name, first, second))
}

#[inline]
pub fn ensure_len<F, D, S>(provided: usize, p: F, required: S, name: D) -> Result<(), Value>
where
    F: Fn(usize) -> bool,
    D: Display,
    S: Display,
{
    if p(provided) {
        Ok(())
    } else {
        e::arg_count(name, required, provided)
    }
}

#[inline]
fn odds<'a, I: Iterator<Item = &'a Value>>(i: I) -> impl Iterator<Item = &'a Value> {
    i.enumerate().filter(|(i, _)| i % 2 != 0).map(|(_, v)| v)
}

#[inline]
fn evens<'a, I: Iterator<Item = &'a Value>>(i: I) -> impl Iterator<Item = &'a Value> {
    i.enumerate().filter(|(i, _)| i % 2 == 0).map(|(_, k)| k)
}
