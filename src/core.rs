use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    convert::TryInto,
    fmt::Write,
    fs::read_to_string,
    rc::Rc,
};

use crate::{env::Env, error::EvalError, eval, reader::read, value::Value};

pub type Args = Rc<VecDeque<Value>>;
pub type EvalResult = Result<Value, Value>;

pub fn keys(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match &args[0] {
        Value::HashMap(h) => Ok(Value::List(Rc::new(
            h.keys().map(|c| c.clone().into()).collect(),
        ))),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn vals(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match &args[0] {
        Value::HashMap(h) => Ok(Value::List(Rc::new(
            h.values().map(|c| c.clone()).collect(),
        ))),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn containsp(args: Args) -> EvalResult {
    ensure_len(args.len() == 2)?;
    match &args[0] {
        Value::HashMap(h) => Ok(Value::Bool(h.contains_key(&args[1].clone().try_into()?))),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn get(args: Args) -> EvalResult {
    ensure_len(args.len() == 2)?;
    match &args[0] {
        Value::HashMap(h) => Ok(h
            .get(&args[1].clone().try_into()?)
            .map(|c| c.clone())
            .unwrap_or_else(|| Value::Nil)),
        Value::Nil => Ok(Value::Nil),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn dissoc(args: Args) -> EvalResult {
    ensure_len(args.len() >= 1)?;
    let mut map = match &args[0] {
        Value::HashMap(h) => h.clone(),
        _ => return Err(EvalError::InvalidArgumentType.into()),
    };
    let map_ref = Rc::make_mut(&mut map);

    for key in args.iter().skip(1) {
        map_ref.remove(&key.clone().try_into()?);
    }
    Ok(Value::HashMap(map))
}

pub fn assoc(args: Args) -> EvalResult {
    ensure_len(args.len() >= 1 && args.len() % 2 != 0)?;
    let mut map = match &args[0] {
        Value::HashMap(h) => h.clone(),
        _ => return Err(EvalError::InvalidArgumentType.into()),
    };
    let map_ref = Rc::make_mut(&mut map);

    let mut iter = args.iter().skip(1);
    while let Some(key) = iter.next() {
        let key = key.clone().try_into()?;
        let value = iter.next().unwrap().clone();
        map_ref.insert(key, value);
    }
    Ok(Value::HashMap(map))
}

pub fn mapp(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match args[0] {
        Value::HashMap(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn hash_map(args: Args) -> EvalResult {
    if args.len() % 2 != 0 {
        return Err(EvalError::InvalidNumberOfArguments.into());
    }

    let mut iter = args.iter();
    let mut map = HashMap::new();
    while let Some(key) = iter.next() {
        let key = key.clone().try_into()?;
        let value = iter.next().unwrap().clone();
        map.insert(key, value);
    }
    Ok(Value::HashMap(Rc::new(map)))
}

pub fn sequentialp(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match args[0] {
        Value::Vector(_) | Value::List(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn vectorp(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match args[0] {
        Value::Vector(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn vector(args: Args) -> EvalResult {
    Ok(Value::Vector(Rc::new(
        args.iter().map(|c| c.clone()).collect(),
    )))
}

pub fn keyword(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match &args[0] {
        keyword @ Value::Keyword(_) => Ok(keyword.clone()),
        Value::String(s) => Ok(Value::Keyword(Rc::clone(s))),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn keywordp(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match args[0] {
        Value::Keyword(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn symbol(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match &args[0] {
        Value::String(s) => Ok(Value::Symbol(Rc::clone(s))),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn truep(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match args[0] {
        Value::Bool(b) => Ok(Value::Bool(b)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn falsep(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match args[0] {
        Value::Bool(b) => Ok(Value::Bool(!b)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn symbolp(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match args[0] {
        Value::Symbol(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn nilp(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match args[0] {
        Value::Nil => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn apply(args: Args) -> EvalResult {
    ensure_len(args.len() >= 1)?;

    let mut f_args: VecDeque<_> = args
        .iter()
        .take(args.len().saturating_sub(1))
        .skip(1)
        .map(|c| c.clone())
        .collect();
    if args.len() > 1 {
        match args.back().unwrap() {
            Value::List(l) => {
                for elm in l.iter() {
                    f_args.push_back(elm.clone());
                }
            }
            Value::Vector(v) => {
                for elm in v.iter() {
                    f_args.push_back(elm.clone());
                }
            }
            _ => return Err(EvalError::InvalidArgumentType.into()),
        }
    }
    match args.get(0).unwrap() {
        Value::Closure {
            binds, body, env, ..
        } => {
            if binds.len() != f_args.len() {
                match binds.get(binds.len().saturating_sub(2)).map(|s| s.as_str()) {
                    Some("&") => (),
                    _ => return Err(EvalError::InvalidNumberOfArguments.into()),
                }
            }

            let cenv = Env::new()
                .env(env.clone())
                .binds(&binds, Rc::new(f_args))
                .make();
            eval(body.as_ref().clone(), cenv)
        }
        Value::Function(func) => func(Rc::new(f_args)),

        _ => return Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn map(args: Args) -> EvalResult {
    ensure_len(args.len() == 2)?;

    let iter: Box<dyn Iterator<Item = &Value>> = match args.get(1).unwrap() {
        Value::List(l) => Box::new(l.iter()),
        Value::Vector(v) => Box::new(v.iter()),
        _ => return Err(EvalError::InvalidArgumentType.into()),
    };

    match args.get(0).unwrap() {
        Value::Closure {
            binds, body, env, ..
        } => {
            if binds.len() != 1 && (binds.len() != 2 || binds.get(0).unwrap().as_ref() != "&") {
                return Err(EvalError::InvalidNumberOfArguments.into());
            }

            Ok(Value::List(Rc::new(
                iter.map(|c| {
                    let cenv = Env::new()
                        .env(env.clone())
                        .binds(
                            &binds,
                            Rc::new({
                                let mut list = VecDeque::with_capacity(1);
                                list.push_back(c.clone());
                                list
                            }),
                        )
                        .make();
                    eval(body.as_ref().clone(), cenv)
                })
                .collect::<Result<_, _>>()?,
            )))
        }
        Value::Function(func) => Ok(Value::List(Rc::new(
            iter.map(|c| {
                let mut list = VecDeque::with_capacity(1);
                list.push_back(c.clone());
                func(Rc::new(list))
            })
            .collect::<Result<_, _>>()?,
        ))),
        _ => return Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn throw(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    Err(EvalError::Exception(args[0].clone()).into())
}

pub fn rest(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match args[0].clone() {
        Value::List(mut l) => {
            let list_ref = Rc::make_mut(&mut l);
            list_ref.pop_front();
            Ok(Value::List(l))
        }
        Value::Vector(v) => {
            if v.len() > 1 {
                Ok(Value::List(Rc::new(
                    v[1..].iter().map(|c| c.clone()).collect(),
                )))
            } else {
                Ok(Value::List(Rc::new(VecDeque::new())))
            }
        }
        Value::Nil => Ok(Value::List(Rc::new(VecDeque::new()))),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn first(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match &args[0] {
        Value::List(l) => Ok(l.get(0).map(|c| c.clone()).unwrap_or_else(|| Value::Nil)),
        Value::Vector(v) => Ok(v.get(0).map(|c| c.clone()).unwrap_or_else(|| Value::Nil)),
        Value::Nil => Ok(Value::Nil),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn nth(args: Args) -> EvalResult {
    ensure_len(args.len() == 2)?;

    let mut iter = args.iter();
    let seq = iter.next().unwrap();
    match iter.next().unwrap() {
        Value::Number(n) => match seq {
            Value::List(l) => l.get(*n as usize).map(|e| e.clone()).ok_or_else(|| {
                EvalError::Exception(Value::Symbol(Rc::new("out-of-range".into()))).into()
            }),
            Value::Vector(v) => v.get(*n as usize).map(|e| e.clone()).ok_or_else(|| {
                EvalError::Exception(Value::Symbol(Rc::new("out-of-range".into()))).into()
            }),
            _ => Err(EvalError::InvalidArgumentType.into()),
        },
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn concat(args: Args) -> EvalResult {
    let mut result = VecDeque::new();
    for elm in args.iter() {
        match elm {
            Value::List(list) => {
                for item in list.iter() {
                    result.push_back(item.clone());
                }
            }
            Value::Vector(vector) => {
                for item in vector.iter() {
                    result.push_back(item.clone());
                }
            }
            _ => return Err(EvalError::InvalidArgumentType.into()),
        }
    }
    Ok(Value::List(Rc::new(result)))
}

pub fn cons(args: Args) -> EvalResult {
    ensure_len(args.len() == 2)?;

    let mut iter = args.iter();
    let elm = iter.next().unwrap();
    match iter.next().unwrap() {
        Value::List(list) => {
            let mut list = list.clone();
            let args_ref = Rc::make_mut(&mut list);
            args_ref.push_front(elm.clone());
            Ok(Value::List(list))
        }
        Value::Vector(vector) => {
            let mut list: VecDeque<_> = vector.iter().map(|c| c.clone()).collect();
            list.push_front(elm.clone());
            Ok(Value::List(Rc::new(list)))
        }
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn swap(mut args: Args) -> EvalResult {
    ensure_len(args.len() >= 2)?;

    let args_ref = Rc::make_mut(&mut args);
    match args_ref.pop_front().unwrap() {
        Value::Atom(atom) => match args_ref.pop_front().unwrap() {
            Value::Function(func) => {
                args_ref.push_front(atom.borrow().clone());
                let new_val = func(args)?;
                atom.replace(new_val.clone());
                Ok(new_val)
            }

            Value::Closure {
                env, binds, body, ..
            } => {
                args_ref.push_front(atom.borrow().clone());

                if binds.len() != args.len() {
                    match binds.get(binds.len() - 2).map(|s| s.as_str()) {
                        Some("&") => (),
                        _ => return Err(EvalError::InvalidNumberOfArguments.into()),
                    }
                }

                let closure_env = Env::new().env(env).binds(&binds, args).make();
                let new_val = eval(body.as_ref().clone(), closure_env)?;
                atom.replace(new_val.clone());
                Ok(new_val)
            }

            _ => Err(EvalError::InvalidArgumentType.into()),
        },
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn reset(args: Args) -> EvalResult {
    ensure_len(args.len() == 2)?;

    let mut iter = args.iter();
    match iter.next().unwrap() {
        Value::Atom(atom) => {
            let value = iter.next().unwrap();
            atom.replace(value.clone());
            Ok(value.clone())
        }
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn deref(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match &args[0] {
        Value::Atom(a) => Ok(a.borrow().clone()),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn atomp(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match args[0] {
        Value::Atom(_) => Ok(Value::Bool(true)),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn atom(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    Ok(Value::Atom(Rc::new(RefCell::new(args[0].clone()))))
}

pub fn read_string(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match &args[0] {
        Value::String(s) => Ok(read(s)?),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn slurp(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match &args[0] {
        Value::String(s) => Ok(Value::String(Rc::new(
            read_to_string(s.as_ref()).map_err(EvalError::from)?,
        ))),
        _ => Err(EvalError::InvalidArgumentType.into()),
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
    ensure_len(args.len() == 1)?;
    match args[0] {
        Value::List(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn emptyp(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match &args[0] {
        Value::List(l) => Ok(Value::Bool(l.is_empty())),
        Value::Vector(v) => Ok(Value::Bool(v.is_empty())),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn count(args: Args) -> EvalResult {
    ensure_len(args.len() == 1)?;
    match &args[0] {
        Value::List(l) => Ok(Value::Number(l.len() as i128)),
        Value::Vector(v) => Ok(Value::Number(v.len() as i128)),
        Value::Nil => Ok(Value::Number(0)),
        _ => Err(EvalError::InvalidArgumentType.into()),
    }
}

pub fn equal(args: Args) -> EvalResult {
    ensure_len(args.len() == 2)?;
    Ok(Value::Bool(args[0] == args[1]))
}

pub fn less(args: Args) -> EvalResult {
    arithmetic_compare(args, i128::lt)
}

pub fn less_equal(args: Args) -> EvalResult {
    arithmetic_compare(args, i128::le)
}

pub fn greater(args: Args) -> EvalResult {
    arithmetic_compare(args, i128::gt)
}

pub fn greater_equal(args: Args) -> EvalResult {
    arithmetic_compare(args, i128::ge)
}

pub fn add(args: Args) -> EvalResult {
    arithmetic_operation(args, i128::checked_add)
}

pub fn subtract(args: Args) -> EvalResult {
    arithmetic_operation(args, i128::checked_sub)
}

pub fn multiply(args: Args) -> EvalResult {
    arithmetic_operation(args, i128::checked_mul)
}

pub fn divide(args: Args) -> EvalResult {
    arithmetic_operation(args, i128::checked_div)
}

#[inline]
fn arithmetic_compare(args: Args, operation: fn(&i128, &i128) -> bool) -> EvalResult {
    ensure_len(args.len() == 2)?;
    Ok(Value::Bool(operation(
        &args[0].number()?,
        &args[1].number()?,
    )))
}

#[inline]
fn arithmetic_operation(args: Args, operation: fn(i128, i128) -> Option<i128>) -> EvalResult {
    ensure_len(args.len() == 2)?;
    operation(args[0].number()?, args[1].number()?)
        .map(Value::Number)
        .ok_or_else(|| EvalError::NumericOverflow.into())
}

#[inline]
pub fn ensure_len(b: bool) -> Result<(), EvalError> {
    if b {
        Ok(())
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}
