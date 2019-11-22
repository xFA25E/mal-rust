use im_rc::{vector, Vector};

use std::{
    convert::TryInto,
    fmt::{Display, Write},
    fs::read_to_string,
};

use crate::{
    env::Env,
    error as e, eval,
    reader::read,
    value::{Args, EvalResult, Value},
};

pub fn keys(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "keys")?;

    args[0]
        .hashmap()
        .map(|h| Value::List(h.keys().map(Clone::clone).map(Value::from).collect()))
        .ok_or_else(|| e::arg_type("assoc", "hash-map", 0))
}

pub fn vals(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "vals")?;

    args[0]
        .hashmap()
        .map(|h| Value::List(h.values().map(Clone::clone).collect()))
        .ok_or_else(|| e::arg_type("assoc", "hash-map", 0))
}

pub fn containsp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "contains?")?;

    let mut args = args.iter();

    Ok(Value::Bool(
        args.next()
            .and_then(Value::hashmap)
            .ok_or_else(|| e::arg_type("assoc", "hash-map", 0))?
            .contains_key(&args.next().unwrap().clone().try_into()?),
    ))
}

pub fn get(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "get")?;

    let mut args = args.iter();

    match args.next().unwrap() {
        Value::Nil => Ok(Value::Nil),
        other => Ok(other
            .hashmap()
            .ok_or_else(|| e::arg_type("assoc", "hash-map or nil", 0))?
            .get(&args.next().unwrap().clone().try_into()?)
            .map_or_else(|| Value::Nil, Clone::clone)),
    }
}

pub fn dissoc(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n >= 1, "1 or more", "dissoc")?;

    let mut map = args[0]
        .hashmap()
        .ok_or_else(|| e::arg_type("assoc", "hash-map", 0))?
        .clone();

    for key in args.iter().skip(1) {
        map.remove(&key.clone().try_into()?);
    }

    Ok(Value::HashMap(map))
}

pub fn assoc(args: Args) -> EvalResult {
    ensure_len(
        args.len(),
        |n| n >= 1 && n % 2 != 0,
        "1 or more odd number of arguments",
        "assoc",
    )?;

    let mut map = args[0]
        .hashmap()
        .ok_or_else(|| e::arg_type("assoc", "hash-map", 0))?
        .clone();

    for (k, v) in Pairs(args.iter().skip(1)) {
        map.insert(k.clone().try_into()?, v.clone());
    }

    Ok(Value::HashMap(map))
}

pub fn mapp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "map?")?;

    Ok(Value::Bool(args[0].hashmap().is_some()))
}

pub fn hash_map(args: Args) -> EvalResult {
    ensure_len(
        args.len(),
        |n| n % 2 == 0,
        "even number of arguments",
        "hash-map",
    )?;

    Pairs(args.iter())
        .map(|(k, v)| Ok((k.clone().try_into()?, v.clone())))
        .collect::<Result<_, _>>()
        .map(Value::HashMap)
}

pub fn sequentialp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "sequential?")?;

    Ok(Value::Bool(args[0].sequence().is_some()))
}

pub fn vectorp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "vector?")?;

    Ok(Value::Bool(args[0].vector().is_some()))
}

pub fn vector(args: Args) -> EvalResult {
    Ok(Value::Vector(args))
}

pub fn keyword(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "keyword")?;

    match &args[0] {
        keyword @ Value::Keyword(_) => Ok(keyword.clone()),
        other => other
            .string()
            .map(|s| Value::make_keyword(&**s))
            .ok_or_else(|| e::arg_type("keyword", "string or keyword", 0)),
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

    args[0]
        .string()
        .map(|s| Value::make_symbol(&**s))
        .ok_or_else(|| e::arg_type("symbol", "string", 0))
}

pub fn truep(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "true?")?;

    Ok(Value::Bool(
        args[0].bool().map_or_else(|| false, |is_true| is_true),
    ))
}

pub fn falsep(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "false?")?;

    Ok(Value::Bool(
        args[0].bool().map_or_else(|| false, |is_true| !is_true),
    ))
}

pub fn symbolp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "symbol?")?;

    Ok(Value::Bool(args[0].symbol().is_some()))
}

pub fn nilp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "nil?")?;

    match args[0] {
        Value::Nil => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn apply(mut args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n >= 1, "1 or more", "apply")?;

    let func = args.pop_front().unwrap();
    match args.pop_back() {
        Some(Value::List(seq)) | Some(Value::Vector(seq)) => args.append(seq),
        None => (),
        Some(_) => return Err(e::arg_type("apply", "list or vector", args.len() + 1)),
    }

    match func {
        Value::Function(func) => func(args),
        Value::Closure {
            env,
            binds,
            body,
            is_rest,
        } => {
            let env = Env::with_env(env).fn_binds(binds, args, is_rest)?;
            eval(body.as_ref().clone(), env)
        }
        _ => Err(e::arg_type("apply", "function", 0)),
    }
}

pub fn map(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "map")?;

    let mut args = args.iter();
    let func = args.next().unwrap();
    let seq = args
        .next()
        .and_then(Value::sequence)
        .ok_or_else(|| e::arg_type("map", "list or vector", 1))?;

    match func {
        Value::Function(func) => seq
            .iter()
            .map(|c| func(vector![c.clone()]))
            .collect::<Result<_, _>>()
            .map(Value::List),
        Value::Closure {
            binds,
            body,
            env,
            is_rest,
        } => seq
            .iter()
            .map(|c| {
                let env = Env::with_env(env.clone()).fn_binds(
                    binds.clone(),
                    vector![c.clone()],
                    *is_rest,
                )?;
                eval(body.as_ref().clone(), env)
            })
            .collect::<Result<_, _>>()
            .map(Value::List),

        _ => Err(e::arg_type("map", "function", 0)),
    }
}

pub fn throw(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "throw")?;

    Err(args[0].clone())
}

pub fn rest(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "rest")?;

    match &args[0] {
        Value::Nil => Ok(Value::List(Vector::new())),
        other => {
            let mut seq = other
                .sequence()
                .ok_or_else(|| e::arg_type("rest", "list, vector or nil", 0))?
                .clone();
            seq.pop_front();
            Ok(Value::List(seq))
        }
    }
}

pub fn first(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "first")?;

    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        other => other
            .sequence()
            .ok_or_else(|| e::arg_type("first", "list, vector or nil", 0))?
            .get(0)
            .map(Clone::clone)
            .ok_or_else(|| Value::Nil),
    }
}

pub fn nth(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "nth")?;

    let mut args = args.iter();

    let seq = args
        .next()
        .and_then(Value::sequence)
        .ok_or_else(|| e::arg_type("nth", "list or vector", 0))?;
    let inx = args
        .next()
        .and_then(Value::number)
        .ok_or_else(|| e::arg_type("nth", "number", 1))?;

    seq.get(*inx as usize)
        .map(Clone::clone)
        .ok_or_else(|| Value::make_string("Index out of range"))
}

pub fn concat(args: Args) -> EvalResult {
    let mut result = Vector::new();

    args.iter().enumerate().try_for_each(|(i, value)| {
        value
            .sequence()
            .map(|vec| result.append(vec.clone()))
            .ok_or_else(|| e::arg_type("cons", "list or vector", i))
    })?;

    Ok(Value::List(result))
}

pub fn cons(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "cons")?;

    let mut args = args.iter();

    let head = args.next().unwrap().clone();
    let mut seq = args
        .next()
        .and_then(Value::sequence)
        .ok_or_else(|| e::arg_type("cons", "list or vector", 1))?
        .clone();

    seq.push_front(head);
    Ok(Value::List(seq))
}

pub fn swap(mut args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n >= 2, "2 or more", "swap!")?;

    match args.pop_front().unwrap() {
        Value::Atom(atom) => {
            let func = args.pop_front().unwrap();
            args.push_front(atom.borrow().clone());

            match func {
                Value::Function(func) => {
                    let new_val = func(args)?;
                    atom.replace(new_val.clone());
                    Ok(new_val)
                }
                Value::Closure {
                    env,
                    binds,
                    body,
                    is_rest,
                } => {
                    let env = Env::with_env(env).fn_binds(binds, args, is_rest)?;
                    let new_val = eval(body.as_ref().clone(), env)?;
                    atom.replace(new_val.clone());
                    Ok(new_val)
                }
                _ => Err(e::arg_type("swap!", "function", 1)),
            }
        }
        _ => Err(e::arg_type("swap!", "atom", 0)),
    }
}

pub fn reset(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "reset!")?;

    let mut args = args.iter();
    let atom = args
        .next()
        .and_then(Value::atom)
        .ok_or_else(|| e::arg_type("reset!", "atom", 0))?;

    let value = args.next().unwrap().clone();

    atom.replace(value.clone());
    Ok(value)
}

pub fn deref(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "deref")?;

    args[0]
        .atom()
        .map(|a| a.borrow().clone())
        .ok_or_else(|| e::arg_type("deref", "atom", 0))
}

pub fn atomp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "atom?")?;

    Ok(Value::Bool(args[0].atom().is_some()))
}

pub fn atom(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "atom")?;

    Ok(Value::make_atom(args[0].clone()))
}

pub fn read_string(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "read-string")?;

    args[0]
        .string()
        .map_or_else(|| Err(e::arg_type("read-string", "string", 0)), |s| read(s))
}

pub fn slurp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "slurp")?;

    args[0].string().map_or_else(
        || Err(e::arg_type("slurp", "string", 0)),
        |s| {
            read_to_string(s.as_ref())
                .map(Value::make_string)
                .map_err(e::io)
        },
    )
}

pub fn println(args: Args) -> EvalResult {
    let mut args = args.iter();

    if let Some(v) = args.next() {
        print!("{}", v);
    }

    for v in args {
        print!(" {}", v);
    }

    println!();
    Ok(Value::Nil)
}

pub fn prn(args: Args) -> EvalResult {
    let mut args = args.iter();

    if let Some(v) = args.next() {
        print!("{:?}", v);
    }

    for v in args {
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

    Ok(Value::make_string(result))
}

pub fn pr_str(args: Args) -> EvalResult {
    let mut args = args.iter();
    let mut result = String::new();

    if let Some(value) = args.next() {
        write!(result, "{:?}", value).unwrap();
    }

    for value in args {
        write!(result, " {:?}", value).unwrap();
    }

    Ok(Value::make_string(result))
}

pub fn list(args: Args) -> EvalResult {
    Ok(Value::List(args))
}

pub fn listp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "list?")?;

    Ok(Value::Bool(args[0].list().is_some()))
}

pub fn emptyp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "empty?")?;

    args[0]
        .sequence()
        .map(|v| Value::Bool(v.is_empty()))
        .ok_or_else(|| e::arg_type("empty?", "list or vector", 0))
}

pub fn count(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "count")?;

    match &args[0] {
        Value::Nil => Ok(Value::Number(0)),
        other => other
            .sequence()
            .map(|v| Value::Number(v.len() as i128))
            .ok_or_else(|| e::arg_type("empty?", "list, vector or nil", 0)),
    }
}

pub fn equal(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, '=')?;

    let mut args = args.iter();

    Ok(Value::Bool(args.next().unwrap() == args.next().unwrap()))
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

    let mut args = args.iter();

    Ok(Value::Bool(operation(
        args.next()
            .and_then(Value::number)
            .ok_or_else(|| e::arg_type(&name, "number", 0))?,
        args.next()
            .and_then(Value::number)
            .ok_or_else(|| e::arg_type(name, "number", 1))?,
    )))
}

#[inline]
fn arithmetic_operation<D: Display>(
    args: Args,
    operation: fn(i128, i128) -> Option<i128>,
    name: D,
) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, &name)?;

    let mut args = args.iter();

    let first = args
        .next()
        .and_then(Value::number)
        .ok_or_else(|| e::arg_type(&name, "number", 0))?;
    let second = args
        .next()
        .and_then(Value::number)
        .ok_or_else(|| e::arg_type(&name, "number", 1))?;

    operation(*first, *second)
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
        Err(e::arg_count(name, required, provided))
    }
}

pub struct Pairs<'a, I: Iterator<Item = &'a Value>>(pub I);

impl<'a, I: Iterator<Item = &'a Value>> Iterator for Pairs<'a, I> {
    type Item = (&'a Value, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .and_then(|key| self.0.next().map(|value| (key, value)))
    }
}
