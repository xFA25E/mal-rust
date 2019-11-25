use im_rc::{vector, Vector};

use std::{
    convert::TryInto,
    fmt::{Display, Write as FmtWrite},
    fs::read_to_string,
    io::{stdin, stdout, BufRead, Write as IoWrite},
    rc::Rc,
};

use crate::{
    env::Env,
    error as e, eval,
    reader::read,
    value::{Args, EvalResult, Value},
};

pub fn seq(mut args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "conj")?;

    match args.pop_front().unwrap() {
        Value::List(s, _) | Value::Vector(s, _) => Ok(if s.is_empty() {
            Value::Nil
        } else {
            Value::make_list(s)
        }),
        Value::String(s) => Ok(if s.is_empty() {
            Value::Nil
        } else {
            Value::make_list(
                s.chars()
                    .map(|c| Value::make_string(c.to_string()))
                    .collect(),
            )
        }),
        Value::Nil => Ok(Value::Nil),
        _ => Err(e::arg_type("seq", "list, vector, string or nil", 0)),
    }
}

pub fn conj(mut args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n >= 2, 2, "conj")?;

    match args.pop_front().unwrap() {
        Value::List(mut l, m) => {
            for value in args {
                l.push_front(value);
            }
            Ok(Value::List(l, m))
        }
        Value::Vector(mut v, m) => {
            for value in args {
                v.push_back(value);
            }
            Ok(Value::Vector(v, m))
        }
        _ => Err(e::arg_type("conj", "list or vector", 0)),
    }
}

pub fn time_ms(_: Args) -> EvalResult {
    Ok(Value::Number(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis()
            .try_into()
            .unwrap(),
    ))
}

pub fn macrop(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "meta")?;

    let bl = match args[0] {
        Value::Macro { .. } => true,
        _ => false,
    };

    Ok(Value::Bool(bl))
}

pub fn fnp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "meta")?;

    let bl = match args[0] {
        Value::Function(_, _) | Value::Closure { .. } => true,
        _ => false,
    };

    Ok(Value::Bool(bl))
}

pub fn numberp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "meta")?;

    Ok(Value::Bool(args[0].number().is_some()))
}

pub fn stringp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "meta")?;

    Ok(Value::Bool(args[0].string().is_some()))
}

pub fn with_meta(mut args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "meta")?;
    let func = args.pop_front().unwrap();
    let meta = Rc::new(args.pop_front().unwrap());

    match func {
        Value::Function(f, _) => Ok(Value::Function(f, meta)),
        Value::Closure {
            binds,
            body,
            env,
            is_rest,
            ..
        } => Ok(Value::Closure {
            binds,
            body,
            env,
            is_rest,
            meta,
        }),
        Value::Macro {
            binds,
            body,
            env,
            is_rest,
            ..
        } => Ok(Value::Macro {
            binds,
            body,
            env,
            is_rest,
            meta,
        }),
        Value::List(l, _) => Ok(Value::List(l, meta)),
        Value::Vector(v, _) => Ok(Value::Vector(v, meta)),
        Value::HashMap(h, _) => Ok(Value::HashMap(h, meta)),
        Value::Atom(a, _) => Ok(Value::Atom(a, meta)),
        _ => Err(e::arg_type(
            "meta",
            0,
            "function, macro, list, vector, hashmap or atom",
        )),
    }
}

pub fn meta(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "meta")?;
    match &args[0] {
        Value::Function(_, meta)
        | Value::Closure { meta, .. }
        | Value::Macro { meta, .. }
        | Value::Vector(_, meta)
        | Value::List(_, meta)
        | Value::Atom(_, meta)
        | Value::HashMap(_, meta) => Ok(meta.as_ref().clone()),
        _ => Err(e::arg_type("meta", 0, "function")),
    }
}

pub fn readline(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "readline")?;

    let string = args[0]
        .string()
        .ok_or_else(|| e::arg_type("readline", "string", 0))?;

    write!(stdout().lock(), "{}", string)
        .and_then(|_| stdout().lock().flush())
        .map_err(e::io)?;

    let mut line = String::new();
    stdin()
        .lock()
        .read_line(&mut line)
        .map_err(e::io)
        .and_then(|n| if n == 0 { Err(e::eof()) } else { Ok(()) })?;
    line.pop();
    Ok(Value::make_string(line))
}

pub fn keys(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "keys")?;

    args[0]
        .hashmap()
        .map(|h| Value::make_list(h.keys().map(Clone::clone).map(Value::from).collect()))
        .ok_or_else(|| e::arg_type("assoc", "hash-map", 0))
}

pub fn vals(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "vals")?;

    args[0]
        .hashmap()
        .map(|h| Value::make_list(h.values().map(Clone::clone).collect()))
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

    Ok(Value::make_hashmap(map))
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

    Ok(Value::make_hashmap(map))
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
        .map(Value::make_hashmap)
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
    Ok(Value::make_vector(args))
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

pub fn map(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 2, 2, "map")?;

    let mut args = args.iter();
    let func = args.next().unwrap();
    let seq = args
        .next()
        .and_then(Value::sequence)
        .ok_or_else(|| e::arg_type("map", "list or vector", 1))?;

    match func {
        Value::Function(func, _) => seq
            .iter()
            .map(|c| func(vector![c.clone()]))
            .collect::<Result<_, _>>()
            .map(Value::make_list),
        Value::Closure {
            binds,
            body,
            env,
            is_rest,
            ..
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
            .map(Value::make_list),

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
        Value::Nil => Ok(Value::make_list(Vector::new())),
        other => {
            let mut seq = other
                .sequence()
                .ok_or_else(|| e::arg_type("rest", "list, vector or nil", 0))?
                .clone();
            seq.pop_front();
            Ok(Value::make_list(seq))
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
            .map_or_else(|| Ok(Value::Nil), |f| Ok(f.clone())),
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

    Ok(Value::make_list(result))
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
    Ok(Value::make_list(seq))
}

pub fn swap(mut args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n >= 2, "2 or more", "swap!")?;

    match args.pop_front().unwrap() {
        Value::Atom(atom, _) => {
            let func = args.pop_front().unwrap();
            args.push_front(atom.borrow().clone());

            match func {
                Value::Function(func, _) => {
                    let new_val = func(args)?;
                    atom.replace(new_val.clone());
                    Ok(new_val)
                }
                Value::Closure {
                    env,
                    binds,
                    body,
                    is_rest,
                    ..
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
    Ok(Value::make_list(args))
}

pub fn listp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "list?")?;

    Ok(Value::Bool(args[0].list().is_some()))
}

pub fn emptyp(args: Args) -> EvalResult {
    ensure_len(args.len(), |n| n == 1, 1, "empty?")?;

    match &args[0] {
        Value::Nil => Ok(Value::Bool(true)),
        other => other
            .sequence()
            .map(|v| Value::Bool(v.is_empty()))
            .ok_or_else(|| e::arg_type("empty?", "list or vector", 0)),
    }
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
