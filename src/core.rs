use std::{cell::RefCell, collections::VecDeque, fmt::Write, fs::read_to_string, rc::Rc};

use crate::{env::Env, error::EvalError, eval, reader::read, value::Value};

pub type Args = Rc<VecDeque<Value>>;
pub type EvalResult = Result<Value, EvalError>;

pub fn rest(args: Args) -> EvalResult {
    match args.get(0).map(|c| c.clone()) {
        Some(Value::List(mut l)) => {
            let list_ref = Rc::make_mut(&mut l);
            list_ref.pop_front();
            Ok(Value::List(l))
        }
        Some(Value::Vector(v)) => {
            if v.len() > 1 {
                Ok(Value::List(Rc::new(
                    v[1..].iter().map(|c| c.clone()).collect(),
                )))
            } else {
                Ok(Value::List(Rc::new(VecDeque::new())))
            }
        }
        Some(Value::Nil) => Ok(Value::List(Rc::new(VecDeque::new()))),
        Some(_) => Err(EvalError::InvalidArgumentType),
        None => Err(EvalError::InvalidNumberOfArguments),
    }
}

pub fn first(args: Args) -> EvalResult {
    match args.get(0) {
        Some(Value::List(l)) => Ok(l.get(0).map(|c| c.clone()).unwrap_or_else(|| Value::Nil)),
        Some(Value::Vector(v)) => Ok(v.get(0).map(|c| c.clone()).unwrap_or_else(|| Value::Nil)),
        Some(Value::Nil) => Ok(Value::Nil),
        Some(_) => Err(EvalError::InvalidArgumentType),
        None => Err(EvalError::InvalidNumberOfArguments),
    }
}

pub fn nth(args: Args) -> EvalResult {
    if args.len() == 2 {
        let mut iter = args.iter();
        let seq = iter.next().unwrap();
        match iter.next().unwrap() {
            Value::Number(n) => match seq {
                Value::List(l) => Ok(l.get(*n as usize).unwrap().clone()),
                Value::Vector(v) => Ok(v.get(*n as usize).unwrap().clone()),
                _ => Err(EvalError::InvalidArgumentType),
            },
            _ => Err(EvalError::InvalidArgumentType),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
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
            _ => return Err(EvalError::InvalidArgumentType),
        }
    }
    Ok(Value::List(Rc::new(result)))
}

pub fn cons(args: Args) -> EvalResult {
    if args.len() >= 2 {
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
            _ => Err(EvalError::InvalidArgumentType),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn swap(mut args: Args) -> EvalResult {
    if args.len() >= 2 {
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
                            _ => return Err(EvalError::InvalidNumberOfArguments),
                        }
                    }

                    let closure_env = Env::new().env(env).binds(&binds, args).make();
                    let new_val = eval(body.as_ref().clone(), closure_env)?;
                    atom.replace(new_val.clone());
                    Ok(new_val)
                }

                _ => Err(EvalError::InvalidArgumentType),
            },
            _ => Err(EvalError::InvalidArgumentType),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn reset(args: Args) -> EvalResult {
    if args.len() == 2 {
        let mut iter = args.iter();
        match iter.next().unwrap() {
            Value::Atom(atom) => {
                let value = iter.next().unwrap();
                atom.replace(value.clone());
                Ok(value.clone())
            }
            _ => Err(EvalError::InvalidArgumentType),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn deref(args: Args) -> EvalResult {
    match args.front() {
        Some(Value::Atom(a)) => Ok(a.borrow().clone()),
        Some(_) => Err(EvalError::InvalidArgumentType),
        None => Err(EvalError::InvalidNumberOfArguments),
    }
}

pub fn atomp(args: Args) -> EvalResult {
    match args.front() {
        Some(Value::Atom(_)) => Ok(Value::Bool(true)),
        Some(_) => Err(EvalError::InvalidArgumentType),
        None => Err(EvalError::InvalidNumberOfArguments),
    }
}

pub fn atom(args: Args) -> EvalResult {
    match args.front() {
        Some(val) => Ok(Value::Atom(Rc::new(RefCell::new(val.clone())))),
        None => Err(EvalError::InvalidNumberOfArguments),
    }
}

pub fn read_string(args: Args) -> EvalResult {
    match args.front() {
        Some(Value::String(s)) => Ok(read(s)?),
        Some(_) => Err(EvalError::InvalidArgumentType),
        None => Err(EvalError::InvalidNumberOfArguments),
    }
}

pub fn slurp(args: Args) -> EvalResult {
    match args.front() {
        Some(Value::String(s)) => Ok(Value::String(Rc::new(read_to_string(s.as_ref())?))),
        Some(_) => Err(EvalError::InvalidArgumentType),
        None => Err(EvalError::InvalidNumberOfArguments),
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
    if args.len() == 1 {
        let thing = args.get(0).unwrap();
        match thing {
            Value::List(_) => Ok(Value::Bool(true)),
            _ => Ok(Value::Bool(false)),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn emptyp(args: Args) -> EvalResult {
    if args.len() == 1 {
        let list = args.get(0).unwrap();
        match list {
            Value::List(l) => Ok(Value::Bool(l.is_empty())),
            Value::Vector(v) => Ok(Value::Bool(v.is_empty())),
            _ => Err(EvalError::InvalidArgumentType),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn count(args: Args) -> EvalResult {
    if args.len() == 1 {
        let list = args.get(0).unwrap();
        match list {
            Value::List(l) => Ok(Value::Number(l.len() as i128)),
            Value::Vector(v) => Ok(Value::Number(v.len() as i128)),
            Value::Nil => Ok(Value::Number(0)),
            _ => Err(EvalError::InvalidArgumentType),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn equal(args: Args) -> EvalResult {
    if args.len() == 2 {
        let first = args.get(0).unwrap();
        let second = args.get(1).unwrap();
        Ok(Value::Bool(first == second))
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
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

fn arithmetic_compare(args: Args, operation: fn(&i128, &i128) -> bool) -> EvalResult {
    if args.len() == 2 {
        let mut iter = args.iter();
        let first = iter.next().unwrap().number()?;
        let second = iter.next().unwrap().number()?;
        Ok(Value::Bool(operation(&first, &second)))
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
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

fn arithmetic_operation(args: Args, operation: fn(i128, i128) -> Option<i128>) -> EvalResult {
    if args.len() == 2 {
        let mut iter = args.iter();
        let first = iter.next().unwrap().number()?;
        let second = iter.next().unwrap().number()?;
        operation(first, second)
            .map(Value::Number)
            .ok_or_else(|| EvalError::NumericOverflow)
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}
