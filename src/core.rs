use std::{collections::VecDeque, fmt::Write, rc::Rc};

use crate::{error::EvalError, value::Value};

pub type Args = Rc<VecDeque<Value>>;
pub type EvalResult = Result<Value, EvalError>;

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
