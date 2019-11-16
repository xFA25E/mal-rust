use std::{collections::VecDeque, fmt::Write};

use crate::{error::EvalError, value::Value};

pub fn println(args: VecDeque<Value>) -> Result<Value, EvalError> {
    let mut iter = args.into_iter();
    if let Some(v) = iter.next() {
        print!("{}", v);
    }

    for v in iter {
        print!(" {}", v);
    }
    println!();
    Ok(Value::Nil)
}

pub fn prn(args: VecDeque<Value>) -> Result<Value, EvalError> {
    let mut iter = args.into_iter();
    if let Some(v) = iter.next() {
        print!("{:?}", v);
    }

    for v in iter {
        print!(" {:?}", v);
    }
    println!();
    Ok(Value::Nil)
}

pub fn str(args: VecDeque<Value>) -> Result<Value, EvalError> {
    let mut result = String::new();
    for s in args.into_iter() {
        write!(result, "{}", s).unwrap();
    }
    let result = Value::String(result);
    Ok(result)
}

pub fn pr_str(args: VecDeque<Value>) -> Result<Value, EvalError> {
    let mut result = String::new();
    for s in args.into_iter() {
        write!(result, "{:?} ", s).unwrap();
    }
    result.pop();
    let result = Value::String(result);
    Ok(result)
}

pub fn list(args: VecDeque<Value>) -> Result<Value, EvalError> {
    Ok(Value::List(args))
}

pub fn listp(mut args: VecDeque<Value>) -> Result<Value, EvalError> {
    if args.len() == 1 {
        let thing = args.pop_front().unwrap();
        match thing {
            Value::List(_) => Ok(Value::Bool(true)),
            _ => Ok(Value::Bool(false)),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn emptyp(mut args: VecDeque<Value>) -> Result<Value, EvalError> {
    if args.len() == 1 {
        let list = args.pop_front().unwrap();
        match list {
            Value::List(l) => Ok(Value::Bool(l.is_empty())),
            Value::Vector(v) => Ok(Value::Bool(v.is_empty())),
            _ => Err(EvalError::InvalidArgumentType),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn count(mut args: VecDeque<Value>) -> Result<Value, EvalError> {
    if args.len() == 1 {
        let list = args.pop_front().unwrap();
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

pub fn equal(mut args: VecDeque<Value>) -> Result<Value, EvalError> {
    if args.len() == 2 {
        let first = args.pop_front().unwrap();
        let second = args.pop_front().unwrap();
        Ok(Value::Bool(first == second))
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn less(mut args: VecDeque<Value>) -> Result<Value, EvalError> {
    if args.len() == 2 {
        match args.pop_front().unwrap() {
            Value::Number(first) => match args.pop_front().unwrap() {
                Value::Number(second) => Ok(Value::Bool(first < second)),
                _ => Err(EvalError::InvalidArgumentType),
            },
            _ => Err(EvalError::InvalidArgumentType),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn less_equal(mut args: VecDeque<Value>) -> Result<Value, EvalError> {
    if args.len() == 2 {
        match args.pop_front().unwrap() {
            Value::Number(first) => match args.pop_front().unwrap() {
                Value::Number(second) => Ok(Value::Bool(first <= second)),
                _ => Err(EvalError::InvalidArgumentType),
            },
            _ => Err(EvalError::InvalidArgumentType),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn greater(mut args: VecDeque<Value>) -> Result<Value, EvalError> {
    if args.len() == 2 {
        match args.pop_front().unwrap() {
            Value::Number(first) => match args.pop_front().unwrap() {
                Value::Number(second) => Ok(Value::Bool(first > second)),
                _ => Err(EvalError::InvalidArgumentType),
            },
            _ => Err(EvalError::InvalidArgumentType),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn greater_equal(mut args: VecDeque<Value>) -> Result<Value, EvalError> {
    if args.len() == 2 {
        match args.pop_front().unwrap() {
            Value::Number(first) => match args.pop_front().unwrap() {
                Value::Number(second) => Ok(Value::Bool(first >= second)),
                _ => Err(EvalError::InvalidArgumentType),
            },
            _ => Err(EvalError::InvalidArgumentType),
        }
    } else {
        Err(EvalError::InvalidNumberOfArguments)
    }
}

pub fn add(args: VecDeque<Value>) -> Result<Value, EvalError> {
    arithmetic_operation(args, i128::checked_add)
}

pub fn subtract(args: VecDeque<Value>) -> Result<Value, EvalError> {
    arithmetic_operation(args, i128::checked_sub)
}

pub fn multiply(args: VecDeque<Value>) -> Result<Value, EvalError> {
    arithmetic_operation(args, i128::checked_mul)
}

pub fn divide(args: VecDeque<Value>) -> Result<Value, EvalError> {
    arithmetic_operation(args, i128::checked_div)
}

fn arithmetic_operation(
    args: VecDeque<Value>,
    operation: fn(i128, i128) -> Option<i128>,
) -> Result<Value, EvalError> {
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
