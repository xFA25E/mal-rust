use std::io::{stderr, stdin, stdout, BufRead, Write};

use crate::{
    reader::read_str,
    types::{MalError, ReadError, Value},
};

#[allow(non_snake_case)]
fn READ(s: &str) -> Result<Value, ReadError> {
    read_str(s)
}

#[allow(non_snake_case)]
fn EVAL(t: Value) -> Value {
    t
}

#[allow(non_snake_case)]
fn PRINT(t: Value) -> String {
    format!("{:?}", t)
}

fn rep(s: &str) -> Result<String, MalError> {
    Ok(PRINT(EVAL(READ(s)?)))
}

pub fn main() -> Result<(), MalError> {
    let mut line = String::new();

    write!(stdout().lock(), "user> ")?;
    stdout().lock().flush()?;

    loop {
        line.clear();
        stdin().lock().read_line(&mut line)?;

        match rep(&line) {
            Ok(val) => {
                write!(stdout().lock(), "{}\nuser> ", val)?;
                stdout().lock().flush()?;
            }
            Err(e) => {
                write!(stderr().lock(), "{}\nuser> ", e)?;
                stderr().lock().flush()?;
            }
        }
    }
}
