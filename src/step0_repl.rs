use std::io::{self, stdin, stdout, BufRead, Write};

#[allow(non_snake_case)]
fn READ(s: String) -> String {
    s
}

#[allow(non_snake_case)]
fn EVAL(s: String) -> String {
    s
}

#[allow(non_snake_case)]
fn PRINT(s: String) -> String {
    s
}

fn rep(s: String) -> String {
    PRINT(EVAL(READ(s)))
}

pub fn main() -> io::Result<()> {
    write!(stdout().lock(), "user> ")?;
    stdout().lock().flush()?;

    loop {
        let mut line = String::new();
        stdin().lock().read_line(&mut line)?;

        write!(stdout().lock(), "{}\nuser> ", rep(line))?;
        stdout().lock().flush()?;
    }
}
