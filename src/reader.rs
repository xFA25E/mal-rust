use lazy_static::lazy_static;
use regex::Regex;

use std::{
    collections::{HashMap, VecDeque},
    convert::TryInto,
    iter::Peekable,
};

use crate::{error::ReadError, value::Value};

lazy_static! {
    static ref TOKEN_RE: Regex =
        Regex::new(r###"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"###)
            .unwrap();
}

pub fn read(s: &str) -> ReadResult {
    Reader(
        TOKEN_RE
            .captures_iter(s)
            .filter_map(|c| c.get(1).map(|c| c.as_str()))
            .peekable(),
    )
    .read()
}

type ReadResult = Result<Value, ReadError>;

struct Reader<'s, I: Iterator<Item = &'s str>>(Peekable<I>);

impl<'s, I: Iterator<Item = &'s str>> Reader<'s, I> {
    fn read(&mut self) -> ReadResult {
        let token: &str = self.0.next().ok_or_else(|| ReadError::UnexpectedEnd)?;

        match token {
            "'" => self.read_quote(),
            "`" => self.read_quasiquote(),
            "~" => self.read_unquote(),
            "~@" => self.read_splice_unquote(),
            "@" => self.read_deref(),
            token if token.starts_with('(') => self.read_list(),
            token if token.starts_with('[') => self.read_vector(),
            token if token.starts_with('{') => self.read_hash_map(),
            token if token.starts_with(';') => Ok(Value::Comment(())),
            token => Self::read_atom(token),
        }
    }

    fn read_ignore_comment(&mut self) -> ReadResult {
        let mut value = self.read()?;
        while let Value::Comment(_) = value {
            value = self.read()?;
        }
        Ok(value)
    }

    fn read_list(&mut self) -> ReadResult {
        let mut list = VecDeque::new();
        loop {
            let token = self.0.peek().ok_or_else(|| ReadError::UnexpectedEnd)?;
            if token.starts_with(')') {
                self.0.next();
                return Ok(Value::List(list));
            }
            list.push_back(self.read_ignore_comment()?);
        }
    }

    fn read_vector(&mut self) -> ReadResult {
        let mut vec = Vec::new();
        loop {
            let token = self.0.peek().ok_or_else(|| ReadError::UnexpectedEnd)?;
            if token.starts_with(']') {
                self.0.next();
                return Ok(Value::Vector(vec));
            }
            vec.push(self.read_ignore_comment()?);
        }
    }

    fn read_hash_map(&mut self) -> ReadResult {
        let mut map = HashMap::new();
        let end = '}';
        loop {
            let token = self.0.peek().ok_or_else(|| ReadError::UnexpectedEnd)?;
            if token.starts_with(end) {
                self.0.next();
                return Ok(Value::HashMap(map));
            }
            let key = self.read_ignore_comment()?.try_into()?;

            let token = self.0.peek().ok_or_else(|| ReadError::UnexpectedEnd)?;
            if token.starts_with(end) {
                self.0.next();
                return Err(ReadError::UnterminatedHashMapKey);
            }
            map.insert(key, self.read_ignore_comment()?);
        }
    }

    fn read_quote(&mut self) -> ReadResult {
        self.read_read_macro("quote")
    }

    fn read_quasiquote(&mut self) -> ReadResult {
        self.read_read_macro("quasiquote")
    }

    fn read_unquote(&mut self) -> ReadResult {
        self.read_read_macro("unquote")
    }

    fn read_splice_unquote(&mut self) -> ReadResult {
        self.read_read_macro("splice-unquote")
    }

    fn read_deref(&mut self) -> ReadResult {
        self.read_read_macro("deref")
    }

    fn read_read_macro(&mut self, rm: &str) -> ReadResult {
        Ok(Value::List(
            vec![Value::Symbol(rm.into()), self.read_ignore_comment()?]
                .into_iter()
                .collect(),
        ))
    }

    fn read_atom(token: &str) -> ReadResult {
        match token {
            "nil" => Ok(Value::Nil),
            "true" => Ok(Value::Bool(true)),
            "false" => Ok(Value::Bool(false)),
            token if token.starts_with(':') => {
                if token.len() == 1 {
                    Err(ReadError::EmptyKeyword)
                } else {
                    Ok(Value::Keyword(token[1..].into()))
                }
            }
            token if token.starts_with('"') => Self::read_string(token),
            token => token
                .parse()
                .map(Value::Number)
                .or_else(|_| Ok(Value::Symbol(token.into()))),
        }
    }

    fn read_string(token: &str) -> Result<Value, ReadError> {
        if token.ends_with('"') {
            let length = token.chars().count();
            let mut result = String::new();
            let mut is_prev_backslash = false;
            let mut should_end = false;

            for char in token.chars().skip(1).take(length - 2) {
                if should_end {
                    return Err(ReadError::UnterminatedString);
                }

                match char {
                    '\\' => {
                        if is_prev_backslash {
                            result.push('\\');
                            is_prev_backslash = false;
                        } else {
                            is_prev_backslash = true;
                        }
                    }
                    'n' => {
                        if is_prev_backslash {
                            result.push('\n');
                            is_prev_backslash = false;
                        } else {
                            result.push('n');
                        }
                    }
                    '"' => {
                        if is_prev_backslash {
                            result.push('"');
                            is_prev_backslash = false;
                        } else {
                            should_end = true;
                        }
                    }
                    c => {
                        if is_prev_backslash {
                            result.push('\\');
                            is_prev_backslash = false;
                        }
                        result.push(c);
                    }
                }
            }

            Ok(Value::String(result))
        } else {
            Err(ReadError::UnterminatedString)
        }
    }
}
