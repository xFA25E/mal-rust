use im_rc::{vector, HashMap, Vector};
use lazy_static::lazy_static;
use regex::Regex;

use std::{convert::TryInto, iter::Peekable};

use crate::{
    error as e,
    value::{EvalResult, Value},
};

lazy_static! {
    static ref TOKEN_RE: Regex =
        Regex::new(r###"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"###)
            .unwrap();
}

pub fn read(s: &str) -> EvalResult {
    Reader(
        TOKEN_RE
            .captures_iter(s)
            .filter_map(|c| c.get(1).map(|c| c.as_str()))
            .peekable(),
    )
    .read()
}

struct Reader<'s, I: Iterator<Item = &'s str>>(Peekable<I>);

impl<'s, I: Iterator<Item = &'s str>> Reader<'s, I> {
    fn read(&mut self) -> EvalResult {
        let token: &str = self
            .0
            .next()
            .ok_or_else(|| e::unexpected_end("something"))?;

        match token {
            "'" => self.read_quote(),
            "`" => self.read_quasiquote(),
            "~" => self.read_unquote(),
            "~@" => self.read_splice_unquote(),
            "@" => self.read_deref(),
            token if token.starts_with('(') => self.read_list(),
            token if token.starts_with('[') => self.read_vector(),
            token if token.starts_with('{') => self.read_hash_map(),
            token if token.starts_with(';') => Ok(Value::Comment),
            token => Self::read_atom(token),
        }
    }

    fn read_ignore_comment(&mut self) -> EvalResult {
        let mut value = self.read()?;
        while let Value::Comment = value {
            value = self.read()?;
        }
        Ok(value)
    }

    fn read_list(&mut self) -> EvalResult {
        self.read_sequence(')').map(Value::List)
    }

    fn read_vector(&mut self) -> EvalResult {
        self.read_sequence(']').map(Value::Vector)
    }

    fn read_sequence(&mut self, end: char) -> Result<Vector<Value>, Value> {
        let mut vec = Vector::new();
        loop {
            let token = self.0.peek().ok_or_else(|| e::unexpected_end(']'))?;
            if token.starts_with(end) {
                self.0.next();
                return Ok(vec);
            }
            vec.push_back(self.read_ignore_comment()?);
        }
    }

    fn read_hash_map(&mut self) -> EvalResult {
        let mut map = HashMap::new();
        let end = '}';
        loop {
            let token = self.0.peek().ok_or_else(|| e::unexpected_end('}'))?;
            if token.starts_with(end) {
                self.0.next();
                return Ok(Value::HashMap(map));
            }
            let key = self.read_ignore_comment()?.try_into()?;

            let token = self
                .0
                .peek()
                .ok_or_else(|| e::unexpected_end("hash-map value"))?;
            if token.starts_with(end) {
                self.0.next();
                return Err(e::odd_hash_map_args());
            }
            map.insert(key, self.read_ignore_comment()?);
        }
    }

    fn read_quote(&mut self) -> EvalResult {
        self.read_read_macro("quote")
    }

    fn read_quasiquote(&mut self) -> EvalResult {
        self.read_read_macro("quasiquote")
    }

    fn read_unquote(&mut self) -> EvalResult {
        self.read_read_macro("unquote")
    }

    fn read_splice_unquote(&mut self) -> EvalResult {
        self.read_read_macro("splice-unquote")
    }

    fn read_deref(&mut self) -> EvalResult {
        self.read_read_macro("deref")
    }

    fn read_read_macro(&mut self, rm: &str) -> EvalResult {
        Ok(Value::List(vector![
            Value::make_symbol(rm),
            self.read_ignore_comment()?,
        ]))
    }

    fn read_atom(token: &str) -> EvalResult {
        match token {
            "nil" => Ok(Value::Nil),
            "true" => Ok(Value::Bool(true)),
            "false" => Ok(Value::Bool(false)),
            token if token.starts_with(':') => {
                if token.len() == 1 {
                    Err(e::empty_keyword())
                } else {
                    Ok(Value::make_keyword(&token[1..]))
                }
            }
            token if token.starts_with('"') => Self::read_string(token),
            token => token
                .parse()
                .map(Value::Number)
                .or_else(|_| Ok(Value::make_symbol(token))),
        }
    }

    fn read_string(token: &str) -> EvalResult {
        if token.ends_with('"') {
            let length = token.chars().count();
            let mut result = String::new();
            let mut is_prev_backslash = false;
            let mut should_end = false;

            for char in token.chars().skip(1).take(length - 2) {
                if should_end {
                    return Err(e::unterminated_string());
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

            Ok(Value::make_string(result))
        } else {
            Err(e::unterminated_string())
        }
    }
}
