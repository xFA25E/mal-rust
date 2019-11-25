use im_rc::Vector;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    core::{ensure_len, Pairs},
    error as e, eval,
    value::{Args, Binds, Value},
};

pub struct Env(Rc<LispEnv>);

struct LispEnv {
    data: RefCell<HashMap<Rc<String>, Value>>,
    outer: Option<Env>,
}

impl Env {
    pub fn new() -> Self {
        Self(Rc::new(LispEnv {
            data: RefCell::new(HashMap::new()),
            outer: None,
        }))
    }

    pub fn with_env(env: Self) -> Self {
        Self(Rc::new(LispEnv {
            data: RefCell::new(HashMap::new()),
            outer: Some(env),
        }))
    }

    pub fn get(&self, k: &Rc<String>) -> Option<Value> {
        self.0
            .data
            .borrow()
            .get(k)
            .map(Clone::clone)
            .or_else(|| self.0.outer.as_ref().and_then(|e| e.get(k)))
    }

    pub fn most_outer(&self) -> Self {
        self.0
            .outer
            .as_ref()
            .map_or_else(|| self.clone(), Env::most_outer)
    }

    pub fn set(&self, key: Rc<String>, value: Value) -> Value {
        self.0.data.borrow_mut().insert(key, value.clone());
        value
    }

    pub fn let_binds(self, binds: Vector<Value>) -> Result<Self, Value> {
        ensure_len(binds.len(), |n| n % 2 == 0, "even number of args", "let*")?;

        for (i, (k, v)) in Pairs(binds.iter()).enumerate() {
            let key = k
                .symbol()
                .map(Rc::clone)
                .ok_or_else(|| e::arg_type("let*", "symbol", i * 2))?;
            let value = eval(v.clone(), self.clone())?;

            self.0.data.borrow_mut().insert(key, value);
        }

        Ok(self)
    }

    pub fn fn_binds(self, binds: Binds, args: Args, is_rest: bool) -> Result<Self, Value> {
        let (alen, len, mut take) = (args.len(), binds.len(), binds.len());
        if is_rest {
            ensure_len(
                alen,
                |n| n >= len - 1,
                format!("{} or more", len - 1),
                "_fn*_",
            )?;
            take -= 1;
        } else {
            ensure_len(alen, |n| n >= len, len, "_fn*_")?;
        }

        {
            let mut href = self.0.data.borrow_mut();

            for (k, v) in binds.iter().zip(args.iter()).take(take) {
                href.insert(Rc::clone(&k), v.clone());
            }

            if is_rest {
                href.insert(
                    Rc::clone(&binds[take]),
                    Value::make_list(args.iter().skip(take).map(Clone::clone).collect()),
                );
            }
        }
        Ok(self)
    }

    pub fn with_core(self) -> Self {
        use crate::{
            core::{
                add, assoc, atom, atomp, concat, conj, cons, containsp, count, deref, dissoc,
                divide, emptyp, equal, falsep, first, fnp, get, greater, greater_equal, hash_map,
                keys, keyword, keywordp, less, less_equal, list, listp, macrop, map, mapp, meta,
                multiply, nilp, nth, numberp, pr_str, println, prn, read_string, readline, reset,
                rest, seq, sequentialp, slurp, str, stringp, subtract, swap, symbol, symbolp,
                throw, time_ms, truep, vals, vector, vectorp, with_meta,
            },
            value::BuiltinFunction,
        };

        let funcs: &[(&str, BuiltinFunction)] = &[
            ("+", add),
            ("-", subtract),
            ("*", multiply),
            ("/", divide),
            ("prn", prn),
            ("list", list),
            ("list?", listp),
            ("empty?", emptyp),
            ("count", count),
            ("=", equal),
            ("<", less),
            ("<=", less_equal),
            (">", greater),
            (">=", greater_equal),
            ("pr-str", pr_str),
            ("str", str),
            ("println", println),
            ("read-string", read_string),
            ("slurp", slurp),
            ("swap!", swap),
            ("reset!", reset),
            ("deref", deref),
            ("atom?", atomp),
            ("atom", atom),
            ("cons", cons),
            ("concat", concat),
            ("first", first),
            ("nth", nth),
            ("rest", rest),
            ("throw", throw),
            ("map", map),
            // ("apply", apply),
            ("nil?", nilp),
            ("true?", truep),
            ("false?", falsep),
            ("symbol?", symbolp),
            ("keys", keys),
            ("vals", vals),
            ("contains?", containsp),
            ("get", get),
            ("dissoc", dissoc),
            ("assoc", assoc),
            ("map?", mapp),
            ("hash-map", hash_map),
            ("sequential?", sequentialp),
            ("vector?", vectorp),
            ("vector", vector),
            ("keyword", keyword),
            ("keyword?", keywordp),
            ("symbol", symbol),
            ("readline", readline),
            ("time-ms", time_ms),
            ("meta", meta),
            ("with-meta", with_meta),
            ("fn?", fnp),
            ("macro?", macrop),
            ("string?", stringp),
            ("number?", numberp),
            ("seq", seq),
            ("conj", conj),
        ];

        {
            let mut href = self.0.data.borrow_mut();
            for (n, f) in funcs {
                href.insert(Rc::new((*n).into()), Value::make_function(*f));
            }

            href.insert(Rc::new("*ARGV*".into()), Value::make_list(Vector::new()));
            href.insert(
                Rc::new("*host-language*".into()),
                Value::make_string("rust"),
            );
            href.insert(Rc::new("apply".into()), Value::make_symbol("apply"));
        }
        self
    }
}

impl Clone for Env {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

#[cfg(test)]
mod eval_tests {
    use super::*;

    #[test]
    #[should_panic]
    fn test_add() {
        let env = Env::new().with_core().make();

        match env.get("+") {
            Ok(Value::Function(f)) => println!(
                "{:?}",
                if let Ok(n) = f(vec![Value::Number(1)].into_iter().collect()) {
                    n
                } else {
                    panic!("hello")
                }
            ),
            Ok(o) => println!("{:?}", o),
            Err(e) => println!("{}", e),
        }
    }
}
