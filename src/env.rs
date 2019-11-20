use im_rc::Vector;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::value::Value;

pub struct Env(Rc<LispEnv>);

struct LispEnv {
    data: RefCell<HashMap<Rc<String>, Value>>,
    outer: Option<Env>,
}

impl Env {
    pub fn new() -> EnvBuilder {
        EnvBuilder {
            data: HashMap::new(),
            outer: None,
        }
    }

    pub fn get(&self, k: &Rc<String>) -> Option<Value> {
        self.0
            .data
            .borrow()
            .get(k)
            .map(|c| c.clone())
            .or_else(|| self.0.outer.as_ref().and_then(|e| e.get(k)))
    }

    pub fn most_outer(&self) -> Self {
        self.0
            .outer
            .as_ref()
            .map_or_else(|| self.clone(), |o| o.most_outer())
    }

    pub fn set(&self, key: Rc<String>, value: Value) -> Value {
        self.0.data.borrow_mut().insert(key, value.clone());
        value
    }
}

impl Clone for Env {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

pub struct EnvBuilder {
    data: HashMap<Rc<String>, Value>,
    outer: Option<Env>,
}

impl EnvBuilder {
    pub fn env(mut self, env: Env) -> Self {
        self.outer = Some(env);
        self
    }

    pub fn with_core(mut self) -> Self {
        use crate::{
            core::{
                add, apply, assoc, atom, atomp, concat, cons, containsp, count, deref, dissoc,
                divide, emptyp, equal, falsep, first, get, greater, greater_equal, hash_map, keys,
                keyword, keywordp, less, less_equal, list, listp, map, mapp, multiply, nilp, nth,
                pr_str, println, prn, read_string, reset, rest, sequentialp, slurp, str, subtract,
                swap, symbol, symbolp, throw, truep, vals, vector, vectorp,
            },
            value::Function,
        };

        let funcs: &[(&str, Function)] = &[
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
            ("apply", apply),
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
        ];

        for (n, f) in funcs {
            self.data.insert(Rc::new((*n).into()), Value::Function(*f));
        }

        self.data
            .insert(Rc::new("*ARGV*".into()), Value::List(Vector::new()));

        self
    }

    pub fn binds(mut self, vars: &Vec<Rc<String>>, values: Vector<Value>) -> Self {
        let mut vars_iter = vars.iter();
        let mut values = values.clone();

        while let Some(s) = vars_iter.next() {
            if s.as_ref() != "&" {
                let value = values
                    .pop_front()
                    .expect("eval bug: not enough arguments! Should check at before!");

                self.data.insert(Rc::clone(s), value);
            } else {
                let next_sym = vars_iter
                    .next()
                    .expect("eval bug: not enough arguments for rest (&) parameters");

                self.data.insert(next_sym.clone(), Value::List(values));

                if let Some(_) = vars_iter.next() {
                    panic!("too much arguments after rest (&) parameters");
                }
                break;
            }
        }

        if let Some(_) = vars_iter.next() {
            panic!("not enough values for function");
        }

        self
    }

    pub fn make(mut self) -> Env {
        let outer = self.outer.take();

        Env(Rc::new(LispEnv {
            data: RefCell::new(self.data),
            outer,
        }))
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
