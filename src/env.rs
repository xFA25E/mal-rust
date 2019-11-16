use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap, VecDeque},
    rc::Rc,
};

use crate::{error::EvalError, value::Value};

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

    pub fn get(&self, k: &Rc<String>) -> Result<Value, EvalError> {
        self.find(k)
            .and_then(|env| env.0.data.borrow().get(k).map(|c| c.clone()))
            .ok_or_else(|| EvalError::SymbolNotFound)
    }

    fn find(&self, k: &Rc<String>) -> Option<Self> {
        self.0.data.borrow().get(k).map_or_else(
            || self.0.outer.as_ref().and_then(|e| e.find(k)),
            |_| Some(Self(Rc::clone(&self.0))),
        )
    }

    pub fn set(&self, key: Rc<String>, value: Value) -> Value {
        match self.0.data.borrow_mut().entry(key) {
            Entry::Occupied(mut o) => {
                *o.get_mut() = value;
                o.get().clone()
            }
            Entry::Vacant(v) => v.insert(value).clone(),
        }
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
        use crate::core::{
            add, count, divide, emptyp, equal, greater, greater_equal, less, less_equal, list,
            listp, multiply, pr_str, println, prn, str, subtract, Args, EvalResult,
        };

        let funcs: &[(&str, fn(Args) -> EvalResult)] = &[
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
        ];

        for (n, f) in funcs {
            self.data.insert(Rc::new((*n).into()), Value::Function(*f));
        }

        self
    }

    pub fn binds(mut self, vars: &Vec<Rc<String>>, mut values: Rc<VecDeque<Value>>) -> Self {
        let mut vars_iter = vars.iter();
        let values_ref = Rc::make_mut(&mut values);

        while let Some(s) = vars_iter.next() {
            if s.as_ref() != "&" {
                let value = values_ref
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
