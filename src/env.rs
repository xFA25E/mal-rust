use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap, VecDeque},
    rc::Rc,
};

use crate::{error::EvalError, value::Value};

pub struct Env(Rc<LispEnv>);

struct LispEnv {
    data: RefCell<HashMap<String, Value>>,
    outer: Option<Env>,
}

impl Env {
    pub fn new() -> EnvBuilder {
        EnvBuilder {
            data: HashMap::new(),
            outer: None,
        }
    }

    pub fn get(&self, k: &str) -> Result<Value, EvalError> {
        self.find(k)
            .and_then(|env| env.0.data.borrow().get(k).map(|c| c.clone()))
            .ok_or_else(|| EvalError::SymbolNotFound)
    }

    fn find(&self, k: &str) -> Option<Self> {
        self.0.data.borrow().get(k).map_or_else(
            || self.0.outer.as_ref().and_then(|e| e.find(k)),
            |_| Some(Self(Rc::clone(&self.0))),
        )
    }

    pub fn set(&self, key: String, value: Value) -> Value {
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
    data: HashMap<String, Value>,
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
            listp, multiply, pr_str, println, prn, str, subtract,
        };

        self.data.insert("+".into(), Value::Function(add));
        self.data.insert("-".into(), Value::Function(subtract));
        self.data.insert("*".into(), Value::Function(multiply));
        self.data.insert("/".into(), Value::Function(divide));
        self.data.insert("prn".into(), Value::Function(prn));
        self.data.insert("list".into(), Value::Function(list));
        self.data.insert("list?".into(), Value::Function(listp));
        self.data.insert("empty?".into(), Value::Function(emptyp));
        self.data.insert("count".into(), Value::Function(count));
        self.data.insert("=".into(), Value::Function(equal));
        self.data.insert("<".into(), Value::Function(less));
        self.data.insert("<=".into(), Value::Function(less_equal));
        self.data.insert(">".into(), Value::Function(greater));
        self.data
            .insert(">=".into(), Value::Function(greater_equal));
        self.data.insert("pr-str".into(), Value::Function(pr_str));
        self.data.insert("str".into(), Value::Function(str));
        self.data.insert("println".into(), Value::Function(println));
        self
    }

    pub fn binds(mut self, vars: &Vec<String>, mut values: VecDeque<Value>) -> Self {
        let mut iter = vars.iter();

        while let Some(s) = iter.next() {
            if s != "&" {
                self.data.insert(
                    s.clone(),
                    values
                        .pop_front()
                        .expect("eval bug: not enough arguments! Should check at before!"),
                );
            } else {
                let next_sym = iter
                    .next()
                    .expect("eval bug: not enough arguments for rest (&) parameters");
                self.data.insert(next_sym.clone(), Value::List(values));
                if let Some(_) = iter.next() {
                    panic!("too much arguments after rest (&) parameters");
                }
                break;
            }
        }

        if let Some(_) = iter.next() {
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
