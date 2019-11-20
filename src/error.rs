use std::{fmt::Display, rc::Rc};

use crate::value::Value;

#[inline]
pub fn unexpected_end<D: Display>(expected: D) -> Value {
    Value::String(Rc::new(format!(
        "Read ended unexpectedly. Expected \"{}\" somewhere",
        expected
    )))
}

#[inline]
pub fn unterminated_string<OK>() -> Result<OK, Value> {
    Err(Value::String(Rc::new(
        "String is not terminated with \"".into(),
    )))
}

#[inline]
pub fn empty_keyword<OK>() -> Result<OK, Value> {
    Err(Value::String(Rc::new(
        "Found an empty keyword (just a colon \":\")".into(),
    )))
}

#[inline]
pub fn invalid_hash_key<D: Display, OK>(provided: D) -> Result<OK, Value> {
    Err(Value::String(Rc::new(format!(
        "Can't use \"{}\" as a hash-map key.\n\
         Possible hash-map keys:\n\
         keywords, strings, symbols, numbers, booleans, nil",
        provided
    ))))
}

#[inline]
pub fn odd_hash_map_args<OK>() -> Result<OK, Value> {
    Err(Value::String(Rc::new(
        "Number of hash-map arguments is not even.\n\
         One of the keys does not have any value"
            .into(),
    )))
}

#[inline]
pub fn arg_type<OK, F, D, N>(func: F, arg_type: D, pos: N) -> Result<OK, Value>
where
    F: Display,
    D: Display,
    N: Display,
{
    Err(Value::String(Rc::new(format!(
        "Provided argument type to \"{}\" at position {}, is not valid.\n\
         Expected \"{}\"",
        func, arg_type, pos
    ))))
}

#[inline]
pub fn arg_count<OK, D, S, N>(func: D, required: S, provided: N) -> Result<OK, Value>
where
    D: Display,
    S: Display,
    N: Display,
{
    Err(Value::String(Rc::new(format!(
        "{}: invalid number of arguments. Required: {}. Provided: {}",
        func, required, provided
    ))))
}

#[inline]
pub fn symbol_not_found<D: Display>(symbol: D) -> Value {
    Value::String(Rc::new(format!(
        "It seems that symbol \"{}\" is undefined",
        symbol
    )))
}

#[inline]
pub fn not_function<OK, D: Display>(element: D) -> Result<OK, Value> {
    Err(Value::String(Rc::new(format!(
        "\"{}\" is not a function",
        element
    ))))
}

#[inline]
pub fn rest_parameter<OK>() -> Result<OK, Value> {
    Err(Value::String(Rc::new(
        "Invalid rest-parameter (&) syntax in \"fn*\".\n\
         \"&\" should be in the penultimate position"
            .into(),
    )))
}

#[inline]
pub fn catch_block<OK>() -> Result<OK, Value> {
    Err(Value::String(Rc::new(
        "Invalid catch block. Expected \"catch*\" symbol".into(),
    )))
}

#[inline]
pub fn io<OK, D: Display>(error: D) -> Result<OK, Value> {
    Err(Value::String(Rc::new(format!("Io error: {}", error))))
}

#[inline]
pub fn numeric_overflow<D, N>(func: D, first: N, second: N) -> Value
where
    D: Display,
    N: Display,
{
    Value::String(Rc::new(format!(
        "Encountered numeric overflow in \"{}\". Arguments: {}, {}",
        func, first, second
    )))
}
