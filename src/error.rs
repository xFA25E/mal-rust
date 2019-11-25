use std::fmt::Display;

use crate::value::Value;

pub fn eof() -> Value {
    Value::make_string("EOF")
}

#[inline]
pub fn unexpected_end<D: Display>(expected: D) -> Value {
    Value::make_string(format!(
        "Read ended unexpectedly. Expected \"{}\" somewhere",
        expected
    ))
}

#[inline]
pub fn unterminated_string() -> Value {
    Value::make_string("String is not terminated with \"")
}

#[inline]
pub fn empty_keyword() -> Value {
    Value::make_string("Found an empty keyword (just a colon \":\")")
}

#[inline]
pub fn invalid_hash_key<D: Display>(provided: D) -> Value {
    Value::make_string(format!(
        "Can't use \"{}\" as a hash-map key.\n\
         Possible hash-map keys:\n\
         keywords, strings, symbols, numbers, booleans, nil",
        provided
    ))
}

#[inline]
pub fn odd_hash_map_args() -> Value {
    Value::make_string(
        "Number of hash-map arguments is not even.\n\
         One of the keys does not have any value",
    )
}

#[inline]
pub fn arg_type<F, D, N>(func: F, arg_type: D, pos: N) -> Value
where
    F: Display,
    D: Display,
    N: Display,
{
    Value::make_string(format!(
        "Provided argument type to \"{}\" at position {}, is not valid.\n\
         Expected \"{}\"",
        func, pos, arg_type
    ))
}

#[inline]
pub fn arg_count<D, S, N>(func: D, required: S, provided: N) -> Value
where
    D: Display,
    S: Display,
    N: Display,
{
    Value::make_string(format!(
        "{}: invalid number of arguments. Required: {}. Provided: {}",
        func, required, provided
    ))
}

#[inline]
pub fn symbol_not_found<D: Display>(symbol: D) -> Value {
    Value::make_string(format!("It seems that symbol \"{}\" is undefined", symbol))
}

#[inline]
pub fn not_function<D: Display>(element: D) -> Value {
    Value::make_string(format!("\"{}\" is not a function", element))
}

#[inline]
pub fn rest_parameter() -> Value {
    Value::make_string(
        "Invalid rest-parameter (&) syntax in \"fn*\".\n\
         \"&\" should be in the penultimate position",
    )
}

#[inline]
pub fn catch_block() -> Value {
    Value::make_string("Invalid catch block. Expected \"catch*\" symbol")
}

#[inline]
pub fn io<D: Display>(error: D) -> Value {
    Value::make_string(format!("Io error: {}", error))
}

#[inline]
pub fn numeric_overflow<D, N>(func: D, first: N, second: N) -> Value
where
    D: Display,
    N: Display,
{
    Value::make_string(format!(
        "Encountered numeric overflow in \"{}\". Arguments: {}, {}",
        func, first, second
    ))
}
