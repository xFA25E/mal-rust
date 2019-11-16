use std::{
    fmt::{self, Display},
    io,
};

#[derive(Debug)]
pub enum ReadError {
    UnexpectedEnd,
    UnterminatedString,
    EmptyKeyword,
    UnterminatedHashMapKey,
    InvalidHashKey,
}

impl Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedEnd => write!(f, "Read ended unexpectedly"),
            Self::UnterminatedString => write!(f, "Could not find string terminator"),
            Self::EmptyKeyword => write!(f, "Empty keyword"),
            Self::InvalidHashKey => write!(f, "Invalid hash key"),
            Self::UnterminatedHashMapKey => {
                write!(f, "One of the hash map keys does not have any value")
            }
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    InvalidArgumentType,
    InvalidNumberOfArguments,
    SymbolNotFound,
    FormIsNotCallable,
    NumericOverflow,
    InvalidFnParameters,
}

impl Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidArgumentType => write!(f, "Invalid argument type"),
            Self::InvalidNumberOfArguments => write!(f, "Invalid number of argumnts"),
            Self::SymbolNotFound => write!(f, "Symbol not found"),
            Self::FormIsNotCallable => write!(f, "Form is not callable"),
            Self::NumericOverflow => write!(f, "Numeric overflow"),
            Self::InvalidFnParameters => write!(f, "Invalid function parameters"),
        }
    }
}

#[derive(Debug)]
pub enum MalError {
    Eval(EvalError),
    Read(ReadError),
    Io(io::Error),
}

impl From<ReadError> for MalError {
    fn from(source: ReadError) -> Self {
        MalError::Read(source)
    }
}

impl From<io::Error> for MalError {
    fn from(source: io::Error) -> Self {
        MalError::Io(source)
    }
}

impl From<EvalError> for MalError {
    fn from(source: EvalError) -> Self {
        MalError::Eval(source)
    }
}

impl Display for MalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Read(r) => write!(f, "{}", r),
            Self::Io(i) => write!(f, "{}", i),
            Self::Eval(e) => write!(f, "{}", e),
        }
    }
}
