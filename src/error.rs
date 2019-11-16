use std::{
    fmt::{self, Display},
    io::Error as IoError,
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
    Read(ReadError),
    Io(IoError),
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
            Self::Read(r) => write!(f, "{}", r),
            Self::Io(i) => write!(f, "{}", i),
        }
    }
}

impl From<ReadError> for EvalError {
    fn from(source: ReadError) -> Self {
        Self::Read(source)
    }
}

impl From<IoError> for EvalError {
    fn from(source: IoError) -> Self {
        Self::Io(source)
    }
}
