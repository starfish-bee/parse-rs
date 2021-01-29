use std::{error, fmt};

// TODO: kind of a mess- think about where errors should be defined, esp. if making tokens pluggable
use crate::tokens::Token;

pub struct Reporter<'a, T> {
    error: ErrorKind<T>,
    input: &'a str,
}

impl<'a, T> fmt::Debug for Reporter<'a, T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let offset = match &self.error {
            ErrorKind::LexError(error) => {
                writeln!(
                    f,
                    "lex error - unexpected symbol '{}' at position {}",
                    error.symbol, error.offset
                )?;
                error.offset
            }
            ErrorKind::ParseError(error) => {
                writeln!(
                    f,
                    "parse error - unexpected token '{:?}' at position {}",
                    error.token, error.offset
                )?;
                writeln!(f, "{}", error.message)?;
                error.offset
            }
        };

        writeln!(f, "{}", self.input)?;
        for _ in 0..offset {
            write!(f, " ")?;
        }

        writeln!(f, "~")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind<T> {
    LexError(LexError),
    ParseError(ParseError<T>),
}

impl<T> ErrorKind<T> {
    pub fn report(self, input: &str) -> Reporter<T> {
        Reporter { error: self, input }
    }
}

impl<T> fmt::Display for ErrorKind<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Debug>::fmt(self, f)
    }
}

impl<T> From<LexError> for ErrorKind<T> {
    fn from(error: LexError) -> Self {
        ErrorKind::LexError(error)
    }
}

impl<T> From<ParseError<T>> for ErrorKind<T> {
    fn from(error: ParseError<T>) -> Self {
        ErrorKind::ParseError(error)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError<T> {
    token: Token<T>,
    offset: usize,
    message: String,
}

impl<T> ParseError<T> {
    pub(crate) fn new(token: Token<T>, offset: usize, message: String) -> Self {
        Self {
            token,
            offset,
            message,
        }
    }
}

impl<T> fmt::Display for ParseError<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Debug>::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LexError {
    symbol: char,
    offset: usize,
}

impl LexError {
    pub(crate) fn new(symbol: char) -> Self {
        Self { symbol, offset: 0 }
    }

    pub(crate) fn offset(self, offset: usize) -> Self {
        Self {
            symbol: self.symbol,
            offset: self.offset + offset,
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Debug>::fmt(self, f)
    }
}

impl error::Error for LexError {}

#[cfg(test)]
mod test {
    use crate::{
        error::{ErrorKind, LexError, ParseError},
        tokens::{Operator, Token},
    };

    #[derive(Debug, Clone, Copy)]
    struct Op;
    impl Operator for Op {
        fn parse(_: &str) -> Option<(&str, Self, usize)> {
            None
        }
        fn precedence(&self) -> (usize, usize) {
            (0, 0)
        }
    }

    #[test]
    fn test_reporter() {
        let input = "0 0 0";

        let lex_error: ErrorKind<Op> = ErrorKind::from(LexError {
            symbol: '0',
            offset: 3,
        });
        let parse_error: ErrorKind<Op> = ErrorKind::from(ParseError {
            token: Token::Value(0),
            offset: 5,
            message: "message goes here".to_string(),
        });

        assert_eq!(
            format!("{:?}", lex_error.report(input)),
            format!(
                "lex error - unexpected symbol '0' at position 3\n{}\n   ~\n",
                input
            )
        );
        assert_eq!(
            format!("{:?}", parse_error.report(input)),
            format!(
                "parse error - unexpected token 'Value(0)' at position 5\nmessage goes here\n{}\n     ~\n",
                input
            )
        );
    }
}
