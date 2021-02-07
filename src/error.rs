use std::{error, fmt};

pub struct Reporter<'a> {
    error: ErrorKind,
    input: &'a str,
}

impl<'a> fmt::Debug for Reporter<'a> {
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
                    "parse error - unexpected token '{}' at position {}",
                    error.token, error.offset
                )?;
                writeln!(f, "{}", error.context)?;
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorKind {
    LexError(LexError),
    ParseError(ParseError),
}

impl ErrorKind {
    pub fn report(self, input: &str) -> Reporter {
        Reporter { error: self, input }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Debug>::fmt(self, f)
    }
}

impl From<LexError> for ErrorKind {
    fn from(error: LexError) -> Self {
        ErrorKind::LexError(error)
    }
}

impl From<ParseError> for ErrorKind {
    fn from(error: ParseError) -> Self {
        ErrorKind::ParseError(error)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParseError {
    token: String,
    offset: usize,
    context: String,
}

impl ParseError {
    pub(crate) fn new(token: String, offset: usize, context: String) -> Self {
        Self {
            token,
            offset,
            context,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Debug>::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

        let lex_error: ErrorKind = ErrorKind::from(LexError {
            symbol: '0',
            offset: 3,
        });
        let parse_error: ErrorKind = ErrorKind::from(ParseError {
            token: Token::<Op>::Value(0).to_string(),
            offset: 5,
            context: "message goes here".to_string(),
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
                "parse error - unexpected token '0' at position 5\nmessage goes here\n{}\n     ~\n",
                input
            )
        );
    }
}
