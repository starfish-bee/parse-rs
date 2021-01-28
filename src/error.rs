use std::{error, fmt};

// TODO: kind of a mess- think about where errors should be defined, esp. if making tokens pluggable
use crate::tokens::Token;

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
                    "parse error - unexpected token '{:?}' at position {}",
                    error.token, error.offset
                )?;
                if let Some(message) = &error.message {
                    writeln!(f, "{}", message)?;
                };
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

impl error::Error for ErrorKind {}

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

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    token: Token,
    offset: usize,
    message: Option<String>,
}

impl ParseError {
    pub(crate) fn new(token: Token, offset: usize, message: Option<String>) -> Self {
        Self {
            token,
            offset,
            message,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Debug>::fmt(self, f)
    }
}

impl error::Error for ParseError {}

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

#[test]
fn test_reporter() {
    let input = "0 0 0";

    let lex_error = ErrorKind::from(LexError {
        symbol: '0',
        offset: 3,
    });
    let parse_error = ErrorKind::from(ParseError {
        token: Token::Value(0),
        offset: 5,
        message: Some("message goes here".to_string()),
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
