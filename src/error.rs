//! Contains [`ErrorKind`], the main error type returned by [`parse`](crate::parse), as
//! well [`Reporter`], a helper type for nicer error reporting.

use std::{error, fmt};

/// A helper type for nicer error formatting. Created by calling [`ErrorKind::report`] or
/// [`ErrorKind::into_report`].
///
/// Displays the position of the token causing the error within the input.
///
/// # Example
/// ```
/// use parser::{Operator, parse};
/// #[derive(Clone, Copy)]
/// struct MyOp;
///
/// impl Operator for MyOp {
///     // MyOp has no valid input
///     fn parse(_: &str) -> Option<(&str, Self)> {
///         None
///     }
///     // Function will never be called as MyOp is never parsed
///     fn precedence(&self) -> (usize, usize) {
///         (1, 2)
///     }
/// }
/// // Without being defined as an operator, '+' is not a valid character
/// let input = "1 + 2";
/// let report = "\
/// lex error - unexpected symbol '+' at position 2
/// 1 + 2
///   ~";
/// match parse::<MyOp>(input) {
///     Ok(_) => unreachable!(),
///     Err(e) => {
///         assert_eq!(
///             format!("{}", e.into_report(&input)),
///             report
///         )
///     }
/// }
/// ```

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Reporter<'a> {
    pub error: ErrorKind,
    pub input: &'a str,
}

// Reporter displays in the following way:
// line 1: a message explaining the error source and position in input
// line 2: a message giving additional context, if parsing error
// line 3: the original input
// line 4: a tilde '~' highlighting the position of the error
// e.g.
// parse error - unexpected token `+` at position 4
// expected value
// 1 + + 3
//     ~
impl<'a> fmt::Display for Reporter<'a> {
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

        write!(f, "~")
    }
}

impl<'a> error::Error for Reporter<'a> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        self.error.source()
    }
}

/// The error returned by [`parse`](crate::parse), representing a parsing or lexing error.
///
/// Can be converted to a [`Reporter`] for nicer error formatting by using [`ErrorKind::report`],
/// which clones the `ErrorKing`, or [`ErrorKind::into_report`], which consumes it.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorKind {
    LexError(LexError),
    ParseError(ParseError),
}

impl ErrorKind {
    pub fn report<'a>(&self, input: &'a str) -> Reporter<'a> {
        Reporter {
            error: self.clone(),
            input,
        }
    }

    pub fn into_report(self, input: &str) -> Reporter {
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

impl error::Error for ErrorKind {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(match self {
            Self::LexError(ref error) => error,
            Self::ParseError(ref error) => error,
        })
    }
}

/// An error occuring during the parsing phase.
///
/// The token source of the error is represented here as a string. If the token is a custom
/// operator, the [`to_string`](crate::Operator::to_string) method of the [`Operator`](crate::Operator)
/// trait is used to generate this representation.
///
/// The error also provides the byte offset in the source `&str` of the first character of the token,
/// as well as additional context.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParseError {
    pub token: String,
    pub offset: usize,
    pub context: String,
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

impl error::Error for ParseError {}

/// An error occuring during the lexing phase.
///
/// Contains the symbol source of the error, as well as the byte offset of that symbol in the input `&str`.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct LexError {
    pub symbol: char,
    pub offset: usize,
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
        test_dep::Op,
        tokens::Token,
    };

    #[test]
    fn test_reporter() {
        let input = "0 0 0";
        let input_2 = "0 + +";

        let lex_error: ErrorKind = ErrorKind::from(LexError {
            symbol: '0',
            offset: 3,
        });
        let parse_error: ErrorKind = ErrorKind::from(ParseError {
            token: Token::<Op>::Value(0).to_string(),
            offset: 5,
            context: "message goes here".to_string(),
        });
        let op_error: ErrorKind = ErrorKind::from(ParseError {
            token: Token::<Op>::Op(Op::Add).to_string(),
            offset: 5,
            context: "message goes here".to_string(),
        });

        assert_eq!(
            format!("{}", lex_error.report(input)),
            format!(
                "lex error - unexpected symbol '0' at position 3\n{}\n   ~",
                input
            )
        );
        assert_eq!(
            format!("{}", parse_error.report(input)),
            format!(
                "parse error - unexpected token '0' at position 5\nmessage goes here\n{}\n     ~",
                input
            )
        );
        assert_eq!(
            format!("{}", op_error.report(input_2)),
            format!(
                "parse error - unexpected token '+' at position 5\nmessage goes here\n{}\n     ~",
                input_2
            )
        );
    }
}
