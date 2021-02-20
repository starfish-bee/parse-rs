use std::fmt;

use crate::error::LexError;
use crate::utils::take_while;

// currently, valid tokens include operators (see Operator trait) valid u32 strings and parenthesis only
// TODO: expand range of valid values
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token<T> {
    Value(u32),
    Op(T),
    LeftParen,
    RightParen,
    Eof,
}

impl<T> Token<T>
where
    T: Operator,
{
    pub(crate) fn parse(input: &str) -> Result<(&str, Self), LexError> {
        if input.is_empty() {
            Ok((input, Self::Eof))
        } else {
            // unwrap ok as already tested if input is empty
            match input.chars().next().unwrap() {
                // slice okay as '(', ')' both 1 byte long => always valid character boundary
                '(' => Ok((&input[1..], Self::LeftParen)),
                ')' => Ok((&input[1..], Self::RightParen)),
                x if x.is_numeric() => {
                    let (input, val) = take_while(input, char::is_numeric);
                    // unwrap ok as val is guaranteed to be a string of numbers
                    Ok((input, Self::Value(val.parse().unwrap())))
                }
                x => {
                    let (input, op) = Operator::parse(input).ok_or_else(|| LexError::new(x))?;
                    Ok((input, Self::Op(op)))
                }
            }
        }
    }
}

impl<T> fmt::Display for Token<T>
where
    T: Operator,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(val) => write!(f, "{}", val),
            Self::Op(op) => write!(f, "{}", op.to_string()),
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::Eof => write!(f, "eof"),
        }
    }
}

/// Trait that defines operator parsing, associativity and precedence.
///
/// Operators are cloned during lexing, so it is recommended that they implement copy if possible.
/// # Implementing Operator
/// The [`Operator::parse`] method should accept a `&str` input and determine whether it begins with a valid operator.
/// If so, if should return `Some((A, B))`, where:
/// - `A` is the remaining input after the parsed operator
/// - `B` is the parsed operator
///
/// If the input does not begin with a valid operator, it should return `None`. The input will never be empty.
///
/// At least one precedence method should be implemented. These define operator precedence and (for infix operators)
/// associativity. All methods return an [`Option`], where `None` indicates the operator is not of the appropriate class
/// (e.g. an operator returning `None` for [`Operator::infix_precedence`] indicates it is not an infix operator). If
/// an operator has both a postfix and infix definition, it will always be parsed as an infix operator.
///
/// The [`Operator::infix_precedence`] method takes `&self` and returns `(A, B)`. `A` and `B` define the associativity and
/// precedence of the operator, where `A <= B` implies left-associativity, and `A > B` implies right-associativity.
/// The lower the values, the lower the precedence of the operator. The following rules should
/// be adhered to when implementing this function:
/// - For each operator, the lower value should be odd.
/// - For each operator, the higher value should be 1 higher than the lower value.
/// - No two operators should return overlapping values from this function, except in the case that they have
/// identical precendence and associativity.
///
/// If these rules are not followed no guarantee is made of correct or sensible behaviour.
///
/// [`Operator::prefix_precedence`] and [`Operator::postfix_precedence`] both take `&self` and return `A`, where `A` defines
/// the precedence of the operator. The lower the value, the lower the precedence of the operator. The return value should be odd,
/// as with [`Operator::infix_precedence`].
///
/// Optionally, you may also provide an implementation of [`Operator::to_string`]. This method defines how the operator
/// is represented in [`ParseError`](crate::error::ParseError) in the event that an operator is the source of a parsing error.
/// The default method always returns `"custom operator"`.
///
/// # Example Implementation
/// This example shows a simple implementation that parses `[` and `]` as operators, where `[` has lower precendence and is
/// left-associative, and `]` has higher precedence and is right-associative.
/// ```
/// use parser::Operator;
///
/// // A is infix, B is prefix or infix, C is postfix
/// #[derive(Clone, Copy, Debug)]
/// enum MyOperator {
///     A,
///     B,
///     C,
/// }
///
/// impl Operator for MyOperator {
///     fn parse(input: &str) -> Option<(&str, Self)> {
///         let op = match input.chars().next() {
///             Some('[') => Some(Self::A),
///             Some(']') => Some(Self::B),
///             Some('x') => Some(Self::C),
///             _ => None,
///         };
///         op.map(|op| (&input[1..], op))
///     }
///
///     fn infix_precedence(&self) -> Option<(usize, usize)> {
///         match self {
///             Self::A => Some((1, 2)),
///             Self::B => Some((4, 3)),
///             _ => None,
///         }
///     }
///
///     fn prefix_precedence(&self) -> Option<usize> {
///         match self {
///             Self::B => Some(5),
///             _ => None,
///         }
///     }
///
///     fn postfix_precedence(&self) -> Option<usize> {
///         match self {
///             Self::C => Some(7),
///             _ => None,
///         }
///     }
///
///     fn to_string(&self) -> String {
///         match self {
///             Self::A => "[".to_string(),
///             Self::B => "]".to_string(),
///             Self::C => "x".to_string(),
///         }
///     }
/// }
/// ```

pub trait Operator: Sized + Clone {
    fn parse(input: &str) -> Option<(&str, Self)>;
    // defines precedence and associativity of infix operators. lower values impl lower precedence.
    // for op => (x, y) op is left-associative if x <= y, and right-associative if x > y. Each level
    // of precedence should begin with an odd number.
    fn infix_precedence(&self) -> Option<(usize, usize)> {
        None
    }
    // defines precedence of prefix operators. lower values impl lower precedence.
    // Each level of precedence should begin with an odd number.
    fn prefix_precedence(&self) -> Option<usize> {
        None
    }
    // defines precedence of postfix operators. lower values impl lower precedence.
    // Each level of precedence should begin with an odd number.
    fn postfix_precedence(&self) -> Option<usize> {
        None
    }
    // method used by parser::parse to fill the token field in error::ParseError when parsing errors occur
    fn to_string(&self) -> String {
        "custom operator".to_string()
    }
}

/// Optional trait to define operator behaviour and allow access to the [`calculate`](crate::Tree::calculate) method of [`Tree`](crate::Tree).
///
/// # Implementing Calculate
/// In [`Calculate::apply`], operator arguments are passed in as a slice of `u32`s. If the operator is parsed as infix or postfix, the slice
/// will always be length 1. For infix operators, it will be length 2.
/// ```
/// use parser::Calculate;
///
/// enum MyOperator {
///     A,
///     B,
/// }
///
/// impl Calculate for MyOperator {
///     fn apply(&self, args: &[u32]) -> u32 {
///         match self {
///             Self::A => args[0] + args[1],
///             Self::B => {
///                 if args.len() == 1 {
///                     args[0]
///                 } else {
///                     args[0] * args[0].pow(args[1])
///                 }
///             },
///         }
///     }
/// }
/// ```

pub trait Calculate {
    fn apply(&self, args: &[u32]) -> u32;
}

#[cfg(test)]
mod test {
    use crate::{
        error::LexError,
        test_dep::Op,
        tokens::{Operator, Token},
    };

    #[test]
    fn test_op_parse() {
        assert_eq!(Op::parse("a"), None);
        assert_eq!(Op::parse("/123"), Some(("123", Op::Div)));
    }

    #[test]
    fn test_token_parse() {
        assert_eq!(Token::<Op>::parse(""), Ok(("", Token::Eof)));
        assert_eq!(Token::<Op>::parse("a"), Err(LexError::new('a')));
        assert_eq!(Token::<Op>::parse("1"), Ok(("", Token::Value(1))));
        assert_eq!(
            Token::<Op>::parse("123abc4"),
            Ok(("abc4", Token::Value(123)))
        );
        assert_eq!(
            Token::<Op>::parse("+qwerty"),
            Ok(("qwerty", Token::Op(Op::Add)))
        );
    }
}
