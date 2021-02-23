//! This is a library that uses Pratt parsing to generates syntax trees from user-defined operators.
//! It also optionally allows users to define operator behaviour, providing a method to automatically
//! recurse through the tree and calculate the output.
//!
//! Currently this library only allows for infix operators, and will only allow valid `u32` values. Parenthesis
//! have fixed bahviour, causing everything contained within to be evaluated as a contained expression. Whitespace
//! Is ignored. When defining operator behaviour, they must act on a vector of `u32s`, and return a single `u32`.
//!
//! # Using parser
//! parser provides a trait, [`Operator`] , that must be implemented for any type being used as an operator.
//! Once this trait has been defined, the [`parse`] function will generate a [`Tree`] object from a `&str` input.
//! If the user additionally implements the optional [`Calculate`] trait for their type, the [`Tree`] object will
//! have access to the [`Tree::calculate`] method, which will calculate the output of the [`Tree`].
//!
//! ```
//! use parser::*;
//!
//! #[derive(Debug, Clone, Copy)]
//! enum MyOperator {
//!     Add,
//!     Sub,
//!     MysteryOperator,
//! }
//!
//! impl Operator for MyOperator {
//!     fn parse(input: &str) -> Option<(&str, Self)> {
//!         input
//!             .chars()
//!             .next()
//!             .map(|ch| {
//!                 let op = match ch {
//!                     '^' => Self::Add,
//!                     '#' => Self::Sub,
//!                     '?' => Self::MysteryOperator,
//!                     _ => return None,
//!                 };
//!                 Some((&input[1..], op))
//!             })
//!             .flatten()
//!     }
//!
//!     fn infix_precedence(&self) -> Option<(usize, usize)> {
//!         match self {
//!             Self::Add => Some((6, 5)),
//!             Self::Sub => Some((2, 1)),
//!             Self::MysteryOperator => Some((3, 4)),
//!         }
//!     }
//! }
//!
//! impl Calculate for MyOperator {
//!     fn apply(&self, args: &[u32]) -> u32 {
//!         match self {
//!             Self::Add => args[0] + args[1],
//!             Self::Sub => args[0] - args[1],
//!             Self::MysteryOperator => args[0].pow(args[0] + args[1]),
//!         }
//!     }
//! }
//!
//!
//! let input = "(5 # 8 # 4) ^ 1 ? 1";
//! let tree = parse::<MyOperator>(input).unwrap();
//! assert_eq!(format!("{:?}", tree), "MysteryOperator [Add [Sub [5, Sub [8, 4]], 1], 1]");
//! assert_eq!(tree.calculate(), 8);
//!
//! ```
//!
//! # Operator Derive Macro
//!
//! This library provides a convenience macro, [`Operator`](derive_operator::Operator), to automatically
//!  derive the [ `Operator` ] trait for a type. To access this macro, the library must be built with the
//! `derive_operator` feature enabled.
//!
#![cfg_attr(
    feature = "derive",
    doc = r#"
```
use parser::*;

#[derive(Debug, Clone, Copy, Operator)]
enum MyOperator {
    #[ident("[")]
    #[assoc("right")]
    Sub,
    #[ident("mystery!")]
    #[assoc("left")]
    MysteryOperator,
    #[ident("add")]
    #[assoc("right")]
    Add,
}

let input = "(5 [ 8 [ 4) add 1 mystery! 1";
let tree = parse::<MyOperator>(input).unwrap();
assert_eq!(format!("{:?}", tree), "MysteryOperator [Add [Sub [5, Sub [8, 4]], 1], 1]");
```
"#
)]
pub mod error;
mod lexer;
mod test_dep;
mod tokens;
mod utils;

use error::{ErrorKind, ParseError};
use lexer::Lexer;
use tokens::Token;
pub use tokens::{Calculate, Operator};

#[cfg(feature = "derive")]
/// A derive macro that implements [`Operator`] for a user-defined `Enum`.
///
/// To use this macro enable the "derive" feature.
///
/// # Using The `Operator` Derive Macro
///
/// This macro will implement [`Operator`] such that each `Enum` field is a separate operator, with the order of
/// precedence being equal to the order of fields. There two associated attributes, `ident` and `assoc`. The `ident`
/// attribute must be provided for each field, and contains the `&str` that should be parsed as that field. The
/// optional `assoc` attribute can be either "`"left"` or `"right"`, defining the associativity of the field. If
/// no `assoc` attribute is provided, the field will be left-associative by default.
///
/// This macro will use the `ident` attribute value as the string representation of each operator field
/// (see [`ParseError`](crate::error::ParseError))
///
/// # Example
/// ```
/// use parser::{parse, Operator};
///
/// #[derive(Operator, Debug, Copy, Clone)]
/// enum MyOp {
///     #[ident("+")]
///     #[assoc("left")]
///     Add,
///     #[ident("-")]
///     Sub,
///     #[ident("/")]
///     #[assoc("right")]
///     Div
/// }
///
/// let input_1 = "8 / 4 / 2";
/// let input_2 = "2 - 1 + 1 + 2";
/// assert_eq!(
///     format!("{:?}", parse::<MyOp>(input_1).unwrap()),
///     "Div [8, Div [4, 2]]"
/// );
/// assert_eq!(
///     format!("{:?}", parse::<MyOp>(input_2).unwrap()),
///     "Add [Add [Sub [2, 1], 1], 2]"
/// );
/// assert_eq!(
///     <MyOp as Operator>::to_string(&MyOp::Add),
///     "+"
/// );
/// ```
pub use derive_operator::Operator;

/// Function that takes an input `&str` and parses it as a [`Tree<T>`], where `T` is a user-defined
/// type that implements [`Operator`].
///
/// This function expects an input that is a series of valid `u32` values and operators.
/// Parenthesis may wrap any valid expression. Operators will be parsed as infix before postfix,
/// so operators with both definitions will always be treated as infix.
pub fn parse<T>(input: &str) -> Result<Tree<T>, ErrorKind>
where
    T: Operator,
{
    let mut tokens = Lexer::lex(input)?;
    parse_impl(&mut tokens, 0)
}

fn parse_impl<T>(tokens: &mut Lexer<T>, prec: usize) -> Result<Tree<T>, ErrorKind>
where
    T: Operator,
{
    // unwrap ok as Eof is always checked for
    let mut lhs = match tokens.next().unwrap() {
        (Token::Value(x), _) => Tree::Atom(x),
        (Token::LeftParen, _) => {
            let lhs = parse_impl(tokens, 0)?;
            match tokens.next().unwrap() {
                (Token::RightParen, _) => lhs,
                (token, i) => {
                    return Err(ErrorKind::from(ParseError::new(
                        token.to_string(),
                        i,
                        "expected ')'".to_string(),
                    )))
                }
            }
        }
        (Token::Op(op), i) => {
            if let Some(prec) = op.prefix_precedence() {
                let expr = parse_impl(tokens, prec)?;
                Tree::Expr((op, vec![expr]))
            } else {
                return Err(ParseError::new(
                    op.to_string(),
                    i,
                    "expected value or prefix operator".to_owned(),
                )
                .into());
            }
        }
        (token, i) => {
            return Err(ParseError::new(
                token.to_string(),
                i,
                "expected value or prefix operator".to_owned(),
            )
            .into())
        }
    };

    loop {
        // unwrap ok as Eof is always checked for
        let op = match tokens.peek().unwrap() {
            (Token::Eof, _) | (Token::RightParen, _) => break,
            (Token::Op(x), _) => x,
            (token, i) => {
                return Err(ParseError::new(
                    token.to_string(),
                    i,
                    "expected operator or eof".to_owned(),
                )
                .into())
            }
        };

        if let Some((l_prec, r_prec)) = op.infix_precedence() {
            if l_prec <= prec {
                break;
            }
            tokens.next();
            lhs = Tree::Expr((op, vec![lhs, parse_impl(tokens, r_prec)?]));
        } else if let Some(l_prec) = op.postfix_precedence() {
            if l_prec <= prec {
                break;
            }
            tokens.next();
            lhs = Tree::Expr((op, vec![lhs]));
        } else {
            panic!(format!(
                "no precedence definition for operator {}",
                op.to_string()
            ));
        }
    }

    Ok(lhs)
}

/// The syntax tree representation generated by [`parse`]. `T` represents a user-defined type the implements [`Operator`].
#[derive(PartialEq, Eq, Clone)]
pub enum Tree<T> {
    Atom(u32),
    Expr((T, Vec<Tree<T>>)),
}

impl<T> Tree<T>
where
    T: Calculate,
{
    /// This method calculates the value of the [`Tree`] by recursively calculating the value of each branch. This is only available
    /// if [`Calculate`] is implemented for `T`.
    pub fn calculate(&self) -> u32 {
        match self {
            Self::Atom(x) => *x,
            Self::Expr((op, vec)) => {
                let params: Vec<_> = vec.iter().map(|tree| tree.calculate()).collect();
                op.apply(&params)
            }
        }
    }
}

impl<T> std::fmt::Debug for Tree<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Atom(x) => write!(f, "{:?}", x)?,
            Self::Expr((op, trees)) => {
                write!(f, "{:?}", op)?;
                write!(f, " [{:?}", trees[0])?;
                for tree in &trees[1..] {
                    write!(f, ", ")?;
                    tree.fmt(f)?;
                }
                write!(f, "]")?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::{
        error::{ErrorKind, ParseError},
        test_dep::Op,
        tokens::Token,
        {parse, Tree},
    };

    #[test]
    fn test_fmt() {
        let tree = Tree::Expr((
            Op::Add,
            vec![
                Tree::Expr((Op::Mul, vec![Tree::Atom(3), Tree::Atom(7)])),
                Tree::Atom(20),
            ],
        ));
        assert_eq!(format!("{:?}", tree), "Add [Mul [3, 7], 20]")
    }

    #[test]
    fn test_parse() {
        assert_eq!(format!("{:?}", parse::<Op>("1+1").unwrap()), "Add [1, 1]");
        assert_eq!(
            parse::<Op>(""),
            Err(ErrorKind::ParseError(ParseError::new(
                Token::<Op>::Eof.to_string(),
                0,
                "expected value or prefix operator".to_owned()
            )))
        );
        assert_eq!(
            parse::<Op>("1 1"),
            Err(ErrorKind::ParseError(ParseError::new(
                Token::<Op>::Value(1).to_string(),
                2,
                "expected operator or eof".to_owned()
            )))
        );
        assert_eq!(
            parse::<Op>("-1"),
            Err(ErrorKind::ParseError(ParseError::new(
                Token::<Op>::Op(Op::Sub).to_string(),
                0,
                "expected value or prefix operator".to_owned()
            )))
        );
        assert_eq!(format!("{:?}", parse::<Op>("1").unwrap()), "1");
        assert_eq!(format!("{:?}", parse::<Op>("*1").unwrap()), "Mul [1]");
        assert_eq!(format!("{:?}", parse::<Op>("1!").unwrap()), "Fact [1]");
        assert_eq!(
            format!("{:?}", parse::<Op>("1+*1+2").unwrap()),
            "Add [Add [1, Mul [1]], 2]"
        );
        assert_eq!(
            format!("{:?}", parse::<Op>("1+1*2").unwrap()),
            "Add [1, Mul [1, 2]]"
        );
        assert_eq!(
            format!("{:?}", parse::<Op>("1*2+1").unwrap()),
            "Add [Mul [1, 2], 1]"
        );
        assert_eq!(format!("{:?}", parse::<Op>("(1)").unwrap()), "1");
        assert_eq!(
            format!("{:?}", parse::<Op>("1*(2+1)").unwrap()),
            "Mul [1, Add [2, 1]]"
        );
        assert_eq!(
            format!("{:?}", parse::<Op>("1*(2+1*(3+4))").unwrap()),
            "Mul [1, Add [2, Mul [1, Add [3, 4]]]]"
        );
        assert_eq!(
            format!("{:?}", parse::<Op>("*1*3!").unwrap()),
            "Mul [Mul [1], Fact [3]]"
        );
        let input = "(1 + 1";
        match parse::<Op>(input) {
            Ok(_) => panic!("input '{}' should not parse", input),
            Err(e) => {
                assert_eq!(
                    e,
                    ErrorKind::ParseError(ParseError::new(
                        Token::<Op>::Eof.to_string(),
                        6,
                        "expected ')'".to_owned()
                    ))
                );
                assert_eq!(
                    format!("{}", e.report(input)),
                         "parse error - unexpected token \'eof\' at position 6\nexpected \')\'\n(1 + 1\n      ~"
                );
            }
        }
    }

    #[test]
    fn test_calculate() {
        assert_eq!(parse::<Op>("1 + 1").unwrap().calculate(), 2);
        assert_eq!(parse::<Op>("3 / 1").unwrap().calculate(), 3);
        assert_eq!(parse::<Op>("2 * (4 + 6)").unwrap().calculate(), 20);
        assert_eq!(parse::<Op>("*1*3!").unwrap().calculate(), 12);
    }
}
