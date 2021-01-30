use crate::error::LexError;
use crate::utils::take_while;

// currently, valid tokens include operators (see crate::operators) and valid u32 strings only
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
    pub(crate) fn parse(input: &str) -> Result<(&str, Self, usize), LexError> {
        if input.is_empty() {
            Ok((input, Self::Eof, 0))
        } else {
            // unwrap ok as already tested if input is empty
            match input.chars().next().unwrap() {
                '(' => Ok((&input[1..], Self::LeftParen, 1)),
                ')' => Ok((&input[1..], Self::RightParen, 1)),
                x if x.is_numeric() => {
                    let (input, val, off) = take_while(input, char::is_numeric);
                    // unwrap ok as val is guaranteed to be a string of numbers
                    Ok((input, Self::Value(val.parse().unwrap()), off))
                }
                x => {
                    let (input, op, off) =
                        Operator::parse(input).ok_or_else(|| LexError::new(x))?;
                    Ok((input, Self::Op(op), off))
                }
            }
        }
    }
}

// TODO: differentiate between prefix, postfix and infix operators
pub trait Operator: Sized + Copy {
    fn parse(input: &str) -> Option<(&str, Self, usize)>;
    // defines precedence and associativity of infix operators. lower values impl lower precedence.
    // for op => (x, y) op is left-associative if x <= y, and right-associative if x > y. Each level
    // of precedence should begin with an odd number.
    // TODO: possible macro generation
    fn precedence(&self) -> (usize, usize);
}

pub trait Calculate: Operator {
    fn apply(&self, _: &[u32]) -> u32;
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
        assert_eq!(Op::parse("/123"), Some(("123", Op::Div, 1)));
    }

    #[test]
    fn test_token_parse() {
        assert_eq!(Token::<Op>::parse(""), Ok(("", Token::Eof, 0)));
        assert_eq!(Token::<Op>::parse("a"), Err(LexError::new('a')));
        assert_eq!(Token::<Op>::parse("1"), Ok(("", Token::Value(1), 1)));
        assert_eq!(
            Token::<Op>::parse("123abc4"),
            Ok(("abc4", Token::Value(123), 3))
        );
        assert_eq!(
            Token::<Op>::parse("+qwerty"),
            Ok(("qwerty", Token::Op(Op::Add), 1))
        );
    }
}
