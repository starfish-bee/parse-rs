use crate::error::LexError;
use crate::utils::take_while;

// currently, valid tokens include operators (see crate::operators) and valid u32 strings only
// TODO: expand range of valid values
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token {
    Value(u32),
    Op(Operator),
    Eof,
}

impl Token {
    pub(crate) fn parse(input: &str) -> Result<(&str, Self, usize), LexError> {
        if input.is_empty() {
            Ok((input, Self::Eof, 0))
        } else {
            // unwrap ok as already tested if input is empty
            if input.chars().next().unwrap().is_numeric() {
                let (input, val, off) = take_while(input, char::is_numeric);
                // unwrap ok as val is guaranteed to be a string of numbers
                Ok((input, Self::Value(val.parse().unwrap()), off))
            } else {
                let (input, op, off) = Operator::parse(input)?;
                Ok((input, Self::Op(op), off))
            }
        }
    }
}

// TODO: differentiate between prefix, postfix and infix operators
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    LeftParen,
    RightParen,
}

impl Operator {
    pub(crate) fn parse(input: &str) -> Result<(&str, Self, usize), LexError> {
        // unwrap assumes input already checked for empty
        let op = match input.chars().next().unwrap() {
            '+' => Self::Add,
            '-' => Self::Sub,
            '*' => Self::Mul,
            '/' => Self::Div,
            '(' => Self::LeftParen,
            ')' => Self::RightParen,
            x => return Err(LexError::new(x)),
        };

        Ok((&input[1..], op, 1))
    }

    // defines precedence and associativity of infix operators. lower values impl lower precedence.
    // for op => (x, y) op is left-associative if x <= y, and right-associative if x > y. Each level
    // of precedence should begin with an odd number.
    // TODO: possible macro generation
    pub(crate) fn precedence(&self) -> (usize, usize) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
            _ => (0, 0),
        }
    }
}

#[test]
fn test_op_parse() {
    assert_eq!(Operator::parse("a"), Err(LexError::new('a')));
    assert_eq!(Operator::parse("/123"), Ok(("123", Operator::Div, 1)));
}

#[test]
fn test_token_parse() {
    assert_eq!(Token::parse(""), Ok(("", Token::Eof, 0)));
    assert_eq!(Token::parse("a"), Err(LexError::new('a')));
    assert_eq!(Token::parse("1"), Ok(("", Token::Value(1), 1)));
    assert_eq!(Token::parse("123abc4"), Ok(("abc4", Token::Value(123), 3)));
    assert_eq!(
        Token::parse("+qwerty"),
        Ok(("qwerty", Token::Op(Operator::Add), 1))
    );
}
