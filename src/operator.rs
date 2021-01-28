use crate::error::LexError;

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
    // for op => (x, y) op is left-associative if x < y, and right-associative if x >= y. Each level
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
