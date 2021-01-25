pub fn lex(input: &str) -> Result<Vec<(Token, usize)>, LexError> {
    let mut out = Vec::new();
    let mut i = input;
    let mut offset = 0;
    loop {
        let (i_temp, _, off) = take_while(i, char::is_whitespace);
        offset += off;
        let (i_temp, token, off) = Token::parse(i_temp).map_err(|e| LexError::offset(e, offset))?;

        if let Token::Eof = token {
            break;
        } else {
            out.push((token, offset));
        }

        offset += off;
        i = i_temp;
    }

    Ok(out)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Value(u32),
    Op(Operator),
    Eof,
}

impl Token {
    fn parse(input: &str) -> Result<(&str, Self, usize), LexError> {
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

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    LeftParen,
    RightParen,
}

impl Operator {
    fn parse(input: &str) -> Result<(&str, Self, usize), LexError> {
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
}

#[derive(Debug, PartialEq, Eq)]
pub struct LexError {
    symbol: char,
    offset: usize,
}

impl LexError {
    fn new(symbol: char) -> Self {
        Self { symbol, offset: 0 }
    }

    fn offset(self, offset: usize) -> Self {
        Self {
            symbol: self.symbol,
            offset: self.offset + offset,
        }
    }
}

fn take_while<P>(input: &str, predicate: P) -> (&str, &str, usize)
where
    P: Fn(char) -> bool,
{
    let index = match input.chars().position(|x| !predicate(x)) {
        Some(i) => i,
        None => input.chars().count(),
    };

    (&input[index..], &input[..index], index)
}

#[test]
fn test_take_while() {
    assert_eq!(take_while("abc", char::is_alphabetic), ("", "abc", 3));
    assert_eq!(take_while(" abc", char::is_alphabetic), (" abc", "", 0));
    assert_eq!(take_while(" abc", char::is_whitespace), ("abc", " ", 1));
}

#[test]
fn test_op_parse() {
    assert_eq!(Operator::parse("a"), Err(LexError::new('a')));
    assert_eq!(Operator::parse("/123"), Ok(("123", Operator::Div, 1)));
}

#[test]
fn test_token_parse() {
    assert_eq!(1, 0);
    assert_eq!(Token::parse(""), Ok(("", Token::Eof, 0)));
    assert_eq!(Token::parse("a"), Err(LexError::new('a')));
    assert_eq!(Token::parse("1"), Ok(("", Token::Value(1), 1)));
    assert_eq!(Token::parse("123abc4"), Ok(("abc4", Token::Value(123), 3)));
    assert_eq!(
        Token::parse("+qwerty"),
        Ok(("qwerty", Token::Op(Operator::Add), 1))
    );
}

#[test]
fn test_lex() {
    use Operator::*;
    use Token::*;

    assert_eq!(lex("1"), Ok(vec![(Value(1), 0)]));
    assert_eq!(
        lex("1+16"),
        Ok(vec![(Value(1), 0), (Op(Add), 1), (Value(16), 2)])
    );
    assert_eq!(lex("1+16+a"), Err(LexError::new('a').offset(5)));
    assert_eq!(
        lex("(1+16)/3*(450-5/3)"),
        Ok(vec![
            (Op(LeftParen), 0),
            (Value(1), 1),
            (Op(Add), 2),
            (Value(16), 3),
            (Op(RightParen), 5),
            (Op(Div), 6),
            (Value(3), 7),
            (Op(Mul), 8),
            (Op(LeftParen), 9),
            (Value(450), 10),
            (Op(Sub), 13),
            (Value(5), 14),
            (Op(Div), 15),
            (Value(3), 16),
            (Op(RightParen), 17)
        ])
    );
}
