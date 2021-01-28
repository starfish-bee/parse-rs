use crate::error::LexError;
use crate::operator::Operator;

// stores both the tokens and their positions for better error reporting
#[derive(Debug, PartialEq, Eq)]
pub struct Lexer {
    pub tokens: Vec<(Token, usize)>,
}

impl Lexer {
    pub(crate) fn lex(input: &str) -> Result<Self, LexError> {
        let mut tokens = Vec::new();
        let mut i = input;
        let mut offset = 0;
        loop {
            // whitespace is ignored but the offset is retained for error reporting. i_temp is used
            // as i must be retained between loops, and let binding would shadow it and cause it to be dropped
            // after each iteration
            let (i_temp, _, off) = take_while(i, char::is_whitespace);
            offset += off;
            let (i_temp, token, off) =
                Token::parse(i_temp).map_err(|e| LexError::offset(e, offset))?;

            if let Token::Eof = token {
                tokens.push((token, offset));
                break;
            } else {
                tokens.push((token, offset));
            }

            offset += off;
            i = i_temp;
        }

        // the tokens are stored in a reverse state to allow easy iteration using 'pop' method
        tokens.reverse();

        Ok(Self { tokens })
    }

    pub(crate) fn peek(&self) -> Option<(Token, usize)> {
        self.tokens.last().copied()
    }
}

impl Iterator for Lexer {
    type Item = (Token, usize);

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.pop()
    }
}

// currently, valid tokens include operators (see crate::operators) and valid u32 strings only
// TODO: expand range of valid values
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

// helper function for consuming whitespace and multi-character tokens
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

#[test]
fn test_lex() {
    use Operator::*;
    use Token::*;

    assert_eq!(
        Lexer::lex("1"),
        Ok(Lexer {
            tokens: vec![(Eof, 1), (Value(1), 0)]
        })
    );
    assert_eq!(
        Lexer::lex("1 + 16"),
        Ok(Lexer {
            tokens: vec![(Eof, 6), (Value(16), 4), (Op(Add), 2), (Value(1), 0)]
        })
    );
    assert_eq!(Lexer::lex("1+16+a"), Err(LexError::new('a').offset(5)));
    assert_eq!(
        Lexer::lex("(1+16)/3*(450-5/   3)"),
        Ok(Lexer {
            tokens: vec![
                (Eof, 21),
                (Op(RightParen), 20),
                (Value(3), 19),
                (Op(Div), 15),
                (Value(5), 14),
                (Op(Sub), 13),
                (Value(450), 10),
                (Op(LeftParen), 9),
                (Op(Mul), 8),
                (Value(3), 7),
                (Op(Div), 6),
                (Op(RightParen), 5),
                (Value(16), 3),
                (Op(Add), 2),
                (Value(1), 1),
                (Op(LeftParen), 0),
            ]
        })
    );
}

#[test]
fn test_lex_iter() {
    use Operator::*;
    use Token::*;

    let mut lex = Lexer::lex("1+16").unwrap();

    assert_eq!(lex.next(), Some((Value(1), 0)));
    assert_eq!(lex.next(), Some((Op(Add), 1)));
    assert_eq!(lex.next(), Some((Value(16), 2)));
    assert_eq!(lex.next(), Some((Eof, 4)));
    assert_eq!(lex.next(), None);
}
