#[derive(Debug, PartialEq, Eq)]
struct Lexer(Vec<Token>);

impl Lexer {
    fn lex(input: &str) -> Result<Self, String> {
        let mut out = Vec::new();
        let mut i = input;
        loop {
            let (input, _) = take_while(i, char::is_whitespace);
            let (input, token) = Token::parse(input)?;
            i = input;

            if let Token::Eof = token {
                break;
            } else {
                out.push(token);
            }
        }

        Ok(Self(out))
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Token {
    Value(u32),
    Op(Operator),
    Eof,
}

impl Token {
    fn parse(input: &str) -> Result<(&str, Self), String> {
        if input.is_empty() {
            Ok((input, Self::Eof))
        } else {
            // unwrap ok as already tested if input is empty
            if input.chars().next().unwrap().is_numeric() {
                let (input, val) = take_while(input, char::is_numeric);
                // unwrap ok as val is guaranteed to be a string of numbers
                Ok((input, Self::Value(val.parse().unwrap())))
            } else {
                let (input, op) = Operator::parse(input)?;
                Ok((input, Self::Op(op)))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    LeftParen,
    RightParen,
}

impl Operator {
    fn parse(input: &str) -> Result<(&str, Self), String> {
        // unwrap assumes input already checked for empty
        let op = match input.chars().next().unwrap() {
            '+' => Self::Add,
            '-' => Self::Sub,
            '*' => Self::Mul,
            '/' => Self::Div,
            '(' => Self::LeftParen,
            ')' => Self::RightParen,
            x => return Err(format!("unrecognised symbol: {}", x)),
        };

        Ok((&input[1..], op))
    }
}

fn take_while<P>(input: &str, predicate: P) -> (&str, &str)
where
    P: Fn(char) -> bool,
{
    let index = match input.chars().position(|x| !predicate(x)) {
        Some(i) => i,
        None => input.chars().count(),
    };

    (&input[index..], &input[..index])
}

#[test]
fn test_take_while() {
    assert_eq!(take_while("abc", char::is_alphabetic), ("", "abc"));
    assert_eq!(take_while(" abc", char::is_alphabetic), (" abc", ""));
    assert_eq!(take_while(" abc", char::is_whitespace), ("abc", " "));
}

#[test]
fn test_op_parse() {
    assert_eq!(
        Operator::parse("a"),
        Err("unrecognised symbol: a".to_string())
    );
    assert_eq!(Operator::parse("/123"), Ok(("123", Operator::Div)));
}

#[test]
fn test_token_parse() {
    assert_eq!(Token::parse(""), Ok(("", Token::Eof)));
    assert_eq!(Token::parse("a"), Err("unrecognised symbol: a".to_string()));
    assert_eq!(Token::parse("1"), Ok(("", Token::Value(1))));
    assert_eq!(Token::parse("123abc4"), Ok(("abc4", Token::Value(123))));
    assert_eq!(
        Token::parse("+qwerty"),
        Ok(("qwerty", Token::Op(Operator::Add)))
    );
}

#[test]
fn test_lex() {
    use Operator::*;
    use Token::*;

    assert_eq!(Lexer::lex("1"), Ok(Lexer(vec![Value(1)])));
    assert_eq!(
        Lexer::lex("1+16"),
        Ok(Lexer(vec![Value(1), Op(Add), Value(16)]))
    );
    assert_eq!(
        Lexer::lex("1+16+a"),
        Err("unrecognised symbol: a".to_string())
    );
    assert_eq!(
        Lexer::lex("(1+16)/3*(45-5/3)"),
        Ok(Lexer(vec![
            Op(LeftParen),
            Value(1),
            Op(Add),
            Value(16),
            Op(RightParen),
            Op(Div),
            Value(3),
            Op(Mul),
            Op(LeftParen),
            Value(45),
            Op(Sub),
            Value(5),
            Op(Div),
            Value(3),
            Op(RightParen)
        ]))
    );
}
