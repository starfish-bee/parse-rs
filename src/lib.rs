pub mod error;
pub mod lexer;
pub mod tokens;
pub mod utils;

use error::{ErrorKind, ParseError};
use lexer::Lexer;
use tokens::{Operator, Token};

pub fn parse(input: &str) -> Result<Tree, ErrorKind> {
    let mut tokens = Lexer::lex(input)?;
    parse_impl(&mut tokens, 0)
}

fn parse_impl(tokens: &mut Lexer, prec: usize) -> Result<Tree, ErrorKind> {
    // unwrap ok as long as Eof always checked for
    let mut lhs = match tokens.next().unwrap() {
        (Token::Value(x), _) => Tree::Atom(x),
        (Token::Op(Operator::LeftParen), _) => {
            let lhs = parse_impl(tokens, 0)?;
            match tokens.next().unwrap() {
                (Token::Op(Operator::RightParen), _) => lhs,
                (token, i) => {
                    return Err(ErrorKind::from(ParseError::new(
                        token,
                        i,
                        "expected ')'".to_string(),
                    )))
                }
            }
        }
        (token, i) => return Err(ParseError::new(token, i, "expected value".to_owned()).into()),
    };

    loop {
        // unwrap ok as long as Eof always checked for
        let op = match tokens.peek().unwrap() {
            (Token::Eof, _) => break,
            (Token::Op(x), _) => x,
            (token, i) => {
                return Err(ParseError::new(token, i, "expected operator or eof".to_owned()).into())
            }
        };

        let (l_prec, r_prec) = op.precedence();
        if l_prec <= prec {
            break;
        }

        tokens.next();
        lhs = Tree::Expr((op, vec![lhs, parse_impl(tokens, r_prec)?]));
    }

    Ok(lhs)
}

#[derive(PartialEq, Eq)]
pub enum Tree {
    Atom(u32),
    Expr((Operator, Vec<Tree>)),
}

impl std::fmt::Debug for Tree {
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

#[test]
fn test_fmt() {
    let tree = Tree::Expr((
        Operator::Add,
        vec![
            Tree::Expr((Operator::Mul, vec![Tree::Atom(3), Tree::Atom(7)])),
            Tree::Atom(20),
        ],
    ));
    assert_eq!(format!("{:?}", tree), "Add [Mul [3, 7], 20]")
}

#[test]
fn test_parse() {
    assert_eq!(format!("{:?}", parse("1+1").unwrap()), "Add [1, 1]");
    assert_eq!(
        parse(""),
        Err(ErrorKind::ParseError(ParseError::new(
            Token::Eof,
            0,
            "expected value".to_owned()
        )))
    );
    assert_eq!(
        parse("1 1"),
        Err(ErrorKind::ParseError(ParseError::new(
            Token::Value(1),
            2,
            "expected operator or eof".to_owned()
        )))
    );
    assert_eq!(format!("{:?}", parse("1").unwrap()), "1");
    assert_eq!(
        format!("{:?}", parse("1+1+2").unwrap()),
        "Add [Add [1, 1], 2]"
    );
    assert_eq!(
        format!("{:?}", parse("1+1*2").unwrap()),
        "Add [1, Mul [1, 2]]"
    );
    assert_eq!(
        format!("{:?}", parse("1*2+1").unwrap()),
        "Add [Mul [1, 2], 1]"
    );
    assert_eq!(format!("{:?}", parse("(1)").unwrap()), "1");
    assert_eq!(
        format!("{:?}", parse("1*(2+1)").unwrap()),
        "Mul [1, Add [2, 1]]"
    );
    assert_eq!(
        parse("(1 + 1"),
        Err(ErrorKind::ParseError(ParseError::new(
            Token::Eof,
            6,
            "expected ')'".to_owned()
        )))
    );
}
