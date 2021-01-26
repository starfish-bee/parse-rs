pub mod lexer;

use lexer::{LexError, Lexer, Operator, Token};

pub fn parse(input: &str) -> Result<Tree, ErrorKind> {
    let mut tokens = Lexer::lex(input)?;
    parse_impl(&mut tokens, 0)
}

fn parse_impl(tokens: &mut Lexer, prec: usize) -> Result<Tree, ErrorKind> {
    // unwrap ok as long as Eof always checked for
    println!("{:?}", tokens);
    let mut lhs = match tokens.next().unwrap() {
        (Token::Value(x), _) => Tree::Atom(x),
        (token, i) => {
            return Err(ParseError::new(token, i, Some("Token::Value(u32)".to_owned())).into())
        }
    };

    loop {
        // unwrap ok as long as Eof always checked for
        let op = match tokens.peek().unwrap() {
            (Token::Eof, _) => break,
            (Token::Op(x), _) => x,
            (token, i) => {
                return Err(ParseError::new(
                    token,
                    i,
                    Some("Token::Op(Operator) or Token::Eof".to_owned()),
                )
                .into())
            }
        };

        let (l_prec, r_prec) = op.precedence();
        if l_prec < prec {
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

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    LexError(LexError),
    ParseError(ParseError),
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

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    token: Token,
    offset: usize,
    expected: Option<String>,
}

impl ParseError {
    fn new(token: Token, offset: usize, expected: Option<String>) -> Self {
        Self {
            token,
            offset,
            expected,
        }
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
            Some("Token::Value(u32)".to_owned())
        )))
    );
    assert_eq!(
        parse("1 1"),
        Err(ErrorKind::ParseError(ParseError::new(
            Token::Value(1),
            2,
            Some("Token::Op(Operator) or Token::Eof".to_owned())
        )))
    );
    assert_eq!(format!("{:?}", parse("1").unwrap()), "1");
    assert_eq!(
        format!("{:?}", parse("1+1*2").unwrap()),
        "Add [1, Mul [1, 2]]"
    );
    assert_eq!(
        format!("{:?}", parse("1*2+1").unwrap()),
        "Add [Mul [1, 2], 1]"
    );
}
