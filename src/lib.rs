mod error;
mod lexer;
mod test_dep;
mod tokens;
mod utils;

use error::{ErrorKind, ParseError};
use lexer::Lexer;
use tokens::Token;

#[cfg(feature = "derive")]
pub use derive_operator::*;
pub use tokens::Calculate;
pub use tokens::Operator;

pub fn parse<T>(input: &str) -> Result<Tree<T>, ErrorKind<T>>
where
    T: Operator + std::fmt::Debug,
{
    let mut tokens = Lexer::lex(input)?;
    parse_impl(&mut tokens, 0)
}

fn parse_impl<T>(tokens: &mut Lexer<T>, prec: usize) -> Result<Tree<T>, ErrorKind<T>>
where
    T: Operator + std::fmt::Debug,
{
    // unwrap ok as long as Eof always checked for
    let mut lhs = match tokens.next().unwrap() {
        (Token::Value(x), _) => Tree::Atom(x),
        (Token::LeftParen, _) => {
            let lhs = parse_impl(tokens, 0)?;
            match tokens.next().unwrap() {
                (Token::RightParen, _) => lhs,
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
            (Token::Eof, _) | (Token::RightParen, _) => break,
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
pub enum Tree<T> {
    Atom(u32),
    Expr((T, Vec<Tree<T>>)),
}

impl<T> Tree<T>
where
    T: Calculate,
{
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
                Token::Eof,
                0,
                "expected value".to_owned()
            )))
        );
        assert_eq!(
            parse::<Op>("1 1"),
            Err(ErrorKind::ParseError(ParseError::new(
                Token::Value(1),
                2,
                "expected operator or eof".to_owned()
            )))
        );
        assert_eq!(format!("{:?}", parse::<Op>("1").unwrap()), "1");
        assert_eq!(
            format!("{:?}", parse::<Op>("1+1+2").unwrap()),
            "Add [Add [1, 1], 2]"
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
            parse::<Op>("(1 + 1"),
            Err(ErrorKind::ParseError(ParseError::new(
                Token::Eof,
                6,
                "expected ')'".to_owned()
            )))
        );
    }

    #[test]
    fn test_calculate() {
        assert_eq!(parse::<Op>("1 + 1").unwrap().calculate(), 2);
        assert_eq!(parse::<Op>("3 / 1").unwrap().calculate(), 3);
        assert_eq!(parse::<Op>("2 * (4 + 6)").unwrap().calculate(), 20);
    }
}
