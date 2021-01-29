use crate::error::LexError;
use crate::tokens::{Operator, Token};
use crate::utils::take_while;

// stores both the tokens and their positions for better error reporting
#[derive(Debug, PartialEq, Eq)]
pub struct Lexer<T> {
    pub tokens: Vec<(Token<T>, usize)>,
}

impl<T> Lexer<T>
where
    T: Operator,
{
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

    pub(crate) fn peek(&self) -> Option<(Token<T>, usize)> {
        self.tokens.last().copied()
    }
}

impl<T> Iterator for Lexer<T> {
    type Item = (Token<T>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.pop()
    }
}

#[cfg(test)]
mod test {
    use super::Lexer;
    use crate::error::LexError;
    use crate::tokens::Token;

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum Operator {
        Add,
        Sub,
        Mul,
        Div,
    }

    impl crate::tokens::Operator for Operator {
        fn parse(input: &str) -> Option<(&str, Self, usize)> {
            // unwrap assumes input already checked for empty
            let op = match input.chars().next().unwrap() {
                '+' => Self::Add,
                '-' => Self::Sub,
                '*' => Self::Mul,
                '/' => Self::Div,
                _ => return None,
            };

            Some((&input[1..], op, 1))
        }

        fn precedence(&self) -> (usize, usize) {
            match self {
                Self::Add | Self::Sub => (1, 2),
                Self::Mul | Self::Div => (3, 4),
            }
        }
    }

    use Operator::*;

    #[test]
    fn test_lex() {
        use Token::*;

        assert_eq!(
            Lexer::<Operator>::lex("1"),
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
        assert_eq!(
            Lexer::<Operator>::lex("1+16+a"),
            Err(LexError::new('a').offset(5))
        );
        assert_eq!(
            Lexer::<Operator>::lex("(1+16)/3*(450-5/   3)"),
            Ok(Lexer::<Operator> {
                tokens: vec![
                    (Eof, 21),
                    (RightParen, 20),
                    (Value(3), 19),
                    (Op(Div), 15),
                    (Value(5), 14),
                    (Op(Sub), 13),
                    (Value(450), 10),
                    (LeftParen, 9),
                    (Op(Mul), 8),
                    (Value(3), 7),
                    (Op(Div), 6),
                    (RightParen, 5),
                    (Value(16), 3),
                    (Op(Add), 2),
                    (Value(1), 1),
                    (LeftParen, 0),
                ]
            })
        );
    }

    #[test]
    fn test_lex_iter() {
        use Token::*;

        let mut lex = Lexer::lex("1+16").unwrap();

        assert_eq!(lex.next(), Some((Value(1), 0)));
        assert_eq!(lex.next(), Some((Op(Add), 1)));
        assert_eq!(lex.next(), Some((Value(16), 2)));
        assert_eq!(lex.next(), Some((Eof, 4)));
        assert_eq!(lex.next(), None);
    }
}
