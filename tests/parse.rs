use parser::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Caret,
    Hash,
    QMark,
}

impl Operator for Op {
    fn parse(input: &str) -> Option<(&str, Self)> {
        input
            .chars()
            .next()
            .map(|ch| {
                let op = match ch {
                    '^' => Self::Caret,
                    '#' => Self::Hash,
                    '?' => Self::QMark,
                    _ => return None,
                };
                Some((&input[1..], op))
            })
            .flatten()
    }

    fn infix_precedence(&self) -> Option<(usize, usize)> {
        match self {
            Self::Caret => Some((4, 3)),
            Self::Hash => Some((2, 1)),
            Self::QMark => Some((5, 6)),
        }
    }
}

impl Calculate for Op {
    fn apply(&self, args: &[u32]) -> u32 {
        match self {
            Self::Caret => args[0].pow(args[1]),
            Self::Hash => args[0] * args[1],
            Self::QMark => args[0] * args[1],
        }
    }
}

fn main() {
    let tree = parse::<Op>("4^(3#2)?1").unwrap();
    assert_eq!(format!("{:?}", tree), "Caret [4, QMark [Hash [3, 2], 1]]");
    assert_eq!(tree.calculate(), 4096);
}
