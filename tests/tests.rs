use parser::*;

#[derive(Debug, Clone, Copy)]
enum Op {
    Caret,
    Hash,
    QMark,
}

impl Operator for Op {
    fn parse(input: &str) -> Option<(&str, Self, usize)> {
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
                Some((&input[1..], op, 1))
            })
            .flatten()
    }

    fn precedence(&self) -> (usize, usize) {
        match self {
            Self::Caret => (4, 3),
            Self::Hash => (2, 1),
            Self::QMark => (5, 6),
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

#[test]
fn test_parse() {
    assert_eq!(
        format!("{:?}", parse::<Op>("4^3^2").unwrap()),
        "Caret [4, Caret [3, 2]]"
    );
    assert_eq!(
        format!("{:?}", parse::<Op>("4#3#2").unwrap()),
        "Hash [4, Hash [3, 2]]"
    );
    assert_eq!(
        format!("{:?}", parse::<Op>("4?3?2").unwrap()),
        "QMark [QMark [4, 3], 2]"
    );
    assert_eq!(
        format!("{:?}", parse::<Op>("4^3#2?1").unwrap()),
        "Hash [Caret [4, 3], QMark [2, 1]]"
    );
    assert_eq!(
        format!("{:?}", parse::<Op>("4^(3#2)?1").unwrap()),
        "Caret [4, QMark [Hash [3, 2], 1]]"
    );
}

#[test]
fn test_calculate() {
    assert_eq!(parse::<Op>("4^3^2").unwrap().calculate(), 262144);
    assert_eq!(parse::<Op>("4#3#2").unwrap().calculate(), 24);
    assert_eq!(parse::<Op>("4?3?2").unwrap().calculate(), 24);
    assert_eq!(parse::<Op>("4^3#2?1").unwrap().calculate(), 128);
    assert_eq!(parse::<Op>("4^(3#2)?1").unwrap().calculate(), 4096);
}
