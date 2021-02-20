// A simple operator type for use in unit testing throughout the crate

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Fact,
}

impl crate::tokens::Operator for Op {
    fn parse(input: &str) -> Option<(&str, Self)> {
        // unwrap okay as this will never be passed an empty input
        let op = match input.chars().next().unwrap() {
            '+' => Self::Add,
            '-' => Self::Sub,
            '*' => Self::Mul,
            '/' => Self::Div,
            '!' => Self::Fact,
            _ => return None,
        };

        Some((&input[1..], op))
    }

    fn infix_precedence(&self) -> Option<(usize, usize)> {
        match self {
            Self::Add | Self::Sub => Some((1, 2)),
            Self::Mul | Self::Div => Some((3, 4)),
            _ => None,
        }
    }

    fn prefix_precedence(&self) -> Option<usize> {
        if let Self::Mul = self {
            Some(5)
        } else {
            None
        }
    }

    fn postfix_precedence(&self) -> Option<usize> {
        if let Self::Fact = self {
            Some(7)
        } else {
            None
        }
    }

    fn to_string(&self) -> String {
        match self {
            Self::Add => "+".to_string(),
            Self::Sub => "-".to_string(),
            Self::Mul => "*".to_string(),
            Self::Div => "/".to_string(),
            Self::Fact => "!".to_string(),
        }
    }
}

impl crate::tokens::Calculate for Op {
    fn apply(&self, params: &[u32]) -> u32 {
        match self {
            Self::Add => params[0] + params[1],
            Self::Sub => params[0] - params[1],
            Self::Mul => {
                if params.len() == 1 {
                    2 * params[0]
                } else {
                    params[0] * params[1]
                }
            }
            Self::Div => params[0] / params[1],
            Self::Fact => {
                if params[0] > 1 {
                    params[0] * (params[0] - 1)
                } else {
                    1
                }
            }
        }
    }
}
