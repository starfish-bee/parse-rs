// A simple operator type for use in unit testing throughout the crate

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl crate::tokens::Operator for Op {
    fn parse(input: &str) -> Option<(&str, Self)> {
        // unwrap assumes input already checked for empty
        let op = match input.chars().next().unwrap() {
            '+' => Self::Add,
            '-' => Self::Sub,
            '*' => Self::Mul,
            '/' => Self::Div,
            _ => return None,
        };

        Some((&input[1..], op))
    }

    fn precedence(&self) -> (usize, usize) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Self::Add => "+".to_string(),
            Self::Sub => "-".to_string(),
            Self::Mul => "*".to_string(),
            Self::Div => "/".to_string(),
        }
    }
}

impl crate::tokens::Calculate for Op {
    fn apply(&self, params: &[u32]) -> u32 {
        match self {
            Self::Add => params[0] + params[1],
            Self::Sub => params[0] - params[1],
            Self::Mul => params[0] * params[1],
            Self::Div => params[0] / params[1],
        }
    }
}
