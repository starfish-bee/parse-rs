use parser::*;

#[derive(Operator, Debug, Clone, Copy)]
enum Op {
    #[assoc = "right"]
    #[ident = "#"]
    Hash,
    #[assoc = "right"]
    #[ident = "^"]
    Caret,
    QMark,
}

fn main() {}
