use parser::*;

#[derive(Operator, Debug, Clone, Copy)]
enum Op {
    #[assoc("left", "right")]
    #[ident("#")]
    Hash,
    #[assoc("right")]
    #[ident("^")]
    Caret,
    #[ident("?")]
    QMark,
}

fn main() {}
