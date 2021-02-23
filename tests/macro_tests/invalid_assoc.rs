use parser::*;

#[derive(Operator, Debug, Clone, Copy)]
enum Op {
    #[assoc("up")]
    #[ident("#")]
    Hash,
    #[assoc("right")]
    #[ident("^")]
    Caret,
    #[ident("?")]
    QMark,
}

fn main() {}
