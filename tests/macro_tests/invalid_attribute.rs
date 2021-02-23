use parser::*;

#[derive(Operator, Debug, Clone, Copy)]
enum Op {
    #[assoc("right")]
    #[ident("#")]
    #[weird("")]
    Hash,
    #[assoc("right")]
    #[ident("^")]
    Caret,
    #[ident("?")]
    QMark,
}

fn main() {}
