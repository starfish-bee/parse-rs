use parser::*;

#[derive(Operator, Debug, Clone, Copy)]
struct Op {
    #[assoc("right")]
    #[ident("#")]
    hash: char,
    #[assoc("right")]
    #[ident("^")]
    caret: char,
    #[ident("?")]
    qmark: char,
}

fn main() {}
