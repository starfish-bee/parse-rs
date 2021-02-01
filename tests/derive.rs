use parser::*;

#[derive(Operator, Debug, Clone, Copy)]
enum Op {
    #[assoc = "right"]
    #[ident = "#"]
    Hash,
    #[assoc = "right"]
    #[ident = "^"]
    Caret,
    #[ident = "?"]
    QMark,
}

fn main() {
    let tree = parse::<Op>("4^(3#2)?1").unwrap();
    assert_eq!(format!("{:?}", tree), "Caret [4, QMark [Hash [3, 2], 1]]");
}
