#[test]
fn tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/parser.rs");
    t.pass("tests/derive.rs");
    t.compile_fail("tests/not_enum.rs");
    t.compile_fail("tests/invalid_assoc.rs");
    t.compile_fail("tests/invalid_ident.rs");
    t.compile_fail("tests/no_ident.rs");
}
