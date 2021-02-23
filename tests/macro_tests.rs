#[test]
#[cfg_attr(not(feature = "derive_operator"), ignore)]
fn macro_tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/macro_tests/derive.rs");
    t.compile_fail("tests/macro_tests/not_enum.rs");
    t.compile_fail("tests/macro_tests/invalid_assoc.rs");
    t.compile_fail("tests/macro_tests/invalid_ident.rs");
    t.compile_fail("tests/macro_tests/no_ident.rs");
    t.compile_fail("tests/macro_tests/invalid_attribute.rs");
    t.compile_fail("tests/macro_tests/left_right.rs");
}
