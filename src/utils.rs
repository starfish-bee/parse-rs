// helper function for consuming whitespace and multi-character tokens
pub fn take_while<P>(input: &str, predicate: P) -> (&str, &str, usize)
where
    P: Fn(char) -> bool,
{
    let index = match input.chars().position(|x| !predicate(x)) {
        Some(i) => i,
        None => input.chars().count(),
    };

    (&input[index..], &input[..index], index)
}

#[test]
fn test_take_while() {
    assert_eq!(take_while("abc", char::is_alphabetic), ("", "abc", 3));
    assert_eq!(take_while(" abc", char::is_alphabetic), (" abc", "", 0));
    assert_eq!(take_while(" abc", char::is_whitespace), ("abc", " ", 1));
}
