// helper function for consuming whitespace and multi-character tokens
pub fn take_while<P>(input: &str, predicate: P) -> (&str, &str)
where
    P: Fn(char) -> bool,
{
    input.chars().for_each(|x| println!("{}", x));
    let index = match input.char_indices().find(|&(_, x)| !predicate(x)) {
        Some((i, _)) => i,
        None => input.len(),
    };
    println!("{}", index);

    // slice okay as index is guaranteed to be a valid character boundary
    (&input[index..], &input[..index])
}

#[test]
fn test_take_while() {
    assert_eq!(take_while("ab老 ", char::is_alphabetic), (" ", "ab老"));
    assert_eq!(take_while("ab老", char::is_alphabetic), ("", "ab老"));
    assert_eq!(take_while(" abc", char::is_alphabetic), (" abc", ""));
    assert_eq!(take_while(" abc", char::is_whitespace), ("abc", " "));
}
