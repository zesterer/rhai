use engine::Engine;

#[test]
fn test_string() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<String>("\"Test string: \\u2764\"") {
        assert_eq!(result, "Test string: ❤");
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<String>("\"foo\" + \"bar\"") {
        assert_eq!(result, "foobar");
    } else {
        assert!(false);
    }
}
