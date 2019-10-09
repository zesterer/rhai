use rhai::Engine;

#[test]
fn test_string() {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<String>("\"Test string: \\u2764\""),
        Ok("Test string: ❤".to_string())
    );
    assert_eq!(
        engine.eval::<String>("\"foo\" + \"bar\""),
        Ok("foobar".to_string())
    );
}
