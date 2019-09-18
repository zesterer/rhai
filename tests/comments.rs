use rhai::Engine;

#[test]
fn test_comments() {
    let mut engine = Engine::new();

    assert!(engine
        .eval::<i64>("let x = 5; x // I am a single line comment, yay!")
        .is_ok());

    assert!(engine
        .eval::<i64>("let /* I am a multiline comment, yay! */ x = 5; x")
        .is_ok());
}
