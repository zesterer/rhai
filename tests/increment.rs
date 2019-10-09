use rhai::Engine;

#[test]
fn test_increment() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 1; x += 2; x"), Ok(3));
    assert_eq!(
        engine.eval::<String>("let s = \"test\"; s += \"ing\"; s"),
        Ok("testing".to_string())
    );
}
