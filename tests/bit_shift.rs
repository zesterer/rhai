use rhai::Engine;

#[test]
fn test_left_shift() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("4 << 2"), Ok(16));
}

#[test]
fn test_right_shift() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("9 >> 1"), Ok(4));
}
