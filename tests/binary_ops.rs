use rhai::Engine;

#[test]
fn test_binary_ops() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("10 % 4"), Ok(2));
    assert_eq!(engine.eval::<i64>("10 << 4"), Ok(160));
    assert_eq!(engine.eval::<i64>("10 >> 4"), Ok(0));
    assert_eq!(engine.eval::<i64>("10 & 4"), Ok(0));
    assert_eq!(engine.eval::<i64>("10 | 4"), Ok(14));
    assert_eq!(engine.eval::<i64>("10 ^ 4"), Ok(14));
}
