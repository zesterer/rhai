use rhai::Engine;

#[test]
fn test_if() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("if true { 55 }"), Ok(55));
    assert_eq!(engine.eval::<i64>("if false { 55 } else { 44 }"), Ok(44));
    assert_eq!(engine.eval::<i64>("if true { 55 } else { 44 }"), Ok(55));
}
