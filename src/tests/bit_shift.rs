use engine::Engine;

#[test]
fn test_left_shift() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("4 << 2") {
        assert_eq!(result, 16);
    } else {
        assert!(false);
    }
}

#[test]
fn test_right_shift() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("9 >> 1") {
        assert_eq!(result, 4);
    } else {
        assert!(false);
    }
}
