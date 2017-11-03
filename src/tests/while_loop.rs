use engine::Engine;

#[test]
fn test_while() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 0; while x < 10 { x = x + 1; if x > 5 { \
                                            break } } x") {
        assert_eq!(result, 6);
    } else {
        assert!(false);
    }
}
