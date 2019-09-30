use rhai::Engine;

#[test]
fn test_internal_fn() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("fn addme(a, b) { a+b } addme(3, 4)") {
        assert_eq!(result, 7);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("fn bob() { return 4; 5 } bob()") {
        assert_eq!(result, 4);
    } else {
        assert!(false);
    }
}

#[test]
fn test_big_internal_fn() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>(
        "fn mathme(a, b, c, d, e, f) { a - b * c + d * e - f \
         } mathme(100, 5, 2, 9, 6, 32)",
    ) {
        assert_eq!(result, 112);
    } else {
        assert!(false);
    }
}
