use rhai::Engine;

#[test]
fn test_ops() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("60 + 5") {
        assert_eq!(result, 65);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("(1 + 2) * (6 - 4) / 2") {
        assert_eq!(result, 3);
    } else {
        assert!(false);
    }
}

#[test]
fn test_op_prec() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 0; if x == 10 || true { x = 1} x") {
        assert_eq!(result, 1);
    } else {
        assert!(false);
    }
}
