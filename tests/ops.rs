use rhai::Engine;

#[test]
fn test_ops() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("60 + 5"), Ok(65));
    assert_eq!(engine.eval::<i64>("(1 + 2) * (6 - 4) / 2"), Ok(3));
}

#[test]
fn test_op_prec() {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<i64>("let x = 0; if x == 10 || true { x = 1} x"),
        Ok(1)
    );
}
