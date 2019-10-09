use rhai::Engine;

#[test]
fn test_bool_op1() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<bool>("true && (false || true)"), Ok(true));
}

#[test]
fn test_bool_op2() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<bool>("false && (false || true)"), Ok(false));
}
