use engine::Engine;

#[test]
fn test_bool_op1() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<bool>("true && (false || true)") {
        assert_eq!(result, true);
    } else {
        assert!(false);
    }
}

#[test]
fn test_bool_op2() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<bool>("false && (false || true)") {
        assert_eq!(result, false);
    } else {
        assert!(false);
    }
}