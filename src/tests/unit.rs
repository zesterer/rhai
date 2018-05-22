use engine::Engine;

#[test]
fn test_unit() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<()>("let x = (); x") {
        assert_eq!(result, ());
    } else {
        assert!(false);
    }
}

#[test]
fn test_unit_eq() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<bool>("let x = (); let y = (); x == y") {
        assert!(result);
    } else {
        assert!(false);
    }
}

#[test]
fn test_unit_with_spaces() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<()>("let x = ( ); x") {
        assert_eq!(result, ());
    } else {
        assert!(false);
    }
}