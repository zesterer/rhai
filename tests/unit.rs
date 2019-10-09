use rhai::Engine;

#[test]
fn test_unit() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<()>("let x = (); x"), Ok(()));
}

#[test]
fn test_unit_eq() {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<bool>("let x = (); let y = (); x == y"),
        Ok(true)
    );
}

#[test]
fn test_unit_with_spaces() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<()>("let x = ( ); x"), Ok(()));
}
