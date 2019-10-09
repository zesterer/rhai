use rhai::Engine;

#[test]
fn test_not() {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<bool>("let not_true = !true; not_true"),
        Ok(false)
    );

    assert_eq!(engine.eval::<bool>("fn not(x) { !x } not(false)"), Ok(true));

    // TODO - do we allow stacking unary operators directly? e.g '!!!!!!!true'
    assert_eq!(engine.eval::<bool>("!(!(!(!(true))))"), Ok(true));
}
