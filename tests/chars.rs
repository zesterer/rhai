use rhai::Engine;

#[test]
fn test_chars() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<char>("'y'"), Ok('y'));
    assert_eq!(engine.eval::<char>("'\\u2764'"), Ok('❤'));

    match engine.eval::<char>("''") {
        Err(_) => (),
        _ => assert!(false),
    }
}
