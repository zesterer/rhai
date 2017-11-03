use engine::Engine;

#[test]
fn test_chars() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<char>("'y'") {
        assert_eq!(result, 'y');
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<char>("'\\u2764'") {
        assert_eq!(result, '❤');
    } else {
        assert!(false);
    }

    match engine.eval::<char>("''") {
        Err(_) => (),
        _ => assert!(false),
    }
}