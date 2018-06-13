extern crate rhai;

use rhai::Engine;

#[test]
fn test_increment() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 1; x += 2; x") {
        assert_eq!(result, 3);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<String>("let s = \"test\"; s += \"ing\"; s") {
        assert_eq!(result, "testing".to_owned());
    } else {
        assert!(false);
    }
}
