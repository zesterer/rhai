extern crate rhai;

use rhai::Engine;

#[test]
fn test_decrement() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 10; x -= 7; x") {
        assert_eq!(result, 3);
    } else {
        assert!(false);
    }

    if let Ok(_) = engine.eval::<String>("let s = \"test\"; s -= \"ing\"; s") {
        assert!(false);
    } else {
        assert!(true);
    }
}
