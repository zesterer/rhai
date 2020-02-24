use rhai::Engine;
use rhai::EvalAltResult;

#[test]
fn test_decrement() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 10; x -= 7; x"), Ok(3));

    assert_eq!(
        engine.eval::<String>("let s = \"test\"; s -= \"ing\"; s"),
        Err(EvalAltResult::ErrorFunctionNotFound(
            "- (alloc::string::String, alloc::string::String)".to_string()
        ))
    );
}
