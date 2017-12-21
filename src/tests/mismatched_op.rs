use engine::{Engine, EvalAltResult};

#[test]
fn test_mismatched_op() {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<i64>("60 + \"hello\""),
        Err(EvalAltResult::ErrorFunctionNotFound)
    );
}
