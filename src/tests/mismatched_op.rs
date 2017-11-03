use engine::{Engine, EvalAltResult};

#[test]
fn test_mismatched_op() {
    let mut engine = Engine::new();

    match engine.eval::<i64>("60 + \"hello\"") {
        Err(EvalAltResult::ErrorFunctionArgMismatch) => (),
        _ => assert!(false),
    }
}
