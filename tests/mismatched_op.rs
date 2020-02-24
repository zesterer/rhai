use rhai::{Engine, EvalAltResult, RegisterFn};

#[test]
fn test_mismatched_op() {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<i64>("60 + \"hello\""),
        Err(EvalAltResult::ErrorMismatchOutputType(
            "alloc::string::String".into()
        ))
    );
}

#[test]
fn test_mismatched_op_custom_type() {
    #[derive(Clone)]
    struct TestStruct {
        x: i64,
    }

    impl TestStruct {
        fn new() -> TestStruct {
            TestStruct { x: 1 }
        }
    }

    let mut engine = Engine::new();
    engine.register_type::<TestStruct>();
    engine.register_fn("new_ts", TestStruct::new);

    assert_eq!(
        engine.eval::<i64>("60 + new_ts()"),
        Err(EvalAltResult::ErrorFunctionNotFound(
            "+ (i64, mismatched_op::test_mismatched_op_custom_type::TestStruct)".into()
        ))
    );
}
