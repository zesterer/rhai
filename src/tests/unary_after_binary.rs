use engine::Engine;

#[test]
// TODO also add test case for unary after compound
// Hah, turns out unary + has a good use after all!
fn test_unary_after_binary()
{
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("10 % +4") {
        assert_eq!(result, 2);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("10 << +4") {
        assert_eq!(result, 160);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("10 >> +4") {
        assert_eq!(result, 0);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("10 & +4") {
        assert_eq!(result, 0);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("10 | +4") {
        assert_eq!(result, 14);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("10 ^ +4") {
        assert_eq!(result, 14);
    } else {
        assert!(false);
    }
}
