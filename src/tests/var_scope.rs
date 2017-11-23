use engine::{Engine, Scope};

#[test]
fn test_var_scope() {
    let mut engine = Engine::new();
    let mut scope: Scope = Scope::new();

    if let Ok(_) = engine.eval_with_scope::<()>(&mut scope, "let x = 4 + 5") {
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval_with_scope::<i64>(&mut scope, "x") {
        assert_eq!(result, 9);
    } else {
        assert!(false);
    }

    if let Ok(_) = engine.eval_with_scope::<()>(&mut scope, "x = x + 1; x = x + 2;") {
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval_with_scope::<i64>(&mut scope, "x") {
        assert_eq!(result, 12);
    } else {
        assert!(false);
    }

    if let Ok(_) = engine.eval_with_scope::<()>(&mut scope, "{let x = 3}") {
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval_with_scope::<i64>(&mut scope, "x") {
        assert_eq!(result, 12);
    } else {
        assert!(false);
    }
}
