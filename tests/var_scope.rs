use rhai::{Engine, Scope};

#[test]
fn test_var_scope() {
    let mut engine = Engine::new();
    let mut scope = Scope::new();

    assert_eq!(
        engine.eval_with_scope::<()>(&mut scope, "let x = 4 + 5"),
        Ok(())
    );

    assert_eq!(engine.eval_with_scope::<i64>(&mut scope, "x"), Ok(9));

    assert_eq!(
        engine.eval_with_scope::<()>(&mut scope, "x = x + 1; x = x + 2;"),
        Ok(())
    );

    assert_eq!(engine.eval_with_scope::<i64>(&mut scope, "x"), Ok(12));

    assert_eq!(
        engine.eval_with_scope::<()>(&mut scope, "{let x = 3}"),
        Ok(())
    );

    assert_eq!(engine.eval_with_scope::<i64>(&mut scope, "x"), Ok(12));
}
