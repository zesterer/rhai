use rhai::Engine;

#[test]
fn test_power_of() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("2 ~ 3"), Ok(8));
    assert_eq!(engine.eval::<i64>("(-2 ~ 3)"), Ok(-8));
    assert_eq!(engine.eval::<f64>("2.2 ~ 3.3"), Ok(13.489468760533386_f64));
    assert_eq!(engine.eval::<f64>("2.0~-2.0"), Ok(0.25_f64));
    assert_eq!(engine.eval::<f64>("(-2.0~-2.0)"), Ok(0.25_f64));
    assert_eq!(engine.eval::<f64>("(-2.0~-2)"), Ok(0.25_f64));
    assert_eq!(engine.eval::<i64>("4~3"), Ok(64));
}

#[test]
fn test_power_of_equals() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 2; x ~= 3; x"), Ok(8));
    assert_eq!(engine.eval::<i64>("let x = -2; x ~= 3; x"), Ok(-8));
    assert_eq!(
        engine.eval::<f64>("let x = 2.2; x ~= 3.3; x"),
        Ok(13.489468760533386_f64)
    );
    assert_eq!(
        engine.eval::<f64>("let x = 2.0; x ~= -2.0; x"),
        Ok(0.25_f64)
    );
    assert_eq!(
        engine.eval::<f64>("let x = -2.0; x ~= -2.0; x"),
        Ok(0.25_f64)
    );
    assert_eq!(engine.eval::<f64>("let x = -2.0; x ~= -2; x"), Ok(0.25_f64));
    assert_eq!(engine.eval::<i64>("let x =4; x ~= 3; x"), Ok(64));
}
