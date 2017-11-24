use engine::Engine;

#[test]
fn test_power_of() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("2 ~ 3").unwrap(), 8);
    assert_eq!(engine.eval::<i64>("(-2 ~ 3)").unwrap(), -8);
    assert_eq!(engine.eval::<f64>("2.2 ~ 3.3").unwrap(), 13.489468760533386_f64);
    assert_eq!(engine.eval::<f64>("2.0~-2.0").unwrap(), 0.25_f64);
    assert_eq!(engine.eval::<f64>("(-2.0~-2.0)").unwrap(), 0.25_f64);
    assert_eq!(engine.eval::<f64>("(-2.0~-2)").unwrap(), 0.25_f64);
    assert_eq!(engine.eval::<i64>("4~3").unwrap(), 64);
}

#[test]
fn test_power_of_equals() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 2; x ~= 3; x").unwrap(), 8);
    assert_eq!(engine.eval::<i64>("let x = -2; x ~= 3; x").unwrap(), -8);
    assert_eq!(engine.eval::<f64>("let x = 2.2; x ~= 3.3; x").unwrap(), 13.489468760533386_f64);
    assert_eq!(engine.eval::<f64>("let x = 2.0; x ~= -2.0; x").unwrap(), 0.25_f64);
    assert_eq!(engine.eval::<f64>("let x = -2.0; x ~= -2.0; x").unwrap(), 0.25_f64);
    assert_eq!(engine.eval::<f64>("let x = -2.0; x ~= -2; x").unwrap(), 0.25_f64);
    assert_eq!(engine.eval::<i64>("let x =4; x ~= 3; x").unwrap(), 64);
}
