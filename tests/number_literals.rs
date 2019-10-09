use rhai::Engine;

#[test]
fn test_number_literal() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("65"), Ok(65));
}

#[test]
fn test_hex_literal() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 0xf; x"), Ok(15));
    assert_eq!(engine.eval::<i64>("let x = 0xff; x"), Ok(255));
}

#[test]
fn test_octal_literal() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 0o77; x"), Ok(63));
    assert_eq!(engine.eval::<i64>("let x = 0o1234; x"), Ok(668));
}

#[test]
fn test_binary_literal() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 0b1111; x"), Ok(15));
    assert_eq!(
        engine.eval::<i64>("let x = 0b0011_1100_1010_0101; x"),
        Ok(15525)
    );
}
