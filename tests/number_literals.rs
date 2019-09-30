use rhai::Engine;

#[test]
fn test_number_literal() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("65") {
        assert_eq!(result, 65);
    } else {
        assert!(false);
    }
}

#[test]
fn test_hex_literal() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 0xf; x") {
        assert_eq!(result, 15);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("let x = 0xff; x") {
        assert_eq!(result, 255);
    } else {
        assert!(false);
    }
}

#[test]
fn test_octal_literal() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 0o77; x") {
        assert_eq!(result, 63);
    } else {
        assert!(false)
    }

    if let Ok(result) = engine.eval::<i64>("let x = 0o1234; x") {
        assert_eq!(result, 668);
    } else {
        assert!(false);
    }
}

#[test]
fn test_binary_literal() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 0b1111; x") {
        assert_eq!(result, 15);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("let x = 0b0011_1100_1010_0101; x") {
        assert_eq!(result, 15525);
    } else {
        assert!(false);
    }
}
