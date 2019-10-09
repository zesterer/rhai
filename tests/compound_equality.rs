use rhai::Engine;

#[test]
fn test_or_equals() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 16; x |= 74; x"), Ok(90));
    assert_eq!(engine.eval::<bool>("let x = true; x |= false; x"), Ok(true));
    assert_eq!(engine.eval::<bool>("let x = false; x |= true; x"), Ok(true));
}

#[test]
fn test_and_equals() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 16; x &= 31; x"), Ok(16));
    assert_eq!(
        engine.eval::<bool>("let x = true; x &= false; x"),
        Ok(false)
    );
    assert_eq!(
        engine.eval::<bool>("let x = false; x &= true; x"),
        Ok(false)
    );
    assert_eq!(engine.eval::<bool>("let x = true; x &= true; x"), Ok(true));
}

#[test]
fn test_xor_equals() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 90; x ^= 12; x"), Ok(86));
}

#[test]
fn test_multiply_equals() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 2; x *= 3; x"), Ok(6));
}

#[test]
fn test_divide_equals() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 6; x /= 2; x"), Ok(3));
}

#[test]
fn test_left_shift_equals() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 9; x >>=1; x"), Ok(4));
}

#[test]
fn test_right_shift_equals() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 4; x<<= 2; x"), Ok(16));
}

#[test]
fn test_modulo_equals() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 10; x %= 4; x"), Ok(2));
}
