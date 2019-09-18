use rhai::Engine;

#[test]
fn test_or_equals() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 16; x |= 74; x") {
        assert_eq!(result, 90);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<bool>("let x = true; x |= false; x") {
        assert!(result);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<bool>("let x = false; x |= true; x") {
        assert!(result);
    } else {
        assert!(false);
    }
}

#[test]
fn test_and_equals() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 16; x &= 31; x") {
        assert_eq!(result, 16);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<bool>("let x = true; x &= false; x") {
        assert_eq!(result, false);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<bool>("let x = false; x &= true; x") {
        assert_eq!(result, false);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<bool>("let x = true; x &= true; x") {
        assert!(result);
    } else {
        assert!(false);
    }
}

#[test]
fn test_xor_equals() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 90; x ^= 12; x") {
        assert_eq!(result, 86);
    } else {
        assert!(false);
    }
}

#[test]
fn test_multiply_equals() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 2; x *= 3; x") {
        assert_eq!(result, 6);
    } else {
        assert!(false);
    }
}

#[test]
fn test_divide_equals() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 6; x /= 2; x") {
        assert_eq!(result, 3);
    } else {
        assert!(false);
    }
}

#[test]
fn test_left_shift_equals() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 9; x >>=1; x") {
        assert_eq!(result, 4);
    } else {
        assert!(false);
    }
}

#[test]
fn test_right_shift_equals() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 4; x<<= 2; x") {
        assert_eq!(result, 16);
    } else {
        assert!(false);
    }
}

#[test]
fn test_modulo_equals() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 10; x %= 4; x") {
        assert_eq!(result, 2);
    } else {
        assert!(false);
    }
}
