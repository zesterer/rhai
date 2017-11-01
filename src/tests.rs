use engine::{Engine, Scope, EvalAltResult};
use fn_register::FnRegister;

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
fn test_chars() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<char>("'y'") {
        assert_eq!(result, 'y');
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<char>("'\\u2764'") {
        assert_eq!(result, '❤');
    } else {
        assert!(false);
    }

    match engine.eval::<char>("''") {
        Err(_) => (),
        _ => assert!(false),
    }
}

#[test]
fn test_ops() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("60 + 5") {
        assert_eq!(result, 65);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("(1 + 2) * (6 - 4) / 2") {
        assert_eq!(result, 3);
    } else {
        assert!(false);
    }
}

#[test]
fn test_mismatched_op() {
    let mut engine = Engine::new();

    match engine.eval::<i64>("60 + \"hello\"") {
        Err(EvalAltResult::ErrorFunctionArgMismatch) => (),
        _ => assert!(false),
    }
}

#[test]
fn test_bool_op1() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<bool>("true && (false || true)") {
        assert_eq!(result, true);
    } else {
        assert!(false);
    }
}

#[test]
fn test_bool_op2() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<bool>("false && (false || true)") {
        assert_eq!(result, false);
    } else {
        assert!(false);
    }
}

#[test]
fn test_op_prec() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 0; if x == 10 || true { x = 1} x") {
        assert_eq!(result, 1);
    } else {
        assert!(false);
    }
}

#[test]
fn test_if() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("if true { 55 }") {
        assert_eq!(result, 55);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("if false { 55 } else { 44 }") {
        assert_eq!(result, 44);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("if true { 55 } else { 44 }") {
        assert_eq!(result, 55);
    } else {
        assert!(false);
    }
}

#[test]
fn test_while() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 0; while x < 10 { x = x + 1; if x > 5 { \
                                            break } } x") {
        assert_eq!(result, 6);
    } else {
        assert!(false);
    }
}

#[test]
fn test_var_scope() {
    let mut engine = Engine::new();
    let mut scope: Scope = Vec::new();

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

#[test]
fn test_method_call() {
    #[derive(Clone)]
    struct TestStruct {
        x: i64,
    }

    impl TestStruct {
        fn update(&mut self) {
            self.x += 1000;
        }

        fn new() -> TestStruct {
            TestStruct { x: 1 }
        }
    }

    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    engine.register_fn("update", TestStruct::update);
    engine.register_fn("new_ts", TestStruct::new);

    if let Ok(result) = engine.eval::<TestStruct>("let x = new_ts(); x.update(); x") {
        assert_eq!(result.x, 1001);
    } else {
        assert!(false);
    }

}

#[test]
fn test_get_set() {
    #[derive(Clone)]
    struct TestStruct {
        x: i64,
    }

    impl TestStruct {
        fn get_x(&mut self) -> i64 {
            self.x
        }

        fn set_x(&mut self, new_x: i64) {
            self.x = new_x;
        }

        fn new() -> TestStruct {
            TestStruct { x: 1 }
        }
    }

    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    engine.register_get_set("x", TestStruct::get_x, TestStruct::set_x);
    engine.register_fn("new_ts", TestStruct::new);

    if let Ok(result) = engine.eval::<i64>("let a = new_ts(); a.x = 500; a.x") {
        assert_eq!(result, 500);
    } else {
        assert!(false);
    }
}

#[test]
fn test_big_get_set() {
    #[derive(Clone)]
    struct TestChild {
        x: i64,
    }

    impl TestChild {
        fn get_x(&mut self) -> i64 {
            self.x
        }

        fn set_x(&mut self, new_x: i64) {
            self.x = new_x;
        }

        fn new() -> TestChild {
            TestChild { x: 1 }
        }
    }

    #[derive(Clone)]
    struct TestParent {
        child: TestChild,
    }

    impl TestParent {
        fn get_child(&mut self) -> TestChild {
            self.child.clone()
        }

        fn set_child(&mut self, new_child: TestChild) {
            self.child = new_child;
        }

        fn new() -> TestParent {
            TestParent { child: TestChild::new() }
        }
    }

    let mut engine = Engine::new();

    engine.register_type::<TestChild>();
    engine.register_type::<TestParent>();

    engine.register_get_set("x", TestChild::get_x, TestChild::set_x);
    engine.register_get_set("child", TestParent::get_child, TestParent::set_child);

    engine.register_fn("new_tp", TestParent::new);

    if let Ok(result) = engine.eval::<i64>("let a = new_tp(); a.child.x = 500; a.child.x") {
        assert_eq!(result, 500);
    } else {
        assert!(false);
    }
}

#[test]
fn test_internal_fn() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("fn addme(a, b) { a+b } addme(3, 4)") {
        assert_eq!(result, 7);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("fn bob() { return 4; 5 } bob()") {
        assert_eq!(result, 4);
    } else {
        assert!(false);
    }
}

#[test]
fn test_big_internal_fn() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("fn mathme(a, b, c, d, e, f) { a - b * c + d * e - f \
                                            } mathme(100, 5, 2, 9, 6, 32)") {
        assert_eq!(result, 112);
    } else {
        assert!(false);
    }
}

#[test]
fn test_string() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<String>("\"Test string: \\u2764\"") {
        assert_eq!(result, "Test string: ❤");
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<String>("\"foo\" + \"bar\"") {
        assert_eq!(result, "foobar");
    } else {
        assert!(false);
    }
}

#[test]
fn test_arrays() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = [1, 2, 3]; x[1]") {
        assert_eq!(result, 2);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("let y = [1, 2, 3]; y[1] = 5; y[1]") {
        assert_eq!(result, 5);
    } else {
        assert!(false);
    }
}

#[test]
fn test_array_with_structs() {
    #[derive(Clone)]
    struct TestStruct {
        x: i64,
    }

    impl TestStruct {
        fn update(&mut self) {
            self.x += 1000;
        }

        fn get_x(&mut self) -> i64 {
            self.x
        }

        fn set_x(&mut self, new_x: i64) {
            self.x = new_x;
        }

        fn new() -> TestStruct {
            TestStruct { x: 1 }
        }
    }

    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    engine.register_get_set("x", TestStruct::get_x, TestStruct::set_x);
    engine.register_fn("update", TestStruct::update);
    engine.register_fn("new_ts", TestStruct::new);

    if let Ok(result) = engine.eval::<i64>("let a = [new_ts()]; a[0].x") {
        assert_eq!(result, 1);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("let a = [new_ts()]; a[0].x = 100; a[0].update(); \
                                            a[0].x") {
        assert_eq!(result, 1100);
    } else {
        assert!(false);
    }
}

#[test]
fn test_float() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<bool>("let x = 0.0; let y = 1.0; x < y") {
        assert!(result);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<bool>("let x = 0.0; let y = 1.0; x > y") {
        assert!(!result);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<f64>("let x = 9.9999; x") {
        assert_eq!(result, 9.9999);
    } else {
        assert!(false);
    }
}

#[test]
fn struct_with_float() {
    #[derive(Clone)]
    struct TestStruct {
        x: f64,
    }

    impl TestStruct {
        fn update(&mut self) {
            self.x += 5.789_f64;
        }

        fn get_x(&mut self) -> f64 {
            self.x
        }

        fn set_x(&mut self, new_x: f64) {
            self.x = new_x;
        }

        fn new() -> TestStruct {
            TestStruct { x: 1.0 }
        }
    }

    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    engine.register_get_set("x", TestStruct::get_x, TestStruct::set_x);
    engine.register_fn("update", TestStruct::update);
    engine.register_fn("new_ts", TestStruct::new);

    if let Ok(result) = engine.eval::<f64>("let ts = new_ts(); ts.update(); ts.x") {
        assert_eq!(result, 6.789);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<f64>("let ts = new_ts(); ts.x = 10.1001; ts.x") {
        assert_eq!(result, 10.1001);
    } else {
        assert!(false);
    }
}

#[test]
fn test_comments() {
	let mut engine = Engine::new();

	assert!(engine.eval::<i64>("let x = 5; x // I am a single line comment, yay!").is_ok());

	assert!(engine.eval::<i64>("let /* I am a multiline comment, yay! */ x = 5; x").is_ok());
}

#[test]
fn test_unary_minus() {
	let mut engine = Engine::new();

	assert_eq!(engine.eval::<i64>("let x = -5; x").unwrap(), -5);

	assert_eq!(engine.eval::<i64>("fn n(x) { -x } n(5)").unwrap(), -5);

	assert_eq!(engine.eval::<i64>("5 - -(-5)").unwrap(), 0);
}

#[test]
fn test_not() {
	let mut engine = Engine::new();

	assert_eq!(engine.eval::<bool>("let not_true = !true; not_true").unwrap(), false);

	assert_eq!(engine.eval::<bool>("fn not(x) { !x } not(false)").unwrap(), true);

	// TODO - do we allow stacking unary operators directly? e.g '!!!!!!!true'
	assert_eq!(engine.eval::<bool>("!(!(!(!(true))))").unwrap(), true)
}

#[test]
fn test_loop() {
	let mut engine = Engine::new();

	assert!(
		engine.eval::<bool>("
			let x = 0;
			let i = 0;

			loop {
				if i < 10 {
					x = x + i;
					i = i + 1;
				}
				else {
					break;
				}
			}

			x == 45
		").unwrap()
	)
}

#[test]
fn test_increment() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 1; x += 2; x") {
        assert_eq!(result, 3);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<String>("let s = \"test\"; s += \"ing\"; s") {
        assert_eq!(result, "testing".to_owned());
    } else {
        assert!(false);
    }
}

#[test]
fn test_decrement() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 10; x -= 7; x") {
        assert_eq!(result, 3);
    } else {
        assert!(false);
    }

    if let Ok(_) = engine.eval::<String>("let s = \"test\"; s -= \"ing\"; s") {
        assert!(false);
    } else {
        assert!(true);
    }
}

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
fn test_left_shift() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("4 << 2") {
        assert_eq!(result, 16);
    } else {
        assert!(false);
    }
}

#[test]
fn test_right_shift() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("9 >> 1") {
        assert_eq!(result, 4);
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
fn test_modulo_equals() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("let x = 10; x %= 4; x") {
        assert_eq!(result, 2);
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

#[test]
fn test_binary_ops() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("10 % 4") {
        assert_eq!(result, 2);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("10 << 4") {
        assert_eq!(result, 160);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("10 >> 4") {
        assert_eq!(result, 0);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("10 & 4") {
        assert_eq!(result, 0);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("10 | 4") {
        assert_eq!(result, 14);
    } else {
        assert!(false);
    }

    if let Ok(result) = engine.eval::<i64>("10 ^ 4") {
        assert_eq!(result, 14);
    } else {
        assert!(false);
    }
}

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
