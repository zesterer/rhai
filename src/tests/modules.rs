use std::fs;
use std::io::Write;

use engine::Engine;
use fn_register::FnRegister;

#[test]
fn basic_module() {
    match fs::File::create("__test_module.rhai") {
        Ok(mut f) => write!(f, "let potato = 25;").expect("couldn't write to file") ,
        Err(_) => {
            eprintln!("failed to write to test file");
            assert!(false)
        }
    };

    assert_eq!(
        Engine::new()
        .eval::<i64>("
            let mod = import \"__test_module.rhai\";
            use mod::potato;

            potato
        ").unwrap(),
        25
    );

    fs::remove_file("__test_module.rhai")
        .expect("couldn't remove test file");
}

#[test]
fn call_fn_from_module() {
    match fs::File::create("__test_module_fn.rhai") {
        Ok(mut f) => write!(f, "fn potato() {{ 25 }}").expect("couldn't write to file") ,
        Err(_) => {
            eprintln!("failed to write to test file");
            assert!(false)
        }
    };

    assert_eq!(
        Engine::new()
        .eval::<i64>("
            let mod = import \"__test_module_fn.rhai\";
            use mod::potato;

            potato()
        ").unwrap(),
        25
    );

    fs::remove_file("__test_module_fn.rhai")
        .expect("couldn't remove test file");
}

#[test]
fn call_rust_fn_from_module() {
    fn my_potato() -> i64 { 25 }

    fn register(e: &mut Engine) {
        e.register_fn("my_potato", my_potato);
    }

    match fs::File::create("__test_module_rust_fn.rhai") {
        Ok(mut f) => write!(f, "fn potato() {{ my_potato() }}").expect("couldn't write to file") ,
        Err(_) => {
            eprintln!("failed to write to test file");
            assert!(false)
        }
    };

    let mut engine = Engine::new();
    engine.module_fns(register);

    assert_eq!(
        engine
        .eval::<i64>("
            let mod = import \"__test_module_rust_fn.rhai\";
            use mod::potato;

            potato()
        ").unwrap(),
        25
    );

    fs::remove_file("__test_module_rust_fn.rhai")
        .expect("couldn't remove test file");
}
