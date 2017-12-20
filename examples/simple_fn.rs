extern crate rhai;
use rhai::{Any, Engine, RegisterFn};

fn add(x: i64, y: i64) -> i64 {
    println!("Adding: {}", x + y);

    x + y
}

fn main() {
    let mut engine = Engine::new();

    engine.register_fn("add", add);
    engine
        .call_fn_raw(
            "add".to_owned(),
            vec![
                &mut (Box::new(3i64) as Box<Any>),
                &mut (Box::new(5i64) as Box<Any>),
            ],
        )
        .expect("FAIL");

    //if let Ok(result) = engine.eval::<i64>("add(40, 2)") {
    //    println!("Answer: {}", result); // prints 42
    //}
}
