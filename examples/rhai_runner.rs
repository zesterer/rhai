use std::env;
use std::fmt::Display;

extern crate rhai;
use rhai::{Engine, FnRegister};

fn showit<T: Display>(x: &mut T) -> () {
    println!("{}", x)
}

fn main() {
    for fname in env::args().skip(1) {
        let mut engine = Engine::new();

        register(&mut engine);
        engine.module_fns(register);
        match engine.eval_file::<()>(&fname) {
            Ok(_) => (),
            Err(e) => {println!("Error: {}", e)}
        }
    }
}

fn register(engine: &mut Engine) {
    println!("register ran");
    engine.register_fn("print", showit as fn(x: &mut i32)->());
    engine.register_fn("print", showit as fn(x: &mut i64)->());
    engine.register_fn("print", showit as fn(x: &mut u32)->());
    engine.register_fn("print", showit as fn(x: &mut u64)->());
    engine.register_fn("print", showit as fn(x: &mut f32)->());
    engine.register_fn("print", showit as fn(x: &mut f64)->());
    engine.register_fn("print", showit as fn(x: &mut bool)->());
    engine.register_fn("print", showit as fn(x: &mut String)->());
}
