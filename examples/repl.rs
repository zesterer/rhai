use rhai::{Engine, RegisterFn, Scope};
use std::fmt::Display;
use std::io::{stdin, stdout, Write};
use std::process::exit;

fn showit<T: Display>(x: &mut T) -> () {
    println!("{}", x)
}

fn quit() {
    exit(0);
}

pub fn main() {
    let mut engine = Engine::new();
    let mut scope = Scope::new();

    engine.register_fn("print", showit as fn(x: &mut i32) -> ());
    engine.register_fn("print", showit as fn(x: &mut i64) -> ());
    engine.register_fn("print", showit as fn(x: &mut u32) -> ());
    engine.register_fn("print", showit as fn(x: &mut u64) -> ());
    engine.register_fn("print", showit as fn(x: &mut f32) -> ());
    engine.register_fn("print", showit as fn(x: &mut f64) -> ());
    engine.register_fn("print", showit as fn(x: &mut bool) -> ());
    engine.register_fn("print", showit as fn(x: &mut String) -> ());
    engine.register_fn("exit", quit);

    loop {
        print!("> ");
        let mut input = String::new();
        stdout().flush().expect("couldn't flush stdout");
        if let Err(e) = stdin().read_line(&mut input) {
            println!("input error: {}", e);
        }

        if let Err(e) = engine.consume_with_scope(&mut scope, &input) {
            println!("error: {}", e);
        }
    }
}
