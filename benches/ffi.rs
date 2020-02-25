#![feature(test)]
extern crate test;

use rhai::RegisterFn as _;
use rhai_old::RegisterFn as _;
use test::{Bencher, black_box};

#[derive(Clone, Debug)]
struct Fib {
    x: i64,
    y: i64,
}

impl Fib {
    fn new() -> Self {
        Self { x: 1, y: 1 }
    }

    fn get(&mut self) -> i64 {
        self.x
    }

    fn update(&mut self) {
        self.y += self.x + self.y;
    }
}

#[bench]
fn ffi_old(b: &mut Bencher) {
    let mut engine = rhai_old::Engine::new();

    engine.register_type::<Fib>();

    engine.register_fn("new_fib", Fib::new);
    engine.register_fn("update", Fib::update);
    engine.register_fn("get", Fib::get);

    b.iter(|| {
        black_box(engine.eval::<i64>(r#"
            let fib = new_fib();
            for i in range(0, 1000) {
                fib.update();
            }
            fib.get()
        "#).unwrap());
    })
}

#[bench]
fn ffi(b: &mut Bencher) {
    let mut engine = rhai::Engine::new();

    engine.register_type::<Fib>();

    engine.register_fn("new_fib", Fib::new);
    engine.register_fn("update", Fib::update);
    engine.register_fn("get", Fib::get);

    b.iter(|| {
        black_box(engine.eval::<i64>(r#"
            let fib = new_fib();
            for i in range(0, 1000) {
                fib.update();
            }
            fib.get()
        "#).unwrap());
    })
}
