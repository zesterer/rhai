extern crate rhai;
use rhai::{Any, Engine, EvalAltResult, RegisterFn};

#[derive(Clone, Debug)]
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

fn print_ty_name(test: Vec<&mut Box<Any>>) -> Result<Box<Any>, EvalAltResult> {
    println!("Ty name: {:?}", (*test[0]).as_ref().type_name());

    Ok(Box::new(()))
}

fn main() {
    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    print_ty_name(vec![&mut (Box::new(TestStruct::new()) as Box<Any>)]).unwrap();

    engine.register_fn("update", TestStruct::update);
    engine.register_fn("new_ts", TestStruct::new);
    engine.register_fn_raw("ty_name".to_owned(), None, Box::new(print_ty_name));

    println!(
        "{:?}",
        engine.eval::<TestStruct>("let x = new_ts(); x.update(); x")
    );
    println!(
        "{:?}",
        engine.eval::<TestStruct>("let x = [new_ts()]; x[0].update(); x[0]")
    );
}
