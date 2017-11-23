//! # Rhai - embedded scripting for Rust
//! Rhai is a tiny, simple and very fast embedded scripting language for Rust
//! that gives you a safe and easy way to add scripting to your applications.
//! It provides a familiar syntax based on JS and Rust and a simple Rust interface.
//! Here is a quick example. First, the contents of `my_script.rhai`:
//!
//! ```rust_todo_disable_testing_enable_highlighting
//! fn factorial(x) {
//!     if x == 1 { return 1; }
//!     x * factorial(x - 1)
//! }
//!
//! compute_something(factorial(10))
//! ```
//!
//! And the Rust part:
//!
//! ```rust
//! use rhai::{FnRegister, Engine};
//!
//! fn compute_something(x: i64) -> bool {
//!     (x % 40) == 0
//! }
//!
//! let mut engine = Engine::new();
//! engine.register_fn("compute_something", compute_something);
//! # // Very ugly hack incoming, TODO
//! # use std::fs::{File, remove_file};
//! # use std::io::Write;
//! # let mut f = File::create("my_script.rhai").unwrap();
//! # let _ = write!(f, "{}", "fn f(x) { if x == 1 { return 1; } x * f(x-1) } compute_something(f(10))");
//! assert!(engine.eval_file::<bool>("my_script.rhai").unwrap());
//! # let _ = remove_file("my_script.rhai");
//! ```
//!
//! [Check out the README on github for more information!](https://github.com/jonathandturner/rhai)

// lints required by Rhai
#![allow(unknown_lints,
        type_complexity,
        new_without_default_derive,
        needless_pass_by_value,
        too_many_arguments,
        new_without_default,
        cyclomatic_complexity)]

mod engine;
mod fn_register;
mod parser;

#[cfg(feature = "modules")]
mod module;

#[cfg(test)]
mod tests;

pub use engine::{Engine, Scope, EvalAltResult};
pub use fn_register::FnRegister;

#[cfg(feature = "modules")]
pub use module::{Module, ModuleError, rhai_import};
