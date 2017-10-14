//! Rhai - embedded scripting for Rust

// lints required by Rhai
#![allow(unknown_lints,
        type_complexity,
        new_without_default_derive,
        needless_pass_by_value,
        too_many_arguments)]

mod engine;
mod fn_register;
mod parser;

#[cfg(test)]
mod tests;

pub use engine::{Engine, Scope};
pub use fn_register::FnRegister;
