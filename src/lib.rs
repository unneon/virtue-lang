#![feature(decl_macro)]
#![feature(string_into_chars)]
#![feature(trait_alias)]
#![allow(mismatched_lifetime_syntaxes)]

pub mod ast;
pub mod codegen;
pub mod error;
pub mod parser;
pub mod typecheck;
pub mod util;
pub mod vir;
