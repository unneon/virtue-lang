#![feature(trait_alias)]

use crate::ast::Module;

pub mod ast;
pub mod codegen;
mod parser;

pub fn parse(source: &str) -> Module {
    parser::module(source)
}
