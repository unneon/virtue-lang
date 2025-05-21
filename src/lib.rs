use crate::ast::Module;

pub mod ast;
mod interpreter;
mod parser;

pub fn parse(source: &str) -> Module {
    parser::module(source)
}

pub fn interpret(module: &Module) -> String {
    interpreter::run(module)
}
