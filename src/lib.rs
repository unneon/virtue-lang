use crate::ast::{Item, Module, Statement};

pub mod ast;
mod parser;

pub fn parse(source: &str) -> Module {
    parser::module(source)
}

pub fn interpret(module: &Module) -> String {
    let mut stdout = String::new();
    for item in &module.items {
        let Item::Function { name, body } = item;
        if *name == "main" {
            for statement in body {
                let Statement::Print { fmt } = statement;
                stdout.push_str(fmt);
                stdout.push('\n');
            }
        }
    }
    stdout
}
