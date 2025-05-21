use crate::ast::{FormatSegment, Item, Module, Statement};
use std::collections::HashMap;
use std::fmt::Write;

pub mod ast;
mod parser;

pub fn parse(source: &str) -> Module {
    parser::module(source)
}

pub fn interpret(module: &Module) -> String {
    let mut variables = HashMap::new();
    let mut stdout = String::new();
    for item in &module.items {
        let Item::Function { name, body } = item;
        if *name == "main" {
            for statement in body {
                match statement {
                    Statement::Print { fmt } => {
                        for segment in &fmt.segments {
                            match segment {
                                FormatSegment::Text(text) => stdout.push_str(text),
                                FormatSegment::Variable(variable) => {
                                    let value = variables[variable];
                                    write!(stdout, "{value}").unwrap();
                                }
                            }
                        }
                        stdout.push('\n');
                    }
                    Statement::Assignment { variable, value } => {
                        variables.insert(*variable, *value);
                    }
                }
            }
        }
    }
    stdout
}
