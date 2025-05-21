use crate::ast::{BinaryOperator, Expression, FormatSegment, Item, Module, Statement};
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
                    Statement::Assignment {
                        variable,
                        expression,
                    } => {
                        variables.insert(*variable, evaluate(expression, &variables));
                    }
                }
            }
        }
    }
    stdout
}

fn evaluate(expression: &Expression, variables: &HashMap<&str, i64>) -> i64 {
    match expression {
        Expression::BinaryOperation(op, args) => {
            let (left, right) = args.as_ref();
            let left = evaluate(left, variables);
            let right = evaluate(right, variables);
            match op {
                BinaryOperator::Add => left + right,
                BinaryOperator::Subtract => left - right,
                BinaryOperator::Multiply => left * right,
                BinaryOperator::Divide => left / right,
            }
        }
        Expression::Literal(value) => *value,
        Expression::Variable(variable) => variables[variable],
    }
}
