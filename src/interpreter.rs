use crate::ast::{BinaryOperator, Expression, FormatSegment, Item, Module, Statement};
use std::collections::HashMap;
use std::fmt::Write;

pub fn run(module: &Module) -> String {
    let mut variables = HashMap::new();
    let mut stdout = String::new();
    for item in &module.items {
        let Item::Function { name, body } = item;
        if *name == "main" {
            execute(body, &mut variables, &mut stdout);
        }
    }
    stdout
}

fn execute<'a>(block: &'a [Statement], variables: &mut HashMap<&'a str, i64>, stdout: &mut String) {
    for statement in block {
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
                variables.insert(*variable, evaluate(expression, variables));
            }
            Statement::While { condition, body } => {
                while evaluate(condition, variables) == 1 {
                    execute(body, variables, stdout);
                }
            }
        }
    }
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
                BinaryOperator::LessOrEqual => {
                    if left <= right {
                        1
                    } else {
                        0
                    }
                }
            }
        }
        Expression::Literal(value) => *value,
        Expression::Variable(variable) => variables[variable],
    }
}
