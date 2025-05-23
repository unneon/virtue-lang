mod subprocess;

pub use subprocess::compile_ir;

use crate::ast::BinaryOperator;
use crate::hir::{FormatSegment, Program, Statement, Type};
use std::fmt::Write;

pub fn make_ir(hir: &Program) -> String {
    let mut ir = String::new();
    ir.push_str("declare i32 @printf(i8*, ...)\n\n");

    for (string_id, string) in hir.strings.iter().enumerate() {
        let string_len = string.len() + 1;
        writeln!(
            &mut ir,
            "@string_{string_id} = internal constant [{string_len} x i8] c\"{string}\\00\""
        )
        .unwrap();
    }
    for (function_id, function) in hir.functions.iter().enumerate() {
        for (block_id, block) in function.blocks.iter().enumerate() {
            for (statement_id, statement) in block.iter().enumerate() {
                if let Statement::Print(fmt) = statement {
                    let fmt = fmt.printf_format(function, "\\0A\\00");
                    let fmt_len = fmt.len() - 4;
                    writeln!(&mut ir, "@fmt_{function_id}_{block_id}_{statement_id} = internal constant [{fmt_len} x i8] c\"{fmt}\"").unwrap();
                }
            }
        }
    }

    ir.push('\n');

    let mut temp_counter = 0;

    for (function_id, function) in hir.functions.iter().enumerate() {
        let function_name = function.name;
        writeln!(&mut ir, "define i32 @{function_name}() {{").unwrap();
        for (binding_id, binding) in function.bindings.iter().enumerate() {
            let type_ = match binding.type_ {
                Type::I64 => "i64",
                Type::String => "i8*",
                _ => todo!(),
            };
            writeln!(&mut ir, "    %stack_{binding_id} = alloca {type_}").unwrap();
        }

        'for_blocks: for (block_id, block) in function.blocks.iter().enumerate() {
            if block_id > 0 {
                writeln!(&mut ir, "block_{block_id}:").unwrap();
            }
            for (statement_id, statement) in block.iter().enumerate() {
                match statement {
                    Statement::Assignment(left, right) => {
                        match function.bindings[left.id].type_ {
                            Type::I64 => {
                                let temp = temp_counter;
                                temp_counter += 1;
                                writeln!(
                                    &mut ir,
                                    "    %temp_{temp} = load i64, i64* %stack_{}",
                                    right.id
                                )
                                .unwrap();
                                writeln!(
                                    &mut ir,
                                    "    store i64 %temp_{temp}, i64* %stack_{}",
                                    left.id,
                                )
                                .unwrap();
                            }
                            Type::String => {
                                let temp = temp_counter;
                                temp_counter += 1;
                                writeln!(
                                    &mut ir,
                                    "    %temp_{temp} = load i8*, i8** %stack_{}",
                                    right.id
                                )
                                .unwrap();
                                writeln!(
                                    &mut ir,
                                    "    store i8* %temp_{temp}, i8** %stack_{}",
                                    left.id,
                                )
                                .unwrap();
                            }
                            _ => todo!(),
                        };
                    }
                    Statement::BinaryOperator(binding, op, left, right) => {
                        let instr = match op {
                            BinaryOperator::Add => "add",
                            BinaryOperator::Subtract => "sub",
                            BinaryOperator::Multiply => "mul",
                            BinaryOperator::Divide => "sdiv",
                            BinaryOperator::Modulo => "srem",
                            BinaryOperator::Less => "icmp slt",
                            BinaryOperator::LessOrEqual => "icmp sle",
                            BinaryOperator::Greater => "icmp sgt",
                            BinaryOperator::GreaterOrEqual => "icmp sge",
                            BinaryOperator::Equal => "icmp eq",
                            BinaryOperator::NotEqual => "icmp ne",
                        };
                        let returns_bool = matches!(
                            op,
                            BinaryOperator::Less
                                | BinaryOperator::LessOrEqual
                                | BinaryOperator::Greater
                                | BinaryOperator::GreaterOrEqual
                                | BinaryOperator::Equal
                                | BinaryOperator::NotEqual
                        );
                        let temp_left = temp_counter;
                        let temp_right = temp_counter + 1;
                        let temp_result = temp_counter + 2;
                        temp_counter += 3;
                        writeln!(
                            &mut ir,
                            "    %temp_{temp_left} = load i64, i64* %stack_{}",
                            left.id,
                        )
                        .unwrap();
                        writeln!(
                            &mut ir,
                            "    %temp_{temp_right} = load i64, i64* %stack_{}",
                            right.id
                        )
                        .unwrap();
                        writeln!(
                            &mut ir,
                            "    %temp_{temp_result} = {instr} i64 %temp_{temp_left}, %temp_{temp_right}",
                        ).unwrap();
                        if !returns_bool {
                            writeln!(
                                &mut ir,
                                "    store i64 %temp_{temp_result}, i64* %stack_{}",
                                binding.id
                            )
                            .unwrap();
                        } else {
                            let temp_upcast = temp_counter;
                            temp_counter += 1;
                            writeln!(
                                &mut ir,
                                "    %temp_{temp_upcast} = zext i1 %temp_{temp_result} to i64",
                            )
                            .unwrap();
                            writeln!(
                                &mut ir,
                                "    store i64 %temp_{temp_upcast}, i64* %stack_{}",
                                binding.id
                            )
                            .unwrap();
                        }
                    }
                    Statement::JumpAlways(block) => {
                        writeln!(
                            &mut ir,
                            "    br i1 1, label %block_{block}, label %block_{block}"
                        )
                        .unwrap();
                        continue 'for_blocks;
                    }
                    Statement::JumpConditional {
                        condition,
                        true_block,
                        false_block,
                    } => {
                        let temp_i64 = temp_counter;
                        let temp_i1 = temp_counter + 1;
                        temp_counter += 2;
                        writeln!(
                            &mut ir,
                            "    %temp_{temp_i64} = load i64, i64* %stack_{}",
                            condition.id
                        )
                        .unwrap();
                        writeln!(
                            &mut ir,
                            "    %temp_{temp_i1} = trunc i64 %temp_{temp_i64} to i1"
                        )
                        .unwrap();
                        writeln!(
                            &mut ir,
                            "    br i1 %temp_{temp_i1}, label %block_{true_block}, label %block_{false_block}",
                        )
                        .unwrap();
                        continue 'for_blocks;
                    }
                    Statement::Literal(binding, literal) => {
                        writeln!(
                            &mut ir,
                            "    store i64 {literal}, i64* %stack_{}",
                            binding.id
                        )
                        .unwrap();
                    }
                    Statement::Print(fmt) => {
                        let mut args = String::new();
                        for segment in &fmt.segments {
                            if let FormatSegment::Arg(arg) = segment {
                                let type_ = match function.bindings[arg.id].type_ {
                                    Type::I64 => "i64",
                                    Type::String => "i8*",
                                    _ => todo!(),
                                };
                                let temp = temp_counter;
                                temp_counter += 1;
                                writeln!(
                                    &mut ir,
                                    "    %temp_{temp} = load {type_}, {type_}* %stack_{}",
                                    arg.id
                                )
                                .unwrap();
                                write!(&mut args, ", {type_} %temp_{temp}").unwrap();
                            }
                        }
                        writeln!(&mut ir, "    call i32 (i8*, ...) @printf(i8* @fmt_{function_id}_{block_id}_{statement_id}{args})").unwrap();
                    }
                    Statement::StringConstant(binding, string_id) => {
                        let string_len = hir.strings[*string_id].len() + 1;
                        let temp = temp_counter;
                        temp_counter += 1;
                        writeln!(&mut ir, "    %temp_{temp} = getelementptr [{string_len} x i8], [{string_len} x i8]* @string_{string_id}, i32 0, i32 0").unwrap();
                        writeln!(
                            &mut ir,
                            "    store i8* %temp_{temp}, i8** %stack_{}",
                            binding.id
                        )
                        .unwrap();
                    }
                    _ => {}
                }
            }
            writeln!(&mut ir, "    ret i32 0").unwrap();
        }
        writeln!(&mut ir, "}}").unwrap();
    }

    ir
}
