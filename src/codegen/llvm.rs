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

    for (function_id, function) in hir.functions.iter().enumerate() {
        let function_name = function.name;
        writeln!(&mut ir, "define i32 @{function_name}() {{").unwrap();
        for (block_id, block) in function.blocks.iter().enumerate() {
            if block_id > 0 {
                writeln!(&mut ir, "_{block_id}:").unwrap();
            }
            for (statement_id, statement) in block.iter().enumerate() {
                match statement {
                    Statement::Assignment(left, right) => {
                        match function.bindings[left.id].type_ {
                            Type::I64 => {
                                writeln!(
                                    &mut ir,
                                    "    %{} = xor i64 %{}, 0",
                                    left.id + 1,
                                    right.id + 1
                                )
                                .unwrap();
                            }
                            Type::String => {
                                writeln!(
                                    &mut ir,
                                    "    %{} = getelementptr i8, i8* %{}, i32 0",
                                    left.id + 1,
                                    right.id + 1
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
                            _ => todo!(),
                        };
                        writeln!(
                            &mut ir,
                            "    %{} = {} i64 %{}, %{}",
                            binding.id + 1,
                            instr,
                            left.id + 1,
                            right.id + 1
                        )
                        .unwrap();
                    }
                    Statement::Literal(binding, literal) => {
                        writeln!(&mut ir, "    %{} = xor i64 {literal}, 0", binding.id + 1)
                            .unwrap();
                    }
                    Statement::Print(fmt) => {
                        write!(&mut ir, "    call i32 (i8*, ...) @printf(i8* @fmt_{function_id}_{block_id}_{statement_id}").unwrap();
                        for segment in &fmt.segments {
                            if let FormatSegment::Arg(arg) = segment {
                                let type_ = match function.bindings[arg.id].type_ {
                                    Type::I64 => "i64",
                                    Type::String => "i8*",
                                    _ => todo!(),
                                };
                                write!(&mut ir, ", {type_} %{}", arg.id + 1).unwrap();
                            }
                        }
                        writeln!(&mut ir, ")").unwrap();
                    }
                    Statement::StringConstant(binding, string_id) => {
                        let string_len = hir.strings[*string_id].len() + 1;
                        writeln!(&mut ir, "    %{} = getelementptr [{string_len} x i8], [{string_len} x i8]* @string_{string_id}, i32 0, i32 0", binding.id + 1).unwrap();
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
