mod subprocess;

pub use subprocess::compile_c;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::vir::{BaseType, FormatSegment, Function, Program, Statement, Type};
use std::fmt::Write;

struct State<'a> {
    c: String,
    vir: &'a Program<'a>,
    current_function: usize,
}

impl State<'_> {
    fn prologue(&mut self) {
        self.write("#include <stdio.h>");
        self.write("#include <stdlib.h>");
        for (struct_id, struct_) in self.vir.structs.iter().enumerate() {
            self.write(format!("struct struct{struct_id} {{"));
            for (field_id, field_type) in struct_.fields.iter().enumerate() {
                let field_type = convert_type(field_type);
                self.write(format!("    {field_type} _{field_id};"));
            }
            self.write("};");
        }
        for function in &self.vir.functions {
            let signature = self.function_signature(function);
            self.write(format!("{signature};"));
        }
        for (string_id, string) in self.vir.strings.iter().enumerate() {
            self.write(format!("char str{string_id}[] = \"{string}\";"));
        }
    }

    fn all_functions(&mut self) {
        for function_id in 0..self.vir.functions.len() {
            self.current_function = function_id;
            self.function();
        }
    }

    fn function(&mut self) {
        let function = &self.vir.functions[self.current_function];
        let function_signature = self.function_signature(function);
        self.write(format!("{function_signature} {{"));
        for (binding, binding_data) in function
            .bindings
            .iter()
            .enumerate()
            .skip(function.args.len())
        {
            let binding_type = convert_type(&binding_data.type_);
            self.write(format!("    {binding_type} _{binding};"));
        }
        for block_id in 0..function.blocks.len() {
            self.block(block_id);
        }
        self.write("}");
    }

    fn block(&mut self, block_id: usize) {
        self.write(format!("block{block_id}:"));

        let function = &self.vir.functions[self.current_function];
        let block = &function.blocks[block_id];
        for statement in block {
            match statement {
                Statement::Assignment(left, right) => {
                    let left_id = left.id;
                    let right_id = right.id;
                    self.write(format!("    _{left_id} = _{right_id};"));
                }
                Statement::AssignmentField(object, field, value) => {
                    let object_id = object.id;
                    let value_id = value.id;
                    self.write(format!("    _{object_id}._{field} = _{value_id};"));
                }
                Statement::AssignmentIndex(array, index, value) => {
                    let array_id = array.id;
                    let index_id = index.id;
                    let value_id = value.id;
                    self.write(format!("    _{array_id}[_{index_id}] = _{value_id};"));
                }
                Statement::BinaryOperator(result, op, left, right) => {
                    let result_id = result.id;
                    let left_id = left.id;
                    let right_id = right.id;
                    let op = match op {
                        BinaryOperator::Add => "+",
                        BinaryOperator::Subtract => "-",
                        BinaryOperator::Multiply => "*",
                        BinaryOperator::Divide => "/",
                        BinaryOperator::Modulo => "%",
                        BinaryOperator::BitAnd => "&",
                        BinaryOperator::BitOr => "|",
                        BinaryOperator::BitXor => "^",
                        BinaryOperator::BitShiftLeft => "<<",
                        BinaryOperator::BitShiftRight => ">>",
                        BinaryOperator::Less => "<",
                        BinaryOperator::LessOrEqual => "<=",
                        BinaryOperator::Greater => ">",
                        BinaryOperator::GreaterOrEqual => ">=",
                        BinaryOperator::Equal => "==",
                        BinaryOperator::NotEqual => "!=",
                    };
                    self.write(format!("    _{result_id} = _{left_id} {op} _{right_id};"));
                }
                Statement::Call(result, function, args) => {
                    let result_id = result.id;
                    let function_name = self.vir.functions[*function].name;
                    let mut c_args = String::new();
                    for (arg_index, arg) in args.iter().enumerate() {
                        if arg_index > 0 {
                            c_args.push_str(", ");
                        }
                        let arg_id = arg.id;
                        c_args.push_str(&format!("_{arg_id}"));
                    }
                    self.write(format!("    _{result_id} = {function_name}({c_args});"));
                }
                Statement::Field(result, object, field) => {
                    let result_id = result.id;
                    let object_id = object.id;
                    self.write(format!("    _{result_id} = _{object_id}._{field};"));
                }
                Statement::Index(result, array, index) => {
                    let result_id = result.id;
                    let array_id = array.id;
                    let index_id = index.id;
                    self.write(format!("    _{result_id} = _{array_id}[_{index_id}];"));
                }
                Statement::JumpAlways(target_block) => {
                    self.write(format!("    goto block{target_block};"));
                }
                Statement::JumpConditional {
                    condition,
                    true_block,
                    false_block,
                } => {
                    let condition_id = condition.id;
                    self.write(format!("    if (_{condition_id}) goto block{true_block};"));
                    self.write(format!("    else goto block{false_block};"));
                }
                Statement::Literal(binding, value) => {
                    let binding_id = binding.id;
                    self.write(format!("    _{binding_id} = {value};"));
                }
                Statement::New(_, _) => {}
                Statement::NewArray(array, length) => {
                    let array_id = array.id;
                    let length_id = length.id;
                    self.write(format!(
                        "    _{array_id} = malloc(_{length_id} * sizeof(*_{array_id}));"
                    ));
                }
                Statement::Print(fmt) => {
                    let format_string = fmt.printf_format(function, "");
                    let mut args = String::new();
                    for segment in &fmt.segments {
                        if let FormatSegment::Arg(arg) = segment {
                            let arg_id = arg.id;
                            write!(&mut args, ", _{arg_id}").unwrap();
                        }
                    }
                    self.write(format!("    printf(\"{format_string}\\n\"{args});"));
                }
                Statement::Return(binding) => {
                    let binding_id = binding.id;
                    self.write(format!("    return _{binding_id};"));
                }
                Statement::StringConstant(binding, value) => {
                    let binding_id = binding.id;
                    self.write(format!("    _{binding_id} = str{value};"));
                }
                Statement::UnaryOperator(result, op, arg) => {
                    let result_id = result.id;
                    let arg_id = arg.id;
                    let op = match op {
                        UnaryOperator::Negate => "-",
                        UnaryOperator::BitNot => "~",
                        UnaryOperator::Not => "!",
                    };
                    self.write(format!("    _{result_id} = {op}_{arg_id};"));
                }
            }
        }
    }

    fn function_signature(&self, function: &Function) -> String {
        let function_return_type = convert_type(&function.return_type);
        let function_name = function.name;
        let mut args = String::new();
        for arg_index in 0..function.args.len() {
            if arg_index > 0 {
                args.push_str(", ");
            }
            let arg_type = convert_type(&function.bindings[arg_index].type_);
            args += &format!("{arg_type} _{arg_index}");
        }
        format!("{function_return_type} {function_name}({args})")
    }

    fn write(&mut self, content: impl AsRef<str>) {
        self.c.push_str(content.as_ref());
        self.c.push('\n');
    }
}

fn convert_type(type_: &Type) -> String {
    match &type_.base {
        BaseType::I32 => "int".to_owned(),
        BaseType::I64 => "long long".to_owned(),
        BaseType::String => "char*".to_owned(),
        BaseType::Array(element_type) => format!("{}*", convert_type(element_type)),
        BaseType::Struct(name) => format!("struct struct{name}"),
    }
}

pub fn make_c(vir: &Program) -> String {
    let mut state = State {
        c: String::new(),
        vir,
        current_function: 0,
    };

    state.prologue();
    state.all_functions();

    state.c
}
