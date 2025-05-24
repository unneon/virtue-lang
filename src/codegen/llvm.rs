mod subprocess;

pub use subprocess::compile_ir;

use crate::ast::BinaryOperator;
use crate::hir::{Binding, FormatSegment, Program, Statement, Type};
use std::fmt::Write;

struct State<'a> {
    ir: String,
    hir: &'a Program<'a>,
    current_function: usize,
    temp_counter: usize,
}

impl State<'_> {
    fn prologue_structs(&mut self) {
        for (struct_id, struct_) in self.hir.structs.iter().enumerate() {
            let mut fields = String::new();
            for (field_id, field) in struct_.fields.iter().enumerate() {
                if field_id > 0 {
                    fields += ", ";
                }
                fields += &convert_type(field);
            }
            self.write(format!("%struct_{struct_id} = type {{ {fields} }}"));
        }
    }

    fn prologue_extern(&mut self) {
        self.write("declare i32 @printf(i8*, ...)");
    }

    fn prologue_strings(&mut self) {
        for (string_id, string) in self.hir.strings.iter().enumerate() {
            let string_len = string.len() + 1;
            self.write(format!(
                "@string_{string_id} = internal constant [{string_len} x i8] c\"{string}\\00\""
            ));
        }
    }

    fn prologue_format(&mut self) {
        for (function_id, function) in self.hir.functions.iter().enumerate() {
            for (block_id, block) in function.blocks.iter().enumerate() {
                for (statement_id, statement) in block.iter().enumerate() {
                    if let Statement::Print(fmt) = statement {
                        let fmt = fmt.printf_format(function, "\\0A\\00");
                        let fmt_len = fmt.len() - 4;
                        self.write(format!("@fmt_{function_id}_{block_id}_{statement_id} = internal constant [{fmt_len} x i8] c\"{fmt}\""));
                    }
                }
            }
        }
    }

    fn all_functions(&mut self) {
        for function_id in 0..self.hir.functions.len() {
            self.current_function = function_id;
            self.function();
        }
    }

    fn function(&mut self) {
        self.function_declaration();
        self.function_stack_allocation();
        self.function_args_copy();
        self.function_body();
        self.function_epilogue();
    }

    fn function_declaration(&mut self) {
        let function = &self.hir.functions[self.current_function];
        let return_type = convert_type(&function.return_type);
        let name = function.name;
        let args = self.function_args_declaration();
        self.write(format!("define {return_type} @{name}({args}) {{"));
    }

    fn function_args_declaration(&self) -> String {
        let function = &self.hir.functions[self.current_function];
        let mut decl = String::new();
        for (arg_id, arg) in function.args.iter().enumerate() {
            if arg_id > 0 {
                decl.push_str(", ");
            }
            let arg_type = convert_type(&function.bindings[arg.binding.id].type_);
            write!(&mut decl, "{arg_type} %arg_{arg_id}").unwrap();
        }
        decl
    }

    fn function_stack_allocation(&mut self) {
        let function = &self.hir.functions[self.current_function];
        for (binding_id, binding) in function.bindings.iter().enumerate() {
            let binding_type = convert_type(&binding.type_);
            self.write(format!("%stack_{binding_id} = alloca {binding_type}"));
        }
    }

    fn function_args_copy(&mut self) {
        let function = &self.hir.functions[self.current_function];
        for (arg_id, arg) in function.args.iter().enumerate() {
            let arg_type = convert_type(&function.bindings[arg.binding.id].type_);
            self.write(format!(
                "store {arg_type} %arg_{arg_id}, {arg_type}* %stack_{arg_id}"
            ));
        }
    }

    fn function_body(&mut self) {
        let function = &self.hir.functions[self.current_function];
        for block_id in 0..function.blocks.len() {
            self.block(block_id);
        }
    }

    fn block(&mut self, block_id: usize) {
        if block_id > 0 {
            self.write(format!("block_{block_id}:"));
        }

        let function = &self.hir.functions[self.current_function];
        let block = &function.blocks[block_id];
        for (statement_id, statement) in block.iter().enumerate() {
            self.write(format!("; statement_id={statement_id} {statement:?}"));
            match statement {
                Statement::Assignment(left, right) => {
                    let temp = self.make_temporary();
                    self.load(temp, right);
                    self.store(left, temp);
                }
                Statement::AssignmentField(object_binding, field_id, value_binding) => {
                    let object_binding_id = object_binding.id;
                    let struct_id = function.bindings[object_binding.id].type_.unwrap_struct();
                    let field_type = convert_type(&self.hir.structs[struct_id].fields[*field_id]);
                    let value_temp = self.make_temporary();
                    let field_temp = self.make_temporary();
                    self.load(value_temp, value_binding);
                    self.write(format!(
                        "%temp_{field_temp} = getelementptr inbounds %struct_{struct_id}, ptr %stack_{object_binding_id}, i32 0, i32 {field_id}"
                    ));
                    self.write(format!(
                        "store {field_type} %temp_{value_temp}, {field_type}* %temp_{field_temp}"
                    ));
                }
                Statement::AssignmentIndex(array_binding, index_binding, value_binding) => {
                    let value_type = convert_type(&function.bindings[value_binding.id].type_);
                    let value_temp = self.make_temporary();
                    let array_temp = self.make_temporary();
                    let index_temp = self.make_temporary();
                    let field_temp = self.make_temporary();
                    self.load(value_temp, value_binding);
                    self.load(array_temp, array_binding);
                    self.load(index_temp, index_binding);
                    self.write(format!(
                        "%temp_{field_temp} = getelementptr {value_type}, ptr %temp_{array_temp}, i64 %temp_{index_temp}"
                    ));
                    self.write(format!(
                        "store {value_type} %temp_{value_temp}, {value_type}* %temp_{field_temp}"
                    ));
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
                    let temp_left = self.make_temporary();
                    let temp_right = self.make_temporary();
                    let temp_result = self.make_temporary();
                    self.load(temp_left, left);
                    self.load(temp_right, right);
                    self.write(format!(
                        "%temp_{temp_result} = {instr} i64 %temp_{temp_left}, %temp_{temp_right}"
                    ));
                    if returns_bool {
                        let temp_zext = self.make_temporary();
                        self.write(format!(
                            "%temp_{temp_zext} = zext i1 %temp_{temp_result} to i64"
                        ));
                        self.store(binding, temp_zext);
                    } else {
                        self.store(binding, temp_result);
                    }
                }
                Statement::Call(return_binding, callee_id, arg_bindings) => {
                    let mut signature = String::new();
                    let mut args = String::new();
                    for (arg_index, arg_binding) in arg_bindings.iter().enumerate() {
                        let type_ = convert_type(&function.bindings[arg_binding.id].type_);
                        if arg_index > 0 {
                            signature.push_str(", ");
                        }
                        signature.push_str(&type_);
                        let temp = self.make_temporary();
                        self.load(temp, arg_binding);
                        if arg_index > 0 {
                            args.push_str(", ");
                        }
                        write!(&mut args, "{type_} %temp_{temp}").unwrap();
                    }
                    let temp_return = self.make_temporary();
                    let callee_name = self.hir.functions[*callee_id].name;
                    self.write(format!(
                        "%temp_{temp_return} = call i64 ({signature}) @{callee_name}({args})"
                    ));
                    self.store(return_binding, temp_return);
                }
                Statement::Field(result_binding, object_binding, field_id) => {
                    let object_binding_id = object_binding.id;
                    let struct_id = function.bindings[object_binding.id].type_.unwrap_struct();
                    let field_type = convert_type(&self.hir.structs[struct_id].fields[*field_id]);
                    let result_temp = self.make_temporary();
                    let field_temp = self.make_temporary();
                    self.write(format!(
                        "%temp_{field_temp} = getelementptr inbounds %struct_{struct_id}, ptr %stack_{object_binding_id}, i32 0, i32 {field_id}"
                    ));
                    self.write(format!(
                        "%temp_{result_temp} = load {field_type}, {field_type}* %temp_{field_temp}"
                    ));
                    self.store(result_binding, result_temp);
                }
                Statement::Index(result_binding, array_binding, index_binding) => {
                    let value_type =
                        convert_type(function.bindings[array_binding.id].type_.unwrap_list());
                    let array_temp = self.make_temporary();
                    let index_temp = self.make_temporary();
                    let field_temp = self.make_temporary();
                    let result_temp = self.make_temporary();
                    self.load(array_temp, array_binding);
                    self.load(index_temp, index_binding);
                    self.write(format!(
                        "%temp_{field_temp} = getelementptr {value_type}, ptr %temp_{array_temp}, i64 %temp_{index_temp}"
                    ));
                    self.write(format!(
                        "%temp_{result_temp} = load {value_type}, {value_type}* %temp_{field_temp}"
                    ));
                    self.store(result_binding, result_temp);
                }
                Statement::JumpAlways(block) => {
                    self.write(format!(
                        "br i1 1, label %block_{block}, label %block_{block}"
                    ));
                    return;
                }
                Statement::JumpConditional {
                    condition,
                    true_block,
                    false_block,
                } => {
                    let temp_i64 = self.make_temporary();
                    let temp_i1 = self.make_temporary();
                    self.load(temp_i64, condition);
                    self.write(format!(
                        "%temp_{temp_i1} = trunc i64 %temp_{temp_i64} to i1"
                    ));
                    self.write(format!(
                        "br i1 %temp_{temp_i1}, label %block_{true_block}, label %block_{false_block}"
                    ));
                    return;
                }
                Statement::Literal(binding, literal) => {
                    let binding_id = binding.id;
                    let type_ = convert_type(&function.bindings[binding_id].type_);
                    self.write(format!(
                        "store {type_} {literal}, {type_}* %stack_{binding_id}"
                    ));
                }
                Statement::New(_, _) => {}
                Statement::NewArray(binding, type_, length) => {
                    let type_ = convert_type(type_);
                    let temp = self.make_temporary();
                    self.write(format!("%temp_{temp} = alloca {type_}, i64 {length}"));
                    self.store(binding, temp);
                }
                Statement::Print(fmt) => {
                    let function_id = self.current_function;
                    let mut args = String::new();
                    for segment in &fmt.segments {
                        if let FormatSegment::Arg(arg) = segment {
                            let type_ = convert_type(&function.bindings[arg.id].type_);
                            let temp = self.make_temporary();
                            self.load(temp, arg);
                            write!(&mut args, ", {type_} %temp_{temp}").unwrap();
                        }
                    }
                    self.write(format!("call i32 (i8*, ...) @printf(i8* @fmt_{function_id}_{block_id}_{statement_id}{args})"));
                }
                Statement::Return(binding) => {
                    let type_ = convert_type(&function.return_type);
                    let temp = self.make_temporary();
                    self.load(temp, binding);
                    self.write(format!("ret {type_} %temp_{temp}"));
                    return;
                }
                Statement::StringConstant(binding, string_id) => {
                    let string_len = self.hir.strings[*string_id].len() + 1;
                    let temp = self.make_temporary();
                    self.write(format!("%temp_{temp} = getelementptr [{string_len} x i8], [{string_len} x i8]* @string_{string_id}, i32 0, i32 0"));
                    self.store(binding, temp);
                }
            }
        }
    }

    fn function_epilogue(&mut self) {
        self.write("}");
    }

    fn load(&mut self, dest: usize, source: &Binding) {
        let source_id = source.id;
        let type_ =
            convert_type(&self.hir.functions[self.current_function].bindings[source.id].type_);
        self.write(format!(
            "%temp_{dest} = load {type_}, {type_}* %stack_{source_id}"
        ));
    }

    fn store(&mut self, dest: &Binding, source: usize) {
        let dest_id = dest.id;
        let type_ =
            convert_type(&self.hir.functions[self.current_function].bindings[dest.id].type_);
        self.write(format!(
            "store {type_} %temp_{source}, {type_}* %stack_{dest_id}"
        ));
    }

    fn write(&mut self, line: impl AsRef<str>) {
        let line = line.as_ref();
        if !line.starts_with("%struct_")
            && !line.starts_with("declare")
            && !line.starts_with("@")
            && !line.starts_with("define")
            && !line.starts_with("}")
        {
            self.ir += "    ";
        }
        self.ir += line;
        self.ir += "\n";
    }

    fn make_temporary(&mut self) -> usize {
        let temp = self.temp_counter;
        self.temp_counter += 1;
        temp
    }
}

fn convert_type(type_: &Type) -> String {
    match type_ {
        Type::Array(inner) => format!("{}*", convert_type(inner)),
        Type::I64 => "i64".to_owned(),
        Type::I32 => "i32".to_owned(),
        Type::String => "i8*".to_owned(),
        Type::Struct(id) => format!("%struct_{id}"),
    }
}

pub fn make_ir(hir: &Program) -> String {
    let mut state = State {
        ir: String::new(),
        hir,
        current_function: 0,
        temp_counter: 0,
    };

    state.prologue_structs();
    state.prologue_extern();
    state.prologue_strings();
    state.prologue_format();
    state.all_functions();

    state.ir
}
