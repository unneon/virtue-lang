mod subprocess;

pub use subprocess::compile_ir;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::vir::{BaseType, Binding, Program, Statement, Type};
use std::fmt::Write;

struct State<'a> {
    ir: String,
    vir: &'a Program<'a>,
    current_function: usize,
    temp_counter: usize,
}

impl State<'_> {
    fn prologue_structs(&mut self) {
        for (struct_id, struct_) in self.vir.structs.iter().enumerate() {
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

    fn prologue_strings(&mut self) {
        for (id, text) in self.vir.strings.iter().enumerate() {
            let length = self.vir.string_len(id);
            self.ir += &format!("@string_{id} = internal constant [{length} x i8] c\"");
            for s in *text {
                self.ir += match *s {
                    "\n" => "\\0A",
                    _ => s,
                };
            }
            self.ir += "\"\n";
        }
    }

    fn all_functions(&mut self) {
        for function_id in 0..self.vir.functions.len() {
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
        let function = &self.vir.functions[self.current_function];
        let return_type = convert_type(&function.return_type);
        let name = if function.is_main {
            "_start"
        } else {
            function.name
        };
        let args = self.function_args_declaration();
        self.write(format!("define {return_type} @{name}({args}) {{"));
    }

    fn function_args_declaration(&self) -> String {
        let function = &self.vir.functions[self.current_function];
        let mut decl = String::new();
        for (arg_id, arg) in function.value_args.iter().enumerate() {
            if arg_id > 0 {
                decl.push_str(", ");
            }
            let arg_type = convert_type(&function.bindings[arg.binding.id].type_);
            write!(&mut decl, "{arg_type} %arg_{arg_id}").unwrap();
        }
        decl
    }

    fn function_stack_allocation(&mut self) {
        let function = &self.vir.functions[self.current_function];
        for (binding_id, binding) in function.bindings.iter().enumerate() {
            if binding.type_.base == BaseType::Void {
                continue;
            }
            let binding_type = convert_type(&binding.type_);
            self.write(format!("%stack_{binding_id} = alloca {binding_type}"));
        }
    }

    fn function_args_copy(&mut self) {
        let function = &self.vir.functions[self.current_function];
        for (arg_id, arg) in function.value_args.iter().enumerate() {
            let arg_type = convert_type(&function.bindings[arg.binding.id].type_);
            self.write(format!(
                "store {arg_type} %arg_{arg_id}, {arg_type}* %stack_{arg_id}"
            ));
        }
    }

    fn function_body(&mut self) {
        let function = &self.vir.functions[self.current_function];
        for block_id in 0..function.blocks.len() {
            self.block(block_id);
        }
    }

    fn block(&mut self, block_id: usize) {
        if block_id > 0 {
            self.write(format!("block_{block_id}:"));
        }

        let function = &self.vir.functions[self.current_function];
        let block = &function.blocks[block_id];
        for (statement_id, statement) in block.iter().enumerate() {
            self.write(format!("; statement_id={statement_id} {statement:?}"));
            match statement {
                Statement::Alloc(binding, count) => {
                    let type_ = convert_type(&function.bindings[binding.id].type_.dereference());
                    let temp = self.make_temporary();
                    let count_temp = self.make_temporary();
                    self.load(count_temp, count);
                    self.write(format!(
                        "%temp_{temp} = alloca {type_}, i64 %temp_{count_temp}"
                    ));
                    self.store(binding, temp);
                }
                Statement::Assignment(left, right) => {
                    let temp = self.make_temporary();
                    self.load(temp, right);
                    self.store(left, temp);
                }
                Statement::AssignmentField(object_binding, field_id, value_binding) => {
                    let object_binding_id = object_binding.id;
                    let struct_id = function.bindings[object_binding.id].type_.unwrap_struct();
                    let field_type = convert_type(&self.vir.structs[struct_id].fields[*field_id]);
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
                    let element_type =
                        convert_type(&function.bindings[array_binding.id].type_.dereference());
                    let value_temp = self.make_temporary();
                    let array_temp = self.make_temporary();
                    let index_temp = self.make_temporary();
                    let field_temp = self.make_temporary();
                    self.load(value_temp, value_binding);
                    self.load(array_temp, array_binding);
                    self.load(index_temp, index_binding);
                    self.write(format!(
                        "%temp_{field_temp} = getelementptr {element_type}, ptr %temp_{array_temp}, i64 %temp_{index_temp}"
                    ));
                    if value_type == element_type {
                        self.write(format!(
                            "store {value_type} %temp_{value_temp}, {element_type}* %temp_{field_temp}"
                        ));
                    } else if value_type == "i64" && element_type == "i8" {
                        let trunc_temp = self.make_temporary();
                        self.write(format!(
                            "%temp_{trunc_temp} = trunc i64 %temp_{value_temp} to i8"
                        ));
                        self.write(format!(
                            "store i8 %temp_{trunc_temp}, i8* %temp_{field_temp}"
                        ));
                    } else {
                        todo!()
                    }
                }
                Statement::BinaryOperator(binding, op, left, right) => {
                    let left_type = convert_type(&function.bindings[left.id].type_);
                    let instr = match op {
                        BinaryOperator::Add => "add",
                        BinaryOperator::Subtract => "sub",
                        BinaryOperator::Multiply => "mul",
                        BinaryOperator::Divide => "sdiv",
                        BinaryOperator::Modulo => "srem",
                        BinaryOperator::BitAnd => "and",
                        BinaryOperator::BitOr => "or",
                        BinaryOperator::Xor => "xor",
                        BinaryOperator::ShiftLeft => "shl",
                        BinaryOperator::ShiftRight => "lshr",
                        BinaryOperator::Less => "icmp slt",
                        BinaryOperator::LessOrEqual => "icmp sle",
                        BinaryOperator::Greater => "icmp sgt",
                        BinaryOperator::GreaterOrEqual => "icmp sge",
                        BinaryOperator::Equal => "icmp eq",
                        BinaryOperator::NotEqual => "icmp ne",
                        BinaryOperator::LogicAnd => "and",
                        BinaryOperator::LogicOr => "or",
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
                    if left_type == "i8*" && instr == "add" {
                        let element_type =
                            convert_type(&function.bindings[left.id].type_.dereference());
                        let right_type = convert_type(&function.bindings[right.id].type_);
                        self.write(format!(
                            "%temp_{temp_result} = getelementptr {element_type}, ptr %temp_{temp_left}, {right_type} %temp_{temp_right}"
                        ));
                    } else if left_type == "i8*" && instr == "sub" {
                        let element_type =
                            convert_type(&function.bindings[left.id].type_.dereference());
                        let temp_minus_right = self.make_temporary();
                        let right_type = convert_type(&function.bindings[right.id].type_);
                        self.write(format!(
                            "%temp_{temp_minus_right} = sub {right_type} 0, %temp_{temp_right}"
                        ));
                        self.write(format!(
                                "%temp_{temp_result} = getelementptr {element_type}, ptr %temp_{temp_left}, {right_type} %temp_{temp_minus_right}"
                            ));
                    } else {
                        self.write(format!(
                            "%temp_{temp_result} = {instr} {left_type} %temp_{temp_left}, %temp_{temp_right}"
                        ));
                    }
                    if returns_bool {
                        let temp_zext = self.make_temporary();
                        self.write(format!(
                            "%temp_{temp_zext} = zext i1 %temp_{temp_result} to i8"
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
                    let callee_name = self.vir.functions[*callee_id].name;
                    if let Some(return_binding) = return_binding
                        && function.bindings[return_binding.id].type_.base != BaseType::Void
                    {
                        let return_type = convert_type(&self.vir.functions[*callee_id].return_type);
                        let temp_return = self.make_temporary();
                        self.write(format!(
                            "%temp_{temp_return} = call {return_type} ({signature}) @{callee_name}({args})"
                        ));
                        self.store(return_binding, temp_return);
                    } else {
                        self.write(format!("call void ({signature}) @{callee_name}({args})"));
                    }
                }
                Statement::Field(result_binding, object_binding, field_id) => {
                    let object_binding_id = object_binding.id;
                    let struct_id = function.bindings[object_binding.id].type_.unwrap_struct();
                    let field_type = convert_type(&self.vir.structs[struct_id].fields[*field_id]);
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
                        convert_type(&function.bindings[array_binding.id].type_.dereference());
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
                    self.write(format!("%temp_{temp_i1} = trunc i8 %temp_{temp_i64} to i1"));
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
                Statement::NewArray(binding, length) => {
                    let array_type = &function.bindings[binding.id].type_;
                    let element_type = convert_type(array_type.unwrap_list());
                    let length_temp = self.make_temporary();
                    let result_temp = self.make_temporary();
                    self.load(length_temp, length);
                    self.write(format!(
                        "%temp_{result_temp} = alloca {element_type}, i64 %temp_{length_temp}"
                    ));
                    self.store(binding, result_temp);
                }
                Statement::Return(binding) => {
                    if let Some(binding) = binding {
                        let type_ = convert_type(&function.return_type);
                        let temp = self.make_temporary();
                        self.load(temp, binding);
                        if !function.is_main {
                            self.write(format!("ret {type_} %temp_{temp}"));
                        } else {
                            self.write(format!("call void asm sideeffect \"syscall\", \"{{rax}},{{rdi}}\" (i64 60, {type_} %temp_{temp})"));
                            self.write("unreachable");
                        }
                    } else {
                        self.write("ret void");
                    }
                    return;
                }
                Statement::StringConstant(binding, string_id) => {
                    let binding_id = binding.id;
                    let pointer_temp = self.make_temporary();
                    let pointer_field_temp = self.make_temporary();
                    let length_field_temp = self.make_temporary();
                    let length = self.vir.string_len(*string_id);
                    self.write(format!("%temp_{pointer_temp} = getelementptr [{length} x i8], [{length} x i8]* @string_{string_id}, i32 0, i32 0"));
                    self.write(format!("%temp_{pointer_field_temp} = getelementptr %struct_0, ptr %stack_{binding_id}, i32 0, i32 0"));
                    self.write(format!("%temp_{length_field_temp} = getelementptr %struct_0, ptr %stack_{binding_id}, i32 0, i32 1"));
                    self.write(format!(
                        "store i8* %temp_{pointer_temp}, i8** %temp_{pointer_field_temp}"
                    ));
                    self.write(format!(
                        "store i64 {length}, i64* %temp_{length_field_temp}"
                    ));
                }
                Statement::Syscall(binding, arg_bindings) => {
                    let registers = ["{rax}", "{rdi}", "{rsi}", "{rdx}", "{r10}", "{r8}", "{r9}"]
                        [..arg_bindings.len()]
                        .join(",");
                    let mut args = String::new();
                    for (arg_index, arg_binding) in arg_bindings.iter().enumerate() {
                        if arg_index > 0 {
                            args.push_str(", ");
                        }
                        let type_ = convert_type(&function.bindings[arg_binding.id].type_);
                        let temp = self.make_temporary();
                        self.load(temp, arg_binding);
                        write!(&mut args, "{type_} %temp_{temp}").unwrap();
                    }
                    let result_temp = self.make_temporary();
                    self.write(format!("%temp_{result_temp} = call i64 asm sideeffect \"syscall\", \"=r,{registers}\" ({args})"));
                    self.store(binding, result_temp);
                }
                Statement::UnaryOperator(binding, op, arg) => {
                    let (instr, helper_arg, type_) = match op {
                        UnaryOperator::Negate => ("sub", "0", "i64"),
                        UnaryOperator::BitNot => ("xor", "-1", "i64"),
                        UnaryOperator::LogicNot => ("xor", "1", "i8"),
                    };
                    let arg_temp = self.make_temporary();
                    let binding_temp = self.make_temporary();
                    self.load(arg_temp, arg);
                    self.write(format!(
                        "%temp_{binding_temp} = {instr} {type_} {helper_arg}, %temp_{arg_temp}"
                    ));
                    self.store(binding, binding_temp);
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
            convert_type(&self.vir.functions[self.current_function].bindings[source.id].type_);
        self.write(format!(
            "%temp_{dest} = load {type_}, {type_}* %stack_{source_id}"
        ));
    }

    fn store(&mut self, dest: &Binding, source: usize) {
        let dest_id = dest.id;
        let type_ =
            convert_type(&self.vir.functions[self.current_function].bindings[dest.id].type_);
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
    match &type_.base {
        BaseType::Array(inner) => format!("{}*", convert_type(inner)),
        BaseType::I64 => "i64".to_owned(),
        BaseType::I32 => "i32".to_owned(),
        BaseType::I8 => "i8".to_owned(),
        BaseType::Bool => "i8".to_owned(),
        BaseType::PointerI8 => "i8*".to_owned(),
        BaseType::Struct(id) => format!("%struct_{id}"),
        BaseType::Void => "void".to_owned(),
        BaseType::TypeVariable(_) => unreachable!(),
        BaseType::Error => unreachable!(),
    }
}

pub fn make_ir(vir: &Program) -> String {
    let mut state = State {
        ir: String::new(),
        vir,
        current_function: 0,
        temp_counter: 0,
    };

    state.prologue_structs();
    state.prologue_strings();
    state.all_functions();

    state.ir
}
