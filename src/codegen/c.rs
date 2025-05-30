mod subprocess;

pub use subprocess::compile_c;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::vir::{BaseType, Binding, FormatSegment, Function, Program, Statement, Type};
use std::fmt::Write;

struct State<'a> {
    c: String,
    vir: &'a Program<'a>,
    current_function: usize,
    temporary_counter: usize,
    extended_register_counter: usize,
}

enum Value {
    Binding(Binding),
    Const(i64),
    Temporary(usize),
}

impl State<'_> {
    fn prologue(&mut self) {
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
            self.c += &format!("char str{string_id}[] = \"");
            for string in *string {
                self.c += match *string {
                    "\n" => "\\n",
                    _ => string,
                };
            }
            self.c += "\";\n";
        }
    }

    fn all_functions(&mut self) {
        for function_id in 0..self.vir.functions.len() {
            self.current_function = function_id;
            self.temporary_counter = 0;
            self.extended_register_counter = 0;
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
                Statement::Alloc(binding, count) => {
                    let element_type = function.bindings[binding.id].type_.dereference();
                    self.syscall(
                        Some(binding),
                        &[
                            Value::Const(9),
                            Value::Const(0),
                            Value::Const((count * element_type.byte_size()) as i64),
                            Value::Const(0x3),
                            Value::Const(0x22),
                            Value::Const(-1),
                            Value::Const(0),
                        ],
                    );
                }
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
                        BinaryOperator::Xor => "^",
                        BinaryOperator::ShiftLeft => "<<",
                        BinaryOperator::ShiftRight => ">>",
                        BinaryOperator::Less => "<",
                        BinaryOperator::LessOrEqual => "<=",
                        BinaryOperator::Greater => ">",
                        BinaryOperator::GreaterOrEqual => ">=",
                        BinaryOperator::Equal => "==",
                        BinaryOperator::NotEqual => "!=",
                        BinaryOperator::LogicAnd => "&&",
                        BinaryOperator::LogicOr => "||",
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
                    let size_temp = self.temporary_counter;
                    self.temporary_counter += 1;
                    self.write(format!(
                        "    long long tmp{size_temp} = _{length_id} * sizeof(*_{array_id});"
                    ));
                    self.syscall(
                        Some(array),
                        &[
                            Value::Const(9),
                            Value::Const(0),
                            Value::Temporary(size_temp),
                            Value::Const(0x3),
                            Value::Const(0x22),
                            Value::Const(-1),
                            Value::Const(0),
                        ],
                    );
                }
                Statement::Print(fmt) => {
                    for segment in &fmt.segments {
                        match segment {
                            FormatSegment::Text(text) => {
                                let length = self.vir.string_len(*text);
                                self.write(format!("    virtue_print_raw(str{text}, {length});"));
                            }
                            FormatSegment::Arg(arg) => {
                                let arg_id = arg.id;
                                match function.bindings[arg.id].type_.base {
                                    BaseType::I64 => {
                                        self.write(format!("    virtue_print_int(_{arg_id});"))
                                    }
                                    BaseType::Struct(0) => {
                                        self.write(format!("    virtue_print_str(_{arg_id});"))
                                    }
                                    _ => todo!(),
                                }
                            }
                        }
                    }
                }
                Statement::Return(binding) => {
                    if !function.is_main {
                        let binding_id = binding.id;
                        self.write(format!("    return _{binding_id};"));
                    } else {
                        self.syscall(None, &[Value::Const(60), Value::Binding(*binding)]);
                    }
                }
                Statement::Syscall(result_binding, arg_bindings) => {
                    let args: Vec<_> = arg_bindings.iter().copied().map(Value::Binding).collect();
                    self.syscall(Some(result_binding), &args);
                }
                Statement::StringConstant(binding, value) => {
                    let binding_id = binding.id;
                    let length = self.vir.string_len(*value);
                    self.write(format!("    _{binding_id}._0 = str{value};"));
                    self.write(format!("    _{binding_id}._1 = {length};"));
                }
                Statement::UnaryOperator(result, op, arg) => {
                    let result_id = result.id;
                    let arg_id = arg.id;
                    let op = match op {
                        UnaryOperator::Negate => "-",
                        UnaryOperator::BitNot => "~",
                        UnaryOperator::LogicNot => "!",
                    };
                    self.write(format!("    _{result_id} = {op}_{arg_id};"));
                }
            }
        }
    }

    fn function_signature(&self, function: &Function) -> String {
        let function_return_type = convert_type(&function.return_type);
        let function_name = if function.is_main {
            "_start"
        } else {
            function.name
        };
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

    fn syscall(&mut self, return_binding: Option<&Binding>, arg_bindings: &[Value]) {
        let return_ = if let Some(return_binding) = return_binding {
            let return_id = return_binding.id;
            &format!("\"=a\" (_{return_id})")
        } else {
            ""
        };
        let registers = ["a", "D", "S", "d", "r10", "r8", "r9"];
        assert!(arg_bindings.len() <= registers.len());
        let mut args = String::new();
        for (arg_index, (arg, register)) in arg_bindings.iter().zip(registers).enumerate() {
            if arg_index > 0 {
                args.push_str(", ");
            }
            let arg = match arg {
                Value::Binding(binding) => format!("_{}", binding.id),
                Value::Const(value) => value.to_string(),
                Value::Temporary(temporary) => format!("tmp{temporary}"),
            };
            if arg_index < 4 {
                write!(&mut args, "\"{register}\" ({arg})").unwrap();
            } else {
                let er = self.extended_register_counter;
                self.extended_register_counter += 1;
                self.write(format!(
                    "    register long long r{er} __asm__(\"{register}\") = {arg};"
                ));
                write!(&mut args, "\"r\" (r{er})").unwrap();
            }
        }
        self.write(format!(
            "    asm volatile (\"syscall\" : {return_} : {args} : \"rcx\", \"r11\", \"memory\");"
        ));
    }

    fn write(&mut self, content: impl AsRef<str>) {
        self.c.push_str(content.as_ref());
        self.c.push('\n');
    }
}

fn convert_type(type_: &Type) -> String {
    match &type_.base {
        BaseType::I8 => "signed char".to_owned(),
        BaseType::I32 => "int".to_owned(),
        BaseType::I64 => "long long".to_owned(),
        BaseType::PointerI8 => "signed char*".to_owned(),
        BaseType::Array(element_type) => format!("{}*", convert_type(element_type)),
        BaseType::Struct(name) => format!("struct struct{name}"),
    }
}

pub fn make_c(vir: &Program) -> String {
    let mut state = State {
        c: String::new(),
        vir,
        current_function: 0,
        temporary_counter: 0,
        extended_register_counter: 0,
    };

    state.prologue();
    state.all_functions();

    state.c
}
