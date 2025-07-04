mod subprocess;

pub use subprocess::compile_c;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::vir;
use crate::vir::{BaseType, Binding, Function, Program, Statement, Type};
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
    String(usize),
    Temporary(usize),
}

impl State<'_> {
    fn declare_structs(&mut self) {
        for (struct_id, struct_) in self.vir.structs.iter().enumerate() {
            if struct_.should_codegen() {
                self.write(format!("struct struct{struct_id} {{"));
                for (field_id, field_type) in struct_.fields.iter().enumerate() {
                    let field_type = convert_type(field_type);
                    self.write(format!("    {field_type} _{field_id};"));
                }
                self.write("};");
            }
        }
    }

    fn declare_functions(&mut self) {
        for function in &self.vir.functions {
            if function.should_codegen() {
                let signature = self.function_signature(function);
                self.write(format!("{signature};"));
            }
        }
    }

    fn declare_strings(&mut self) {
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

    fn define_functions(&mut self) {
        for id in 0..self.vir.functions.len() {
            if self.vir.functions[id].should_codegen() {
                self.current_function = id;
                self.temporary_counter = 0;
                self.extended_register_counter = 0;
                self.function();
            }
        }
    }

    fn function(&mut self) {
        let function = &self.vir.functions[self.current_function];
        let signature = self.function_signature(function);
        self.write(format!("{signature} {{"));
        for (binding_id, type_) in function.bindings.iter().enumerate() {
            if binding_id >= function.value_args.len() && !type_.is_void() {
                let type_ = convert_type(type_);
                self.write(format!("    {type_} _{binding_id};"));
            }
        }
        for block_id in 0..function.blocks.len() {
            self.block(block_id);
        }
        self.write("}");
    }

    fn block(&mut self, block_id: usize) {
        self.write(format!("b{block_id}:"));

        for statement in &self.vir.functions[self.current_function].blocks[block_id] {
            self.statement(statement);
        }
    }

    fn statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Alloc(pointer, length) => {
                let pointer_id = pointer.id;
                let length = convert_value(length);
                let size = self.make_temporary();
                self.write(format!(
                    "    long long tmp{size} = {length} * sizeof(*_{pointer_id});"
                ));
                self.malloc(pointer, Value::Temporary(size));
            }
            Statement::Assignment(left, right) => {
                if !self.vir.functions[self.current_function].bindings[left.id].is_void() {
                    let left_id = left.id;
                    let right = convert_value(right);
                    self.write(format!("    _{left_id} = {right};"));
                }
            }
            Statement::AssignmentField(object, field, value) => {
                let object_id = object.id;
                let value = convert_value(value);
                self.write(format!("    _{object_id}._{field} = {value};"));
            }
            Statement::AssignmentIndex(list, index, value) => {
                let list_id = list.id;
                let index = convert_value(index);
                let value = convert_value(value);
                self.write(format!("    _{list_id}[{index}] = {value};"));
            }
            Statement::BinaryOperator(result, op, left, right) => {
                let result_id = result.id;
                let left = convert_value(left);
                let right = convert_value(right);
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
                self.write(format!("    _{result_id} = {left} {op} {right};"));
            }
            Statement::Call {
                return_: result,
                function,
                args,
            } => {
                let result_assignment = if let Some(result) = result {
                    let result_id = result.id;
                    format!("_{result_id} = ")
                } else {
                    String::new()
                };
                let callee_name = self.vir.functions[*function].name.as_ref();
                let mut c_args = String::new();
                for (arg_index, arg) in args.iter().enumerate() {
                    if arg_index > 0 {
                        c_args += ", ";
                    }
                    let arg = convert_value(arg);
                    c_args += &arg.to_string();
                }
                self.write(format!("    {result_assignment}{callee_name}({c_args});"));
            }
            Statement::Field(result, object, field) => {
                let result_id = result.id;
                let object_id = object.id;
                self.write(format!("    _{result_id} = _{object_id}._{field};"));
            }
            Statement::Index(result, list, index) => {
                let result_id = result.id;
                let list_id = list.id;
                let index = convert_value(index);
                self.write(format!("    _{result_id} = _{list_id}[{index}];"));
            }
            Statement::JumpAlways(target_block) => {
                self.write(format!("    goto b{target_block};"));
            }
            Statement::JumpConditional {
                condition,
                true_block,
                false_block,
            } => {
                let condition = convert_value(condition);
                self.write(format!("    if ({condition}) goto b{true_block};"));
                self.write(format!("    else goto b{false_block};"));
            }
            Statement::Return(value) => {
                if !self.vir.functions[self.current_function].is_main {
                    if let Some(value) = value {
                        let value = convert_value(value);
                        self.write(format!("    return {value};"));
                    } else {
                        self.write("    return;");
                    }
                } else {
                    self.syscall(None, &[Value::Const(60), value.unwrap().into()]);
                }
            }
            Statement::Syscall(result, args) => {
                let args: Vec<_> = args.iter().copied().map(Value::from).collect();
                self.syscall(Some(result), &args);
            }
            Statement::UnaryOperator(result, op, arg) => {
                let result_id = result.id;
                let arg = convert_value(arg);
                let op = match op {
                    UnaryOperator::Negate => "-",
                    UnaryOperator::BitNot => "~",
                    UnaryOperator::LogicNot => "!",
                };
                self.write(format!("    _{result_id} = {op}{arg};"));
            }
        }
    }

    fn function_signature(&self, function: &Function) -> String {
        let return_type = convert_type(&function.return_type);
        let name = function.name.as_ref();
        let mut args = String::new();
        for arg_index in 0..function.value_args.len() {
            if arg_index > 0 {
                args += ", ";
            }
            let arg_type = convert_type(&function.bindings[arg_index]);
            args += &format!("{arg_type} _{arg_index}");
        }
        format!("{return_type} {name}({args})")
    }

    fn malloc(&mut self, list: &Binding, size: Value) {
        self.syscall(
            Some(list),
            &[
                Value::Const(9),
                Value::Const(0),
                size,
                Value::Const(0x3),
                Value::Const(0x22),
                Value::Const(-1),
                Value::Const(0),
            ],
        );
    }

    fn syscall(&mut self, result: Option<&Binding>, arg_bindings: &[Value]) {
        let result = if let Some(result) = result {
            let result_id = result.id;
            &format!("\"=a\" (_{result_id})")
        } else {
            ""
        };
        let registers = ["a", "D", "S", "d", "r10", "r8", "r9"];
        assert!(arg_bindings.len() <= registers.len());
        let mut args = String::new();
        for (arg_index, (arg, register)) in arg_bindings.iter().zip(registers).enumerate() {
            if arg_index > 0 {
                args += ", ";
            }
            let arg = match arg {
                Value::Binding(binding) => format!("_{}", binding.id),
                Value::Const(value) => value.to_string(),
                Value::String(string_id) => format!("str{string_id}"),
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
            "    asm volatile (\"syscall\" : {result} : {args} : \"rcx\", \"r11\", \"memory\");"
        ));
    }

    fn make_temporary(&mut self) -> usize {
        let temporary = self.temporary_counter;
        self.temporary_counter += 1;
        temporary
    }

    fn write(&mut self, content: impl AsRef<str>) {
        self.c += content.as_ref();
        self.c += "\n";
    }
}

impl From<vir::Value> for Value {
    fn from(value: vir::Value) -> Self {
        match value {
            vir::Value::Binding(binding) => Value::Binding(binding),
            vir::Value::ConstBool(value) => Value::Const(value as i64),
            vir::Value::ConstI64(value) => Value::Const(value),
            vir::Value::Error => unreachable!(),
            vir::Value::String(string_id) => Value::String(string_id),
        }
    }
}

fn convert_value(value: &vir::Value) -> String {
    match value {
        vir::Value::Binding(binding) => format!("_{}", binding.id),
        vir::Value::ConstBool(value) => (*value as i64).to_string(),
        vir::Value::ConstI64(value) => value.to_string(),
        vir::Value::Error => unreachable!(),
        vir::Value::String(string_id) => format!("str{string_id}"),
    }
}

fn convert_type(type_: &Type) -> String {
    match &type_.base {
        BaseType::I64 => "long long".to_owned(),
        BaseType::I8 => "signed char".to_owned(),
        BaseType::Bool => "unsigned char".to_owned(),
        BaseType::Pointer(inner) => format!("{}*", convert_type(inner)),
        BaseType::Struct(name, _) => format!("struct struct{name}"),
        BaseType::Void => "void".to_owned(),
        BaseType::TypeVariable(_) => unreachable!(),
        BaseType::Error => unreachable!(),
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

    state.declare_structs();
    state.declare_functions();
    state.declare_strings();
    state.define_functions();

    state.c
}
