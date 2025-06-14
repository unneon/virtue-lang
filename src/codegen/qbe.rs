mod subprocess;

pub use subprocess::compile_il;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::typecheck::std::{BOOL, I8, I64};
use crate::vir;
use crate::vir::{BaseType, Binding, Function, Program, Statement, Type};
use qbe::Type::{Byte, Long, SignedByte, UnsignedByte};
use qbe::{Cmp, DataDef, DataItem, Instr, Linkage, Value};

struct State<'a> {
    vir: &'a Program<'a>,
    vir_func: &'a Function<'a>,
    il: qbe::Module<'a>,
    aggregates: &'a [Option<qbe::TypeDef<'a>>],
    func: qbe::Function<'a>,
    temp_counter: usize,
}

impl<'a> State<'a> {
    fn block(&mut self, statements: &'a [Statement]) {
        for statement in statements {
            match statement {
                Statement::Alloc(pointer, count) => {
                    let element_type = self.vir_func.bindings[pointer.id].dereference();
                    let size = self.temp(Instr::Mul(
                        count.into(),
                        Value::Const(self.vir.byte_size(&element_type) as u64),
                    ));
                    self.assign(
                        pointer,
                        Instr::Call(
                            "syscall".to_owned(),
                            vec![
                                (Long, Value::Const(9)),
                                (Long, Value::Const(0)),
                                (Long, size),
                                (Long, Value::Const(0x3)),
                                (Long, Value::Const(0x22)),
                                (Long, Value::Const(u64::MAX)),
                                (Long, Value::Const(0)),
                            ],
                            None,
                        ),
                    );
                }
                Statement::Assignment(left, right) => {
                    let right_type = self.value_type(right);
                    if right_type.is_struct() {
                        let byte_size = self.vir.byte_size(&right_type);
                        self.execute(Instr::Blit(right.into(), left.into(), byte_size as u64));
                    } else {
                        self.assign(left, Instr::Copy(right.into()));
                    }
                }
                Statement::AssignmentField(struct_, field, value) => {
                    let struct_type = &self.vir_func.bindings[struct_.id];
                    let struct_id = struct_type.unwrap_struct();
                    let field_type = &self.vir.structs[struct_id].fields[*field];
                    let field_offset = self.vir.field_offset(struct_id, *field);
                    let field_pointer = self.temp(Instr::Add(
                        struct_.into(),
                        Value::Const(field_offset as u64),
                    ));
                    self.store(field_pointer, value, field_type);
                }
                Statement::AssignmentIndex(list, index, right) => {
                    let element_type = self.vir_func.bindings[list.id].dereference();
                    let offset = self.temp(Instr::Mul(
                        index.into(),
                        Value::Const(self.vir.byte_size(&element_type) as u64),
                    ));
                    let pointer = self.temp(Instr::Add(list.into(), offset));
                    self.store(pointer, right, &element_type);
                }
                Statement::BinaryOperator(result, op, left, right) => {
                    let left = left.into();
                    let right = right.into();
                    let instr = match op {
                        BinaryOperator::Add => Instr::Add(left, right),
                        BinaryOperator::Subtract => Instr::Sub(left, right),
                        BinaryOperator::Multiply => Instr::Mul(left, right),
                        BinaryOperator::Divide => Instr::Div(left, right),
                        BinaryOperator::Modulo => Instr::Rem(left, right),
                        BinaryOperator::BitAnd => Instr::And(left, right),
                        BinaryOperator::BitOr => Instr::Or(left, right),
                        BinaryOperator::Xor => Instr::Xor(left, right),
                        BinaryOperator::ShiftLeft => Instr::Shl(left, right),
                        BinaryOperator::ShiftRight => Instr::Shr(left, right),
                        BinaryOperator::Less => Instr::Cmp(Long, Cmp::Slt, left, right),
                        BinaryOperator::LessOrEqual => Instr::Cmp(Long, Cmp::Sle, left, right),
                        BinaryOperator::Greater => Instr::Cmp(Long, Cmp::Sgt, left, right),
                        BinaryOperator::GreaterOrEqual => Instr::Cmp(Long, Cmp::Sge, left, right),
                        BinaryOperator::Equal => Instr::Cmp(Long, Cmp::Eq, left, right),
                        BinaryOperator::NotEqual => Instr::Cmp(Long, Cmp::Ne, left, right),
                        BinaryOperator::LogicAnd => Instr::And(left, right),
                        BinaryOperator::LogicOr => Instr::Or(left, right),
                    };
                    self.assign(result, instr);
                }
                Statement::Call {
                    return_: result,
                    function,
                    args,
                } => {
                    let function_name = self.vir.functions[*function].name.as_ref();
                    let args = args
                        .iter()
                        .map(|arg| (abi_type(&self.value_type(arg), self.aggregates), arg.into()))
                        .collect();
                    let instr = Instr::Call(function_name.to_string(), args, None);
                    if let Some(result) = result {
                        let right_type = &self.vir_func.bindings[result.id];
                        if right_type.is_struct() {
                            let byte_size = self.vir.byte_size(right_type);
                            // The Rust qbe library converts the type to base type instead of ABI types, though I'm
                            // not sure if this isn't an exception in QBE made for struct-returning functions.
                            // TODO: Make a bug report?
                            let temp = self.make_temporary();
                            self.func.blocks.last_mut().unwrap().items.push(
                                qbe::BlockItem::Statement(qbe::Statement::Assign(
                                    temp.clone(),
                                    abi_type(right_type, self.aggregates),
                                    instr,
                                )),
                            );
                            self.execute(Instr::Blit(temp, result.into(), byte_size as u64));
                        } else {
                            self.assign(result, instr);
                        }
                    } else {
                        self.execute(instr);
                    }
                }
                Statement::Field(value, struct_, field) => {
                    let struct_type = &self.vir_func.bindings[struct_.id];
                    let struct_id = struct_type.unwrap_struct();
                    let field_type = &self.vir.structs[struct_id].fields[*field];
                    let field_offset = self.vir.field_offset(struct_id, *field);
                    let field_pointer = self.temp(Instr::Add(
                        struct_.into(),
                        Value::Const(field_offset as u64),
                    ));
                    self.load(value, field_pointer, field_type);
                }
                Statement::Index(element, list, index) => {
                    let element_type = self.vir_func.bindings[list.id].dereference();
                    let offset = self.temp(Instr::Mul(
                        index.into(),
                        Value::Const(self.vir.byte_size(&element_type) as u64),
                    ));
                    let pointer = self.temp(Instr::Add(list.into(), offset));
                    self.load(element, pointer, &element_type);
                }
                Statement::JumpAlways(block) => {
                    self.execute(Instr::Jmp(format!("_{block}")));
                    return;
                }
                Statement::JumpConditional {
                    condition,
                    true_block,
                    false_block,
                } => {
                    self.execute(Instr::Jnz(
                        condition.into(),
                        format!("_{true_block}"),
                        format!("_{false_block}"),
                    ));
                    return;
                }
                Statement::Return(result) => {
                    if !self.vir_func.is_main {
                        self.execute(Instr::Ret(result.as_ref().map(From::from)));
                    } else {
                        self.execute(Instr::Call(
                            "syscall".to_owned(),
                            vec![(Long, Value::Const(60)), (Long, (&result.unwrap()).into())],
                            None,
                        ));
                        self.execute(Instr::Hlt);
                    }
                    return;
                }
                Statement::Syscall(result, args) => {
                    let args = args
                        .iter()
                        .map(|arg| (extended_type(&self.value_type(arg)), arg.into()))
                        .collect();
                    self.assign(result, Instr::Call("syscall".to_owned(), args, None));
                }
                Statement::UnaryOperator(result, op, arg) => {
                    self.assign(
                        result,
                        match op {
                            UnaryOperator::Negate => Instr::Sub(Value::Const(0), arg.into()),
                            UnaryOperator::BitNot => Instr::Xor(arg.into(), Value::Const(u64::MAX)),
                            UnaryOperator::LogicNot => Instr::Xor(arg.into(), Value::Const(1)),
                        },
                    );
                }
            }
        }
    }

    fn store(&mut self, pointer: Value, value: &vir::Value, type_: &Type) {
        if type_.is_struct() {
            let byte_size = self.vir.byte_size(type_);
            self.execute(Instr::Blit(value.into(), pointer, byte_size as u64));
        } else {
            self.execute(Instr::Store(extended_type(type_), pointer, value.into()))
        }
    }

    fn load(&mut self, result: &Binding, pointer: Value, type_: &Type) {
        if type_.is_struct() {
            let byte_size = self.vir.byte_size(type_);
            self.execute(Instr::Blit(pointer, result.into(), byte_size as u64));
        } else {
            self.assign(result, Instr::Load(load_type(type_), pointer));
        }
    }

    fn temp(&mut self, instr: Instr<'a>) -> Value {
        let temp = self.make_temporary();
        self.func.assign_instr(temp.clone(), Long, instr);
        temp
    }

    fn assign(&mut self, left: &Binding, right: Instr<'a>) {
        let type_ = &self.vir_func.bindings[left.id];
        let type_ = abi_type(type_, self.aggregates);
        self.func.assign_instr(left.into(), type_, right);
    }

    fn execute(&mut self, instr: Instr<'a>) {
        self.func.add_instr(instr);
    }

    fn make_temporary(&mut self) -> Value {
        let temp_id = self.temp_counter;
        self.temp_counter += 1;
        Value::Temporary(format!("tmp_{temp_id}"))
    }

    fn value_type(&self, value: &vir::Value) -> Type {
        match value {
            vir::Value::Binding(binding) => self.vir_func.bindings[binding.id].clone(),
            vir::Value::ConstBool(_) => BOOL,
            vir::Value::ConstI64(_) => I64,
            vir::Value::Error => unreachable!(),
            vir::Value::String(_) => I8.pointer(),
        }
    }
}

impl From<&Binding> for Value {
    fn from(value: &Binding) -> Value {
        Value::Temporary(format!("_{}", value.id))
    }
}

impl From<&vir::Value> for Value {
    fn from(value: &vir::Value) -> Value {
        match value {
            vir::Value::Binding(binding) => binding.into(),
            vir::Value::ConstBool(value) => Value::Const(*value as u64),
            vir::Value::ConstI64(value) => Value::Const(*value as u64),
            vir::Value::Error => unreachable!(),
            vir::Value::String(string_id) => Value::Global(format!("string_{string_id}")),
        }
    }
}

pub fn make_il(vir: &Program) -> String {
    let aggregates: Vec<_> = vir
        .structs
        .iter()
        .enumerate()
        .map(|(struct_id, struct_)| {
            if struct_.should_codegen() {
                Some(qbe::TypeDef {
                    name: format!("_{struct_id}"),
                    align: None,
                    items: struct_
                        .fields
                        .iter()
                        .map(|field| (extended_type(field), 1))
                        .collect(),
                })
            } else {
                None
            }
        })
        .collect();

    let mut state = State {
        vir,
        vir_func: &vir.functions[0],
        il: qbe::Module::new(),
        aggregates: &aggregates,
        func: qbe::Function::default(),
        temp_counter: 0,
    };

    for aggregate in aggregates.iter().flatten() {
        // The Rust qbe library requires reference to a TypeDef to construct an aggregate type, but
        // doesn't provide a way to get it once added (it should probably use an index/name
        // instead.) So we keep a copy in memory to pass it to `abi_type`.
        state.il.add_type(aggregate.clone());
    }

    for (string_id, string) in vir.strings.iter().enumerate() {
        state.il.add_data(DataDef::new(
            Linkage::private(),
            format!("string_{string_id}"),
            None,
            vec![(Byte, DataItem::Str(escape_string(string)))],
        ));
    }

    for function in &vir.functions {
        if !function.should_codegen() {
            continue;
        }
        let linkage = if function.exported {
            Linkage::public()
        } else {
            Linkage::private()
        };
        let args = function
            .value_args
            .iter()
            .map(|arg| {
                (
                    abi_type(&function.bindings[arg.binding.id], &aggregates),
                    (&arg.binding).into(),
                )
            })
            .collect();
        state.vir_func = function;
        state.func = qbe::Function::new(
            linkage,
            function.name.as_ref(),
            args,
            match function.return_type.base {
                BaseType::Void => None,
                _ => Some(abi_type(&function.return_type, &aggregates)),
            },
        );
        state.temp_counter = 0;
        for (block_id, block) in function.blocks.iter().enumerate() {
            state.func.add_block(format!("_{block_id}"));
            if block_id == 0 {
                for (id, binding_type) in function.bindings.iter().enumerate() {
                    if id < function.value_args.len() {
                        continue;
                    }
                    if binding_type.is_struct() {
                        let pointer = Binding { id };
                        let struct_size = vir.byte_size(binding_type);
                        state.assign(&pointer, Instr::Alloc8(struct_size as u64));
                    }
                }
            }
            state.block(block);
        }
        state.il.add_function(state.func);
    }

    state.il.to_string()
}

fn extended_type(type_: &Type) -> qbe::Type<'static> {
    match &type_.base {
        BaseType::I64 => Long,
        BaseType::I8 => Byte,
        BaseType::Bool => Byte,
        BaseType::Pointer(_) => Long,
        BaseType::Struct(_, _) => Long,
        BaseType::Void => unreachable!(),
        BaseType::TypeVariable(_) => Long,
        BaseType::Error => unreachable!(),
    }
}

fn abi_type<'a>(type_: &Type, aggregates: &'a [Option<qbe::TypeDef<'a>>]) -> qbe::Type<'a> {
    match &type_.base {
        BaseType::I64 => Long,
        BaseType::I8 => SignedByte,
        BaseType::Bool => UnsignedByte,
        BaseType::Pointer(_) => Long,
        BaseType::Struct(struct_id, _) => {
            qbe::Type::Aggregate(aggregates[*struct_id].as_ref().unwrap())
        }
        BaseType::Void => unreachable!(),
        BaseType::TypeVariable(_) => unreachable!(),
        BaseType::Error => unreachable!(),
    }
}

fn load_type(type_: &Type) -> qbe::Type<'static> {
    match &type_.base {
        BaseType::I64 => Long,
        BaseType::I8 => SignedByte,
        BaseType::Bool => UnsignedByte,
        BaseType::Pointer(_) => Long,
        BaseType::Struct(_, _) => Long,
        BaseType::Void => unreachable!(),
        BaseType::TypeVariable(_) => unreachable!(),
        BaseType::Error => unreachable!(),
    }
}

fn escape_string(text: &[&str]) -> String {
    let mut escaped = String::new();
    for segment in text {
        escaped.push_str(match *segment {
            "\n" => "\\n",
            _ => segment,
        });
    }
    escaped
}
