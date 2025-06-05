mod subprocess;

pub use subprocess::compile_il;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::vir::{BaseType, Binding, Function, Program, Statement, Type};
use qbe::Type::{Byte, Long, SignedByte, UnsignedByte, Word};
use qbe::{Cmp, DataDef, DataItem, Instr, Linkage, Value};

struct State<'a> {
    vir: &'a Program<'a>,
    vir_func: &'a Function<'a>,
    il: qbe::Module<'a>,
    aggregates: &'a [qbe::TypeDef<'a>],
    func: qbe::Function<'a>,
    temp_counter: usize,
}

impl<'a> State<'a> {
    fn block(&mut self, statements: &'a [Statement]) {
        for statement in statements {
            match statement {
                Statement::Alloc(binding, count) => {
                    let element_type = self.vir_func.bindings[binding.id].type_.dereference();
                    let size = self.make_temporary();
                    self.assign(
                        size.clone(),
                        Instr::Mul(count.into(), Value::Const(element_type.byte_size() as u64)),
                    );
                    let malloc = self.malloc(size);
                    self.assign(binding, malloc);
                }
                Statement::Assignment(left, right) => {
                    self.assign(left, Instr::Copy(right.into()));
                }
                Statement::AssignmentField(object_binding, field, value) => {
                    let field_offset = 8 * field;
                    let field_binding = self.make_temporary();
                    let field_address =
                        Instr::Add(object_binding.into(), Value::Const(field_offset as u64));
                    self.assign(field_binding.clone(), field_address);
                    self.func
                        .add_instr(Instr::Store(Long, field_binding, value.into()));
                }
                Statement::AssignmentIndex(array_binding, index_binding, right_binding) => {
                    let element_type = self.vir_func.bindings[array_binding.id].type_.dereference();
                    let offset_binding = self.make_temporary();
                    let left_binding = self.make_temporary();
                    self.assign(
                        offset_binding.clone(),
                        Instr::Mul(
                            index_binding.into(),
                            Value::Const(element_type.byte_size() as u64),
                        ),
                    );
                    self.assign(
                        left_binding.clone(),
                        Instr::Add(array_binding.into(), offset_binding),
                    );
                    self.func.add_instr(Instr::Store(
                        extended_type(&element_type),
                        left_binding,
                        right_binding.into(),
                    ));
                }
                Statement::BinaryOperator(result_binding, op, left, right) => {
                    let left = left.into();
                    let right = right.into();
                    let result = match op {
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
                    self.assign(result_binding, result);
                }
                Statement::Call(return_binding, function_id, args) => {
                    let function_name = self.vir.functions[*function_id].name;
                    let args = args
                        .iter()
                        .map(|arg| {
                            (
                                abi_type(&self.vir_func.bindings[arg.id].type_, self.aggregates),
                                arg.into(),
                            )
                        })
                        .collect();
                    let return_ = Instr::Call(function_name.to_string(), args, None);
                    if let Some(return_binding) = return_binding {
                        self.assign(return_binding, return_)
                    } else {
                        self.func.add_instr(return_);
                    }
                }
                Statement::Field(binding, object_binding, field) => {
                    let field_offset = 8 * field;
                    let field_binding = self.make_temporary();
                    let field_address =
                        Instr::Add(object_binding.into(), Value::Const(field_offset as u64));
                    self.assign(field_binding.clone(), field_address);
                    self.assign(binding, Instr::Load(Long, field_binding));
                }
                Statement::Index(binding, array_binding, index_binding) => {
                    let element_type = self.vir_func.bindings[array_binding.id].type_.dereference();
                    let offset_binding = self.make_temporary();
                    let right_binding = self.make_temporary();
                    self.assign(
                        offset_binding.clone(),
                        Instr::Mul(
                            index_binding.into(),
                            Value::Const(element_type.byte_size() as u64),
                        ),
                    );
                    self.assign(
                        right_binding.clone(),
                        Instr::Add(array_binding.into(), offset_binding),
                    );
                    self.assign(
                        binding,
                        Instr::Load(load_type(&element_type), right_binding),
                    );
                }
                Statement::JumpAlways(block) => {
                    self.func.add_instr(Instr::Jmp(format!("_{block}")));
                    return;
                }
                Statement::JumpConditional {
                    condition,
                    true_block,
                    false_block,
                } => {
                    self.func.add_instr(Instr::Jnz(
                        condition.into(),
                        format!("_{true_block}"),
                        format!("_{false_block}"),
                    ));
                    return;
                }
                Statement::Literal(binding, literal) => {
                    self.assign(binding, Instr::Copy(Value::Const(*literal as u64)));
                }
                Statement::New(binding, struct_id) => {
                    let field_count = self.vir.structs[*struct_id].fields.len();
                    let struct_size = 8 * field_count;
                    self.assign(binding, Instr::Alloc8(struct_size as u64));
                }
                Statement::NewArray(binding, length) => {
                    let element_type = self.vir_func.bindings[binding.id].type_.dereference();
                    let size = self.make_temporary();
                    self.assign(
                        size.clone(),
                        Instr::Mul(length.into(), Value::Const(element_type.byte_size() as u64)),
                    );
                    let malloc = self.malloc(size);
                    self.assign(binding, malloc);
                }
                Statement::Return(value) => {
                    if !self.vir_func.is_main {
                        self.func
                            .add_instr(Instr::Ret(value.as_ref().map(From::from)));
                    } else {
                        self.func.add_instr(Instr::Call(
                            "syscall".to_owned(),
                            vec![(Long, Value::Const(60)), (Long, (&value.unwrap()).into())],
                            None,
                        ));
                        self.func.add_instr(Instr::Hlt);
                    }
                    return;
                }
                Statement::StringConstant(binding, string) => {
                    self.assign(binding, Instr::Alloc8(16));
                    let pointer_field = binding;
                    let length_field = self.make_temporary();
                    let length = self.vir.string_len(*string);
                    self.assign(
                        length_field.clone(),
                        Instr::Add(pointer_field.into(), Value::Const(8)),
                    );
                    self.func.add_instr(Instr::Store(
                        Long,
                        pointer_field.into(),
                        Value::Global(format!("string_{string}")),
                    ));
                    self.func.add_instr(Instr::Store(
                        Long,
                        length_field,
                        Value::Const(length as u64),
                    ));
                }
                Statement::Syscall(binding, arg_bindings) => {
                    let args = arg_bindings
                        .iter()
                        .map(|arg| {
                            (
                                extended_type(&self.vir_func.bindings[arg.id].type_),
                                arg.into(),
                            )
                        })
                        .collect();
                    self.assign(binding, Instr::Call("syscall".to_owned(), args, None));
                }
                Statement::UnaryOperator(binding, op, arg) => {
                    self.assign(
                        binding,
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

    fn malloc(&mut self, size: impl Into<Value> + 'static) -> Instr<'static> {
        Instr::Call(
            "syscall".to_owned(),
            vec![
                (Long, Value::Const(9)),
                (Long, Value::Const(0)),
                (Long, size.into()),
                (Long, Value::Const(0x3)),
                (Long, Value::Const(0x22)),
                (Long, Value::Const(u64::MAX)),
                (Long, Value::Const(0)),
            ],
            None,
        )
    }

    fn assign(&mut self, left: impl Into<Value>, right: Instr<'a>) {
        let left = left.into();
        let Value::Temporary(left_name) = &left else {
            unreachable!()
        };
        let type_ = if let Some(left) = left_name.strip_prefix("_") {
            let type_ = &self.vir_func.bindings[left.parse::<usize>().unwrap()].type_;
            abi_type(type_, self.aggregates)
        } else {
            Long
        };
        if matches!(right, Instr::Call(_, _, _)) {
            // The Rust qbe library converts the type to base type instead of ABI types, though I'm
            // not sure if this isn't an exception in QBE made for struct-returning functions.
            // TODO: Make a bug report?
            self.func
                .blocks
                .last_mut()
                .unwrap()
                .items
                .push(qbe::BlockItem::Statement(qbe::Statement::Assign(
                    left, type_, right,
                )))
        } else {
            self.func.assign_instr(left, type_, right);
        }
    }

    fn make_temporary(&mut self) -> Value {
        let temp_id = self.temp_counter;
        self.temp_counter += 1;
        Value::Temporary(format!("tmp_{temp_id}"))
    }
}

impl From<&Binding> for Value {
    fn from(value: &Binding) -> Value {
        Value::Temporary(format!("_{}", value.id))
    }
}

pub fn make_il(vir: &Program) -> String {
    let aggregates: Vec<_> = vir
        .structs
        .iter()
        .enumerate()
        .map(|(struct_id, struct_)| qbe::TypeDef {
            name: format!("_{struct_id}"),
            align: None,
            items: struct_
                .fields
                .iter()
                .map(|field| (extended_type(field), 1))
                .collect(),
        })
        .collect();

    let mut state = State {
        vir,
        vir_func: &vir.functions[0],
        il: qbe::Module::new(),
        aggregates: &aggregates,
        func: qbe::Function::new(Linkage::public(), "main", Vec::new(), Some(Word)),
        temp_counter: 0,
    };

    for aggregate in &aggregates {
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
        let linkage = if function.exported {
            Linkage::public()
        } else {
            Linkage::private()
        };
        let name = if function.is_main {
            "_start"
        } else {
            function.name
        };
        let args = function
            .value_args
            .iter()
            .map(|arg| {
                (
                    abi_type(&function.bindings[arg.binding.id].type_, &aggregates),
                    (&arg.binding).into(),
                )
            })
            .collect();
        state.vir_func = function;
        state.func = qbe::Function::new(
            linkage,
            name,
            args,
            match function.return_type.base {
                BaseType::Void => None,
                _ => Some(abi_type(&function.return_type, &aggregates)),
            },
        );
        state.temp_counter = 0;
        for (block_id, block) in function.blocks.iter().enumerate() {
            state.func.add_block(format!("_{block_id}"));
            state.block(block);
        }
        state.il.add_function(state.func);
    }

    state.il.to_string()
}

fn extended_type(type_: &Type) -> qbe::Type<'static> {
    match &type_.base {
        BaseType::Array(_) => Long,
        BaseType::I64 => Long,
        BaseType::I32 => Word,
        BaseType::I8 => Byte,
        BaseType::Bool => Byte,
        BaseType::PointerI8 => Long,
        BaseType::Struct(_) => Long,
        BaseType::Void => unreachable!(),
        BaseType::TypeVariable(_) => unreachable!(),
        BaseType::Error => unreachable!(),
    }
}

fn abi_type<'a>(type_: &Type, aggregates: &'a [qbe::TypeDef<'a>]) -> qbe::Type<'a> {
    match &type_.base {
        BaseType::Array(_) => Long,
        BaseType::I64 => Long,
        BaseType::I32 => Word,
        BaseType::I8 => SignedByte,
        BaseType::Bool => UnsignedByte,
        BaseType::PointerI8 => Long,
        BaseType::Struct(struct_id) => qbe::Type::Aggregate(&aggregates[*struct_id]),
        BaseType::Void => unreachable!(),
        BaseType::TypeVariable(_) => unreachable!(),
        BaseType::Error => unreachable!(),
    }
}

fn load_type(type_: &Type) -> qbe::Type<'static> {
    match &type_.base {
        BaseType::Array(_) => Long,
        BaseType::I64 => Long,
        BaseType::I32 => Word,
        BaseType::I8 => SignedByte,
        BaseType::Bool => UnsignedByte,
        BaseType::PointerI8 => Long,
        BaseType::Struct(_) => Long,
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
