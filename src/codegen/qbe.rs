mod subprocess;

pub use subprocess::compile_il;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::vir::{BaseType, Binding, FormatSegment, Function, Program, Statement, Type};
use qbe::Type::{Byte, Long, Word};
use qbe::{Cmp, DataDef, DataItem, Instr, Linkage, Value};

struct State<'a> {
    vir: &'a Program<'a>,
    vir_func: &'a Function<'a>,
    il: qbe::Module<'static>,
    func: qbe::Function<'static>,
    string_counter: usize,
    temp_counter: usize,
}

impl<'a> State<'a> {
    fn block(&mut self, statements: &'a [Statement]) {
        for statement in statements {
            match statement {
                Statement::Alloc(binding, count) => {
                    self.assign(binding, Instr::Alloc4(*count as u32));
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
                        convert_type(&element_type),
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
                    let args = args.iter().map(|arg| (Long, arg.into())).collect();
                    let return_ = Instr::Call(function_name.to_string(), args, None);
                    self.assign(return_binding, return_)
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
                    let offset_binding = self.make_temporary();
                    let right_binding = self.make_temporary();
                    self.assign(
                        offset_binding.clone(),
                        Instr::Mul(index_binding.into(), Value::Const(8)),
                    );
                    self.assign(
                        right_binding.clone(),
                        Instr::Add(array_binding.into(), offset_binding),
                    );
                    self.assign(binding, Instr::Load(Long, right_binding));
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
                    let size = self.make_temporary();
                    self.assign(size.clone(), Instr::Mul(length.into(), Value::Const(8)));
                    self.assign(
                        binding,
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
                Statement::Print(fmt) => {
                    for segment in &fmt.segments {
                        match segment {
                            FormatSegment::Text(text) => self.func.add_instr(Instr::Call(
                                "virtue_print_raw".to_owned(),
                                vec![
                                    (Long, Value::Global(format!("string_{text}"))),
                                    (Long, Value::Const(self.vir.string_len(*text) as u64)),
                                ],
                                None,
                            )),
                            FormatSegment::Arg(arg) => {
                                let type_ = &self.vir_func.bindings[arg.id].type_;
                                self.func.add_instr(match type_.base {
                                    BaseType::I64 => Instr::Call(
                                        "virtue_print_int".to_owned(),
                                        vec![(Long, arg.into())],
                                        None,
                                    ),
                                    BaseType::Struct(0) => Instr::Call(
                                        "virtue_print_str".to_owned(),
                                        vec![(Long, arg.into())],
                                        None,
                                    ),
                                    _ => todo!(),
                                });
                            }
                        }
                    }
                }
                Statement::Return(value) => {
                    if !self.vir_func.is_main {
                        self.func.add_instr(Instr::Ret(Some(value.into())));
                    } else {
                        self.func.add_instr(Instr::Call(
                            "syscall".to_owned(),
                            vec![(Long, Value::Const(60)), (Long, value.into())],
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
                                convert_type(&self.vir_func.bindings[arg.id].type_),
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

    fn assign(&mut self, left: impl Into<Value>, right: Instr<'static>) {
        self.func.assign_instr(left.into(), Long, right);
    }

    fn make_temporary(&mut self) -> Value {
        let temp_id = self.temp_counter;
        self.temp_counter += 1;
        Value::Temporary(format!("tmp_{temp_id}"))
    }

    fn string_constant(&mut self, text: &[&str], assignment: Option<String>) -> Value {
        let name = if let Some(assignment) = assignment {
            assignment
        } else {
            let string_id = self.string_counter;
            self.string_counter += 1;
            format!("string_{string_id}")
        };
        let text = escape_string(text);
        self.il.add_data(DataDef::new(
            Linkage::private(),
            name.clone(),
            None,
            vec![(Byte, DataItem::Str(text))],
        ));

        Value::Global(name)
    }
}

impl From<&Binding> for Value {
    fn from(value: &Binding) -> Value {
        Value::Temporary(format!("_{}", value.id))
    }
}

pub fn make_il(vir: &Program) -> qbe::Module<'static> {
    let mut state = State {
        vir,
        vir_func: &vir.functions[0],
        il: qbe::Module::new(),
        func: qbe::Function::new(Linkage::public(), "main", Vec::new(), Some(Word)),
        string_counter: 0,
        temp_counter: 0,
    };

    state.string_counter = vir.strings.len();
    for (string_id, string) in vir.strings.iter().enumerate() {
        state.string_constant(string, Some(format!("string_{string_id}")));
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
            .args
            .iter()
            .map(|arg| {
                (
                    convert_type(&function.bindings[arg.binding.id].type_),
                    (&arg.binding).into(),
                )
            })
            .collect();
        state.vir_func = function;
        state.func = qbe::Function::new(
            linkage,
            name,
            args,
            Some(convert_type(&function.return_type)),
        );
        state.temp_counter = 0;
        for (block_id, block) in function.blocks.iter().enumerate() {
            state.func.add_block(format!("_{block_id}"));
            state.block(block);
        }
        state.il.add_function(state.func);
    }

    state.il
}

fn convert_type(type_: &Type) -> qbe::Type<'static> {
    match &type_.base {
        BaseType::Array(_) => Long,
        BaseType::I64 => Long,
        BaseType::I32 => Word,
        BaseType::I8 => Byte,
        BaseType::PointerI8 => Long,
        BaseType::Struct(_) => Long,
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
