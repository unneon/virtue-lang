mod subprocess;

pub use subprocess::compile_il;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::vir::{BaseType, Binding, Function, Program, Statement, Type};
use qbe::Type::{Byte, Long, SignedByte, UnsignedByte};
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
                Statement::Alloc(pointer, count) => {
                    let element_type = self.vir_func.bindings[pointer.id].type_.dereference();
                    let size = self.temp(Instr::Mul(
                        count.into(),
                        Value::Const(self.byte_size(&element_type) as u64),
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
                    self.assign(left, Instr::Copy(right.into()));
                }
                Statement::AssignmentField(struct_, field, value) => {
                    let struct_type = &self.vir_func.bindings[struct_.id].type_;
                    let struct_id = struct_type.unwrap_struct();
                    let field_offset = self.vir.structs[struct_id].field_offset(*field);
                    let field_pointer = self.temp(Instr::Add(
                        struct_.into(),
                        Value::Const(field_offset as u64),
                    ));
                    self.execute(Instr::Store(Long, field_pointer, value.into()));
                }
                Statement::AssignmentIndex(list, index, right) => {
                    let element_type = self.vir_func.bindings[list.id].type_.dereference();
                    let offset = self.temp(Instr::Mul(
                        index.into(),
                        Value::Const(self.byte_size(&element_type) as u64),
                    ));
                    let left = self.temp(Instr::Add(list.into(), offset));
                    self.execute(Instr::Store(
                        extended_type(&element_type),
                        left,
                        right.into(),
                    ));
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
                    ..
                } => {
                    let function_name = self.vir.functions[*function].name.as_ref();
                    let args = args
                        .iter()
                        .map(|arg| {
                            (
                                abi_type(&self.vir_func.bindings[arg.id].type_, self.aggregates),
                                arg.into(),
                            )
                        })
                        .collect();
                    let instr = Instr::Call(function_name.to_string(), args, None);
                    if let Some(result) = result {
                        self.assign(result, instr)
                    } else {
                        self.execute(instr);
                    }
                }
                Statement::Field(value, struct_, field) => {
                    let struct_type = &self.vir_func.bindings[struct_.id].type_;
                    let struct_id = struct_type.unwrap_struct();
                    let field_offset = self.vir.structs[struct_id].field_offset(*field);
                    let field_pointer = self.temp(Instr::Add(
                        struct_.into(),
                        Value::Const(field_offset as u64),
                    ));
                    self.assign(value, Instr::Load(Long, field_pointer));
                }
                Statement::Index(element, list, index) => {
                    let element_type = self.vir_func.bindings[list.id].type_.dereference();
                    let offset = self.temp(Instr::Mul(
                        index.into(),
                        Value::Const(self.byte_size(&element_type) as u64),
                    ));
                    let right = self.temp(Instr::Add(list.into(), offset));
                    self.assign(element, Instr::Load(load_type(&element_type), right));
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
                Statement::Literal(literal, value) => {
                    self.assign(literal, Instr::Copy(Value::Const(*value as u64)));
                }
                Statement::New(pointer, struct_id) => {
                    let struct_size = self.vir.struct_byte_size(*struct_id);
                    self.assign(pointer, Instr::Alloc8(struct_size as u64));
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
                Statement::StringConstant(pointer, string_id) => {
                    self.assign(
                        pointer,
                        Instr::Copy(Value::Global(format!("string_{string_id}"))),
                    );
                }
                Statement::Syscall(result, args) => {
                    let args = args
                        .iter()
                        .map(|arg| {
                            (
                                extended_type(&self.vir_func.bindings[arg.id].type_),
                                arg.into(),
                            )
                        })
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

    fn temp(&mut self, instr: Instr<'a>) -> Value {
        let temp = self.make_temporary();
        self.assign(temp.clone(), instr);
        temp
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

    fn execute(&mut self, instr: Instr<'a>) {
        self.func.add_instr(instr);
    }

    fn make_temporary(&mut self) -> Value {
        let temp_id = self.temp_counter;
        self.temp_counter += 1;
        Value::Temporary(format!("tmp_{temp_id}"))
    }

    fn byte_size(&self, type_: &Type) -> usize {
        match type_.base {
            BaseType::Struct(struct_id, _) => self.vir.struct_byte_size(struct_id),
            _ => type_.byte_size(),
        }
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
        func: qbe::Function::default(),
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
        if function.type_arg_substitutions.is_none() {
            continue;
        }
        let linkage = if function.exported {
            Linkage::public()
        } else {
            Linkage::private()
        };
        let name = if function.is_main {
            "_start"
        } else {
            function.name.as_ref()
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

fn abi_type<'a>(type_: &Type, aggregates: &'a [qbe::TypeDef<'a>]) -> qbe::Type<'a> {
    match &type_.base {
        BaseType::I64 => Long,
        BaseType::I8 => SignedByte,
        BaseType::Bool => UnsignedByte,
        BaseType::Pointer(_) => Long,
        BaseType::Struct(struct_id, _) => qbe::Type::Aggregate(&aggregates[*struct_id]),
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
