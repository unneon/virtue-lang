mod subprocess;

pub use subprocess::compile_il;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::hir;
use crate::hir::{Binding, FormatSegment, Statement};
use qbe::Type::{Byte, Long, Word};
use qbe::{Cmp, DataDef, DataItem, Function, Instr, Linkage, Value};

struct State<'a> {
    hir: &'a hir::Program<'a>,
    hir_func: &'a hir::Function<'a>,
    il: qbe::Module<'static>,
    func: Function<'static>,
    string_counter: usize,
    temp_counter: usize,
}

impl<'a> State<'a> {
    fn block(&mut self, statements: &'a [Statement]) {
        for statement in statements {
            match statement {
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
                    let offset_binding = self.make_temporary();
                    let left_binding = self.make_temporary();
                    self.assign(
                        offset_binding.clone(),
                        Instr::Mul(index_binding.into(), Value::Const(8)),
                    );
                    self.assign(
                        left_binding.clone(),
                        Instr::Add(array_binding.into(), offset_binding),
                    );
                    self.func
                        .add_instr(Instr::Store(Long, left_binding, right_binding.into()));
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
                        BinaryOperator::BitXor => Instr::Xor(left, right),
                        BinaryOperator::BitShiftLeft => Instr::Shl(left, right),
                        BinaryOperator::BitShiftRight => Instr::Shr(left, right),
                        BinaryOperator::Less => Instr::Cmp(Long, Cmp::Slt, left, right),
                        BinaryOperator::LessOrEqual => Instr::Cmp(Long, Cmp::Sle, left, right),
                        BinaryOperator::Greater => Instr::Cmp(Long, Cmp::Sgt, left, right),
                        BinaryOperator::GreaterOrEqual => Instr::Cmp(Long, Cmp::Sge, left, right),
                        BinaryOperator::Equal => Instr::Cmp(Long, Cmp::Eq, left, right),
                        BinaryOperator::NotEqual => Instr::Cmp(Long, Cmp::Ne, left, right),
                    };
                    self.assign(result_binding, result);
                }
                Statement::Call(return_binding, function_id, args) => {
                    let function_name = self.hir.functions[*function_id].name;
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
                    let field_count = self.hir.structs[*struct_id].fields.len();
                    let struct_size = 8 * field_count;
                    self.assign(binding, Instr::Alloc8(struct_size as u64));
                }
                Statement::NewArray(binding, _, length) => {
                    let size = self.make_temporary();
                    self.assign(size.clone(), Instr::Mul(length.into(), Value::Const(8)));
                    self.assign(
                        binding,
                        Instr::Call("malloc".into(), vec![(Long, size)], None),
                    );
                }
                Statement::Print(fmt) => {
                    let fmt_printf = fmt.printf_format(self.hir_func, "\\n");
                    let fmt_string_id = self.string_constant(fmt_printf, None);

                    let mut args = vec![(Long, fmt_string_id)];
                    for segment in &fmt.segments {
                        if let FormatSegment::Arg(arg) = segment {
                            args.push((Long, arg.into()));
                        }
                    }

                    self.func
                        .add_instr(Instr::Call("printf".into(), args, None));
                }
                Statement::Return(value) => {
                    self.func.add_instr(Instr::Ret(Some(value.into())));
                    return;
                }
                Statement::StringConstant(binding, string) => {
                    self.assign(
                        binding,
                        Instr::Copy(Value::Global(format!("string_{string}"))),
                    );
                }
                Statement::UnaryOperator(binding, op, arg) => {
                    self.assign(
                        binding,
                        match op {
                            UnaryOperator::Negate => Instr::Sub(Value::Const(0), arg.into()),
                            UnaryOperator::BitNot => Instr::Xor(arg.into(), Value::Const(u64::MAX)),
                            UnaryOperator::Not => Instr::Xor(arg.into(), Value::Const(1)),
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

    fn string_constant(&mut self, text: String, assignment: Option<String>) -> Value {
        let name = if let Some(assignment) = assignment {
            assignment
        } else {
            let string_id = self.string_counter;
            self.string_counter += 1;
            format!("string_{string_id}")
        };

        self.il.add_data(DataDef::new(
            Linkage::private(),
            name.clone(),
            None,
            vec![(Byte, DataItem::Str(text)), (Byte, DataItem::Const(0))],
        ));

        Value::Global(name)
    }
}

impl From<&Binding> for Value {
    fn from(value: &Binding) -> Value {
        Value::Temporary(format!("_{}", value.id))
    }
}

pub fn make_il(hir: &hir::Program) -> qbe::Module<'static> {
    let mut state = State {
        hir,
        hir_func: &hir.functions[0],
        il: qbe::Module::new(),
        func: Function::new(Linkage::public(), "main", Vec::new(), Some(Word)),
        string_counter: 0,
        temp_counter: 0,
    };

    state.string_counter = hir.strings.len();
    for (string_id, string) in hir.strings.iter().enumerate() {
        state.string_constant(string.to_string(), Some(format!("string_{string_id}")));
    }

    for function in &hir.functions {
        let linkage = if function.exported {
            Linkage::public()
        } else {
            Linkage::private()
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
        state.hir_func = function;
        state.func = Function::new(
            linkage,
            function.name,
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

fn convert_type(type_: &hir::Type) -> qbe::Type<'static> {
    match &type_.base {
        hir::BaseType::Array(_) => Long,
        hir::BaseType::I64 => Long,
        hir::BaseType::I32 => Word,
        hir::BaseType::String => Long,
        hir::BaseType::Struct(_) => Long,
    }
}
