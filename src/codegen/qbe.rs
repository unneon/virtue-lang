mod subprocess;

pub use subprocess::compile_il;

use crate::ast::BinaryOperator;
use crate::hir;
use crate::hir::{FormatSegment, Statement};
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

#[derive(Eq, PartialEq)]
enum Fallthrough {
    Unreachable,
    Reachable,
}

impl<'a> State<'a> {
    fn block(&mut self, statements: &'a [Statement]) -> Fallthrough {
        for statement in statements {
            match statement {
                Statement::Assignment(left, right) => self.func.assign_instr(
                    Value::Temporary(format!("_{left}")),
                    Long,
                    Instr::Copy(Value::Temporary(format!("_{right}"))),
                ),
                Statement::AssignmentField(object_binding, field, value) => {
                    let field_offset = 8 * field;
                    let field_binding = self.make_temporary();
                    self.func.assign_instr(
                        field_binding.clone(),
                        Long,
                        Instr::Add(
                            Value::Temporary(format!("_{object_binding}")),
                            Value::Const(field_offset as u64),
                        ),
                    );
                    self.func.add_instr(Instr::Store(
                        Long,
                        field_binding,
                        Value::Temporary(format!("_{value}")),
                    ));
                }
                Statement::BinaryOperator(result, op, left, right) => {
                    let left = Value::Temporary(format!("_{left}"));
                    let right = Value::Temporary(format!("_{right}"));
                    self.func.assign_instr(
                        Value::Temporary(format!("_{result}")),
                        Long,
                        match op {
                            BinaryOperator::Add => Instr::Add(left, right),
                            BinaryOperator::Subtract => Instr::Sub(left, right),
                            BinaryOperator::Multiply => Instr::Mul(left, right),
                            BinaryOperator::Divide => Instr::Div(left, right),
                            BinaryOperator::Modulo => Instr::Rem(left, right),
                            BinaryOperator::Less => Instr::Cmp(Long, Cmp::Slt, left, right),
                            BinaryOperator::LessOrEqual => Instr::Cmp(Long, Cmp::Sle, left, right),
                            BinaryOperator::Greater => Instr::Cmp(Long, Cmp::Sgt, left, right),
                            BinaryOperator::GreaterOrEqual => {
                                Instr::Cmp(Long, Cmp::Sge, left, right)
                            }
                            BinaryOperator::Equal => Instr::Cmp(Long, Cmp::Eq, left, right),
                            BinaryOperator::NotEqual => Instr::Cmp(Long, Cmp::Ne, left, right),
                        },
                    );
                }
                Statement::Call(return_binding, function_id, args) => {
                    let return_binding = Value::Temporary(format!("_{return_binding}"));
                    let args = args
                        .iter()
                        .map(|arg| (Long, Value::Temporary(format!("_{arg}"))))
                        .collect();
                    self.func.assign_instr(
                        return_binding.clone(),
                        Long,
                        Instr::Call(
                            self.hir.functions[*function_id].name.to_string(),
                            args,
                            None,
                        ),
                    )
                }
                Statement::Field(binding, object_binding, field) => {
                    let field_offset = 8 * field;
                    let field_binding = self.make_temporary();
                    self.func.assign_instr(
                        field_binding.clone(),
                        Long,
                        Instr::Add(
                            Value::Temporary(format!("_{object_binding}")),
                            Value::Const(field_offset as u64),
                        ),
                    );
                    self.func.assign_instr(
                        Value::Temporary(format!("_{binding}")),
                        Long,
                        Instr::Load(Long, field_binding),
                    );
                }
                Statement::JumpAlways(block) => {
                    self.func.add_instr(Instr::Jmp(format!("_{block}")));
                    return Fallthrough::Unreachable;
                }
                Statement::JumpConditional {
                    condition,
                    true_block,
                    false_block,
                } => {
                    self.func.add_instr(Instr::Jnz(
                        Value::Temporary(format!("_{condition}")),
                        format!("_{true_block}"),
                        format!("_{false_block}"),
                    ));
                    return Fallthrough::Unreachable;
                }
                Statement::Literal(binding, literal) => {
                    self.func.assign_instr(
                        Value::Temporary(format!("_{binding}")),
                        Long,
                        Instr::Copy(Value::Const(*literal as u64)),
                    );
                }
                Statement::New(binding, struct_id) => {
                    let field_count = self.hir.structs[*struct_id].fields.len();
                    let struct_size = 8 * field_count;
                    self.func.assign_instr(
                        Value::Temporary(format!("_{binding}")),
                        Long,
                        Instr::Alloc8(struct_size as u64),
                    );
                }
                Statement::Print(fmt) => {
                    let string_id = format!("string_{}", self.string_counter);
                    self.string_counter += 1;

                    let mut c_fmt = String::new();
                    let mut c_args = Vec::new();

                    c_args.push((Long, Value::Global(string_id.clone())));

                    for segment in fmt {
                        match segment {
                            FormatSegment::Text(text) => c_fmt.push_str(text),
                            FormatSegment::Arg(variable) => {
                                c_fmt.push_str(match self.hir_func.bindings[*variable].type_ {
                                    hir::Type::I64 => "%lld",
                                    hir::Type::I32 => "%d",
                                    hir::Type::String => "%s",
                                    hir::Type::Struct(_) => todo!(),
                                });
                                c_args.push((Long, Value::Temporary(format!("_{variable}"))));
                            }
                        }
                    }

                    c_fmt.push_str("\\n");

                    self.string_constant(c_fmt, Some(string_id));

                    self.func
                        .add_instr(Instr::Call("printf".into(), c_args, None));
                }
                Statement::Return(value) => {
                    self.func
                        .add_instr(Instr::Ret(Some(Value::Temporary(format!("_{value}")))));
                    return Fallthrough::Unreachable;
                }
                Statement::StringConstant(binding, string) => {
                    self.func.assign_instr(
                        Value::Temporary(format!("_{binding}")),
                        Long,
                        Instr::Copy(Value::Global(format!("string_{string}"))),
                    );
                }
            }
        }
        Fallthrough::Reachable
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
                    convert_type(&function.bindings[arg.binding].type_),
                    Value::Temporary(format!("_{}", arg.binding)),
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
            if state.block(block) == Fallthrough::Reachable {
                state.func.add_instr(Instr::Ret(Some(Value::Const(0))));
            }
        }
        state.il.add_function(state.func);
    }

    state.il
}

fn convert_type(type_: &hir::Type) -> qbe::Type<'static> {
    match type_ {
        hir::Type::I64 => Long,
        hir::Type::I32 => Word,
        hir::Type::String => Long,
        hir::Type::Struct(_) => Long,
    }
}
