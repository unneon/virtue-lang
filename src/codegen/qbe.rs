mod subprocess;

pub use subprocess::compile_il;

use crate::ast::BinaryOperator;
use crate::hir;
use crate::hir::{FormatSegment, Statement};
use qbe::Type::{Byte, Long, Word};
use qbe::{Cmp, DataDef, DataItem, Function, Instr, Linkage, Value};
use std::collections::HashMap;

struct State<'a> {
    hir: &'a hir::Program<'a>,
    hir_func: &'a hir::Function<'a>,
    il: qbe::Module<'static>,
    func: Function<'static>,
    functions: Vec<(&'a str, &'a [(&'a str, &'a str)], &'a [Statement<'a>])>,
    types: HashMap<&'a str, Type<'a>>,
    variables: HashMap<&'a str, Variable<'a>>,
    block_counter: usize,
    temp_counter: usize,
    string_counter: usize,
}

struct Type<'a> {
    size: usize,
    fields: HashMap<&'a str, usize>,
}

struct Variable<'a> {
    type_: &'a str,
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
                Statement::Assignment(left, right) => {
                    self.func.assign_instr(
                        Value::Temporary(format!("_{left}")),
                        Long,
                        Instr::Copy(Value::Temporary(format!("_{right}"))),
                    )
                    // Expression::Variable(variable) => {
                    //     self.expression(right, Some(variable));
                    //     self.variables.insert(
                    //         *variable,
                    //         Variable {
                    //             type_: self.type_(right),
                    //         },
                    //     );
                    // }
                    // Expression::Field(object, field) => {
                    //     let temporary = self.expression(right, None);
                    //     let field_offset = self.types[self.type_(object)].fields[field];
                    //     let field = self.expression(
                    //         &Expression::add(
                    //             object.as_ref().clone(),
                    //             Expression::Literal(field_offset as i64),
                    //         ),
                    //         None,
                    //     );
                    //     self.func.add_instr(Instr::Store(Long, field, temporary));
                    // }
                    //     _ => todo!("unsupported assignment to {left:?}"),
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
                // Statement::If {
                //     condition,
                //     true_block,
                //     false_block,
                // } => {
                //     let true_block_id = format!("_{}", self.block_counter);
                //     let false_block_id = format!("_{}", self.block_counter + 1);
                //     let after_block_id = format!("_{}", self.block_counter + 2);
                //     self.block_counter += 3;
                //
                //     let condition = self.expression(condition, None);
                //     self.func.add_instr(Instr::Jnz(
                //         condition,
                //         true_block_id.clone(),
                //         false_block_id.clone(),
                //     ));
                //
                //     self.func.add_block(true_block_id);
                //     if self.block(true_block) == Fallthrough::Reachable {
                //         self.func.add_instr(Instr::Jmp(after_block_id.clone()));
                //     }
                //
                //     self.func.add_block(false_block_id);
                //     if self.block(false_block) == Fallthrough::Reachable {
                //         self.func.add_instr(Instr::Jmp(after_block_id.clone()));
                //     }
                //
                //     self.func.add_block(after_block_id);
                // }
                Statement::Literal(binding, literal) => {
                    self.func.assign_instr(
                        Value::Temporary(format!("_{binding}")),
                        Long,
                        Instr::Copy(Value::Const(*literal as u64)),
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
                                c_fmt.push_str(
                                    match self.hir_func.bindings[*variable].type_.as_ref().unwrap()
                                    {
                                        hir::Type::I64 => "%lld",
                                        hir::Type::I32 => "%d",
                                        hir::Type::String => "%s",
                                        hir::Type::Struct(_) => todo!(),
                                    },
                                );
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
                // Statement::Struct { name, fields } => {
                //     let size = 8 * fields.len();
                //     let fields = fields
                //         .iter()
                //         .enumerate()
                //         .map(|(i, (name, _))| (*name, i * 8))
                //         .collect();
                //     self.types.insert(name, Type { size, fields });
                // }
                // Statement::While { condition, body } => {
                //     let condition_block = format!("_{}", self.block_counter);
                //     let body_block = format!("_{}", self.block_counter + 1);
                //     let after_block = format!("_{}", self.block_counter + 2);
                //     self.block_counter += 3;
                //
                //     self.func.add_instr(Instr::Jmp(condition_block.clone()));
                //
                //     self.func.add_block(condition_block.clone());
                //     let condition = self.expression(condition, None);
                //     self.func.add_instr(Instr::Jnz(
                //         condition,
                //         body_block.clone(),
                //         after_block.clone(),
                //     ));
                //
                //     self.func.add_block(body_block);
                //     if self.block(body) == Fallthrough::Reachable {
                //         self.func.add_instr(Instr::Jmp(condition_block));
                //     }
                //
                //     self.func.add_block(after_block);
                // }
                _ => todo!("qbe statement not implemented {statement:?}"),
            }
        }
        Fallthrough::Reachable
    }

    // fn expression(&mut self, expression: &Expression<'a>, assignment: Option<&'a str>) -> Value {
    //     match expression {
    //         Expression::BinaryOperation(op, args) => {
    //             let (left, right) = args.as_ref();
    //             let left = self.expression(left, None);
    //             let right = self.expression(right, None);
    //             let temp = self.get_temporary(assignment);
    //             let instr = match op {
    //                 BinaryOperator::Add => Instr::Add(left, right),
    //                 BinaryOperator::Subtract => Instr::Sub(left, right),
    //                 BinaryOperator::Multiply => Instr::Mul(left, right),
    //                 BinaryOperator::Divide => Instr::Div(left, right),
    //                 BinaryOperator::Modulo => Instr::Rem(left, right),
    //                 BinaryOperator::Less => Instr::Cmp(Long, Cmp::Slt, left, right),
    //                 BinaryOperator::LessOrEqual => Instr::Cmp(Long, Cmp::Sle, left, right),
    //                 BinaryOperator::Greater => Instr::Cmp(Long, Cmp::Sgt, left, right),
    //                 BinaryOperator::GreaterOrEqual => Instr::Cmp(Long, Cmp::Sge, left, right),
    //                 BinaryOperator::Equal => Instr::Cmp(Long, Cmp::Eq, left, right),
    //                 BinaryOperator::NotEqual => Instr::Cmp(Long, Cmp::Ne, left, right),
    //             };
    //             self.func.assign_instr(temp.clone(), Long, instr);
    //             temp
    //         }
    //         Expression::Call(func, args) => {
    //             let temp = self.get_temporary(assignment);
    //             let args = args
    //                 .iter()
    //                 .map(|arg| (Long, self.expression(arg, None)))
    //                 .collect();
    //             self.func.assign_instr(
    //                 temp.clone(),
    //                 Long,
    //                 Instr::Call(func.to_string(), args, None),
    //             );
    //             temp
    //         }
    //         Expression::Field(object, field) => {
    //             let field_offset = self.types[self.type_(object)].fields[field];
    //             let temp = self.get_temporary(assignment);
    //             let field = self.expression(
    //                 &Expression::add(
    //                     object.as_ref().clone(),
    //                     Expression::Literal(field_offset as i64),
    //                 ),
    //                 None,
    //             );
    //             self.func
    //                 .assign_instr(temp.clone(), Long, Instr::Load(Long, field));
    //             temp
    //         }
    //         Expression::Literal(literal) => {
    //             let value = Value::Const(*literal as u64);
    //             if let Some(assignment) = assignment {
    //                 self.func.assign_instr(
    //                     Value::Temporary(assignment.to_string()),
    //                     Long,
    //                     Instr::Copy(value.clone()),
    //                 );
    //             }
    //             value
    //         }
    //         Expression::New(type_) => {
    //             self.variables
    //                 .insert(assignment.unwrap(), Variable { type_ });
    //             let temp = self.get_temporary(assignment);
    //             self.func.assign_instr(
    //                 temp.clone(),
    //                 Long,
    //                 Instr::Alloc8(self.types[type_].size as u64),
    //             );
    //             temp
    //         }
    //         Expression::StringLiteral(text) => {
    //             let value = self.string_constant(text.to_string(), None);
    //             if let Some(assignment) = assignment {
    //                 let assignment = Value::Temporary(assignment.to_string());
    //                 self.func
    //                     .assign_instr(assignment.clone(), Long, Instr::Copy(value.clone()));
    //                 assignment
    //             } else {
    //                 value
    //             }
    //         }
    //         Expression::Variable(source) => {
    //             let value = Value::Temporary(source.to_string());
    //             if let Some(assignment) = assignment {
    //                 self.func.assign_instr(
    //                     Value::Temporary(assignment.to_string()),
    //                     Long,
    //                     Instr::Copy(value.clone()),
    //                 );
    //             }
    //             value
    //         }
    //     }
    // }

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

    // fn type_(&self, expression: &'a Expression) -> &'a str {
    //     match expression {
    //         Expression::BinaryOperation(_, _) => "int",
    //         Expression::Call(_, _) => "int",
    //         Expression::Field(_, _) => "int",
    //         Expression::Literal(_) => "int",
    //         Expression::New(type_) => type_,
    //         Expression::StringLiteral(_) => "string",
    //         Expression::Variable(variable) => self.variables[variable].type_,
    //     }
    // }

    fn get_temporary(&mut self, assignment: Option<&str>) -> Value {
        if let Some(assignment) = assignment {
            Value::Temporary(assignment.to_string())
        } else {
            let temp_id = self.temp_counter;
            self.temp_counter += 1;
            Value::Temporary(format!("temp_{temp_id}"))
        }
    }
}

pub fn make_il(hir: &hir::Program) -> qbe::Module<'static> {
    let mut state = State {
        hir,
        hir_func: &hir.functions[0],
        il: qbe::Module::new(),
        func: Function::new(Linkage::public(), "main", Vec::new(), Some(Word)),
        functions: Vec::new(),
        types: HashMap::new(),
        variables: HashMap::new(),
        block_counter: 0,
        temp_counter: 0,
        string_counter: 0,
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
                    convert_type(function.bindings[arg.binding].type_.as_ref().unwrap()),
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
        state.func.add_block("start");
        if state.block(&function.block) == Fallthrough::Reachable {
            state.func.add_instr(Instr::Ret(Some(Value::Const(0))));
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
