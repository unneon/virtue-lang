mod subprocess;

pub use subprocess::compile_ir;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::typecheck::std::{BOOL, I8, I64};
use crate::vir::{BaseType, Binding, Program, Statement, Type, Value};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue};
use inkwell::{AddressSpace, IntPredicate};

struct State<'a> {
    ctx: &'a Context,
    builder: Builder<'a>,
    module: Module<'a>,
    blocks: Option<Vec<BasicBlock<'a>>>,
    bindings: Option<Vec<Option<PointerValue<'a>>>>,
    struct_types: Vec<StructType<'a>>,
    vir: &'a Program<'a>,
    current_function: usize,
    temp_counter: usize,
}

impl<'a> State<'a> {
    fn prologue_structs(&mut self) {
        for struct_ in &self.vir.structs {
            if struct_.should_codegen() {
                let fields: Vec<_> = struct_
                    .fields
                    .iter()
                    .map(|type_| self.convert_type(type_))
                    .collect();
                self.struct_types.push(self.ctx.struct_type(&fields, false));
            } else {
                self.struct_types.push(self.ctx.struct_type(&[], false));
            }
        }
    }

    fn all_functions(&mut self) {
        for function_id in 0..self.vir.functions.len() {
            if self.vir.functions[function_id].should_codegen() {
                self.current_function = function_id;
                self.function_declaration();
            }
        }

        for function_id in 0..self.vir.functions.len() {
            if self.vir.functions[function_id].should_codegen() {
                self.current_function = function_id;
                self.function_definition();
            }
        }
    }

    fn function_declaration(&mut self) {
        let f = &self.vir.functions[self.current_function];
        let args: Vec<_> = f
            .value_args
            .iter()
            .map(|arg| self.convert_type(&f.bindings[arg.binding.id]).into())
            .collect();
        self.module.add_function(
            f.name.as_ref(),
            if f.return_type.is_void() {
                self.ctx.void_type().fn_type(&args, false)
            } else {
                self.convert_type(&f.return_type).fn_type(&args, false)
            },
            Some(if f.is_main {
                Linkage::External
            } else {
                Linkage::Internal
            }),
        );
    }

    fn function_definition(&mut self) {
        self.function_preprocess_blocks();
        self.function_allocate_stack();
        self.function_copy_args();
        self.function_process_blocks();
    }

    fn function_preprocess_blocks(&mut self) {
        let f = &self.vir.functions[self.current_function];
        let function = self.module.get_function(f.name.as_ref()).unwrap();
        let mut blocks = Vec::new();
        for block_id in 0..f.blocks.len() {
            let block = self
                .ctx
                .append_basic_block(function, &format!("b{block_id}"));
            blocks.push(block);
        }
        self.builder.position_at_end(blocks[0]);
        self.blocks = Some(blocks);
    }

    fn function_allocate_stack(&mut self) {
        let f = &self.vir.functions[self.current_function];
        let mut bindings = Vec::new();
        for (binding_id, type_) in f.bindings.iter().enumerate() {
            if type_.is_void() {
                bindings.push(None);
            } else {
                let type_ = self.convert_type(type_);
                let pointer = self
                    .builder
                    .build_alloca(type_, &format!("s{binding_id}"))
                    .unwrap();
                bindings.push(Some(pointer));
            }
        }
        self.bindings = Some(bindings);
    }

    fn function_copy_args(&mut self) {
        let f = &self.vir.functions[self.current_function];
        let function = self.module.get_function(f.name.as_ref()).unwrap();

        for (arg_id, arg) in f.value_args.iter().enumerate() {
            let arg_value = function.get_nth_param(arg_id as u32).unwrap();
            self.store(&arg.binding, arg_value);
        }
    }

    fn function_process_blocks(&mut self) {
        let function = &self.vir.functions[self.current_function];
        for block_id in 0..function.blocks.len() {
            self.block(block_id);
        }
    }

    fn block(&mut self, block_id: usize) {
        let block = self.blocks.as_ref().unwrap()[block_id];
        self.builder.position_at_end(block);

        let function = &self.vir.functions[self.current_function];
        let block = &function.blocks[block_id];
        for statement in block {
            match statement {
                Statement::Alloc(pointer, count) => {
                    let element_type = function.bindings[pointer.id].dereference();
                    let element_size = self.convert_type(&element_type).size_of().unwrap();
                    let count = self.load(count).into_int_value();
                    let size_temp = self.temp();
                    let size = self
                        .builder
                        .build_int_mul(count, element_size, &size_temp)
                        .unwrap();
                    self.syscall(
                        Some(pointer),
                        &[
                            self.ctx.i64_type().const_int(9, false).into(),
                            self.ctx.i64_type().const_int(0, false).into(),
                            size.into(),
                            self.ctx.i64_type().const_int(0x3, false).into(),
                            self.ctx.i64_type().const_int(0x22, false).into(),
                            self.ctx.i64_type().const_int((-1i64) as u64, true).into(),
                            self.ctx.i64_type().const_int(0, false).into(),
                        ],
                    );
                }
                Statement::Assignment(left, right) => {
                    let right = self.load(right);
                    self.store(left, right);
                }
                Statement::AssignmentField(object, field_id, value) => {
                    let field = self.field(object, *field_id);
                    let value = self.load(value);
                    self.builder.build_store(field, value).unwrap();
                }
                Statement::AssignmentIndex(list, index, value) => {
                    let value_type = self.value_type(value);
                    let element_type = &function.bindings[list.id].dereference();
                    let pointer = self.index(list, index);
                    let value = self.load(value);
                    if value_type == *element_type {
                        self.builder.build_store(pointer, value).unwrap();
                    } else if value_type.is_i64() && element_type.is_i8() {
                        let trunc_temp = self.temp();
                        let trunc = self
                            .builder
                            .build_int_truncate(
                                value.into_int_value(),
                                self.ctx.i8_type(),
                                &trunc_temp,
                            )
                            .unwrap();
                        self.builder.build_store(pointer, trunc).unwrap();
                    } else {
                        todo!()
                    }
                }
                Statement::BinaryOperator(result_binding, op, left, right) => {
                    let left_type = self.value_type(left);
                    if let BaseType::Pointer(left_inner) = &left_type.base
                        && left_inner.is_i8()
                    {
                        let left = self.load(left).into_pointer_value();
                        let right = self.load(right).into_int_value();
                        let index = match op {
                            BinaryOperator::Add => right,
                            BinaryOperator::Subtract => {
                                let index_temp = self.temp();
                                self.builder
                                    .build_int_sub(
                                        right.get_type().const_int(0, false),
                                        right,
                                        &index_temp,
                                    )
                                    .unwrap()
                            }
                            _ => unreachable!(),
                        };
                        let temp = self.temp();
                        let result = unsafe {
                            self.builder
                                .build_gep(self.ctx.i8_type(), left, &[index], &temp)
                        }
                        .unwrap();
                        self.store(result_binding, result.into());
                        continue;
                    }

                    let left = self.load(left).into_int_value();
                    let right = self.load(right).into_int_value();
                    let temp = self.temp();
                    let output = match op {
                        BinaryOperator::Add => {
                            self.builder.build_int_add(left, right, &temp).unwrap()
                        }
                        BinaryOperator::Subtract => {
                            self.builder.build_int_sub(left, right, &temp).unwrap()
                        }
                        BinaryOperator::Multiply => {
                            self.builder.build_int_mul(left, right, &temp).unwrap()
                        }
                        BinaryOperator::Divide => self
                            .builder
                            .build_int_signed_div(left, right, &temp)
                            .unwrap(),
                        BinaryOperator::Modulo => self
                            .builder
                            .build_int_signed_rem(left, right, &temp)
                            .unwrap(),
                        BinaryOperator::BitAnd | BinaryOperator::LogicAnd => {
                            self.builder.build_and(left, right, &temp).unwrap()
                        }
                        BinaryOperator::BitOr | BinaryOperator::LogicOr => {
                            self.builder.build_or(left, right, &temp).unwrap()
                        }
                        BinaryOperator::Xor => self.builder.build_xor(left, right, &temp).unwrap(),
                        BinaryOperator::ShiftLeft => {
                            self.builder.build_left_shift(left, right, &temp).unwrap()
                        }
                        BinaryOperator::ShiftRight => self
                            .builder
                            .build_right_shift(left, right, true, &temp)
                            .unwrap(),
                        BinaryOperator::Less => self
                            .builder
                            .build_int_compare(IntPredicate::SLT, left, right, &temp)
                            .unwrap(),
                        BinaryOperator::LessOrEqual => self
                            .builder
                            .build_int_compare(IntPredicate::SLE, left, right, &temp)
                            .unwrap(),
                        BinaryOperator::Greater => self
                            .builder
                            .build_int_compare(IntPredicate::SGT, left, right, &temp)
                            .unwrap(),
                        BinaryOperator::GreaterOrEqual => self
                            .builder
                            .build_int_compare(IntPredicate::SGE, left, right, &temp)
                            .unwrap(),
                        BinaryOperator::Equal => self
                            .builder
                            .build_int_compare(IntPredicate::EQ, left, right, &temp)
                            .unwrap(),
                        BinaryOperator::NotEqual => self
                            .builder
                            .build_int_compare(IntPredicate::NE, left, right, &temp)
                            .unwrap(),
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
                    if returns_bool {
                        let zext_temp = self.temp();
                        let zext = self
                            .builder
                            .build_int_z_extend(output, self.ctx.i8_type(), &zext_temp)
                            .unwrap();
                        self.store(result_binding, zext.into());
                    } else {
                        self.store(result_binding, output.into());
                    }
                }
                Statement::Call {
                    return_,
                    function: callee_id,
                    args,
                    ..
                } => {
                    let temp = self.temp();
                    let args: Vec<_> = args
                        .iter()
                        .map(|arg_binding| self.load(arg_binding).into())
                        .collect();
                    let result = self
                        .builder
                        .build_call(
                            self.module
                                .get_function(self.vir.functions[*callee_id].name.as_ref())
                                .unwrap(),
                            &args,
                            &temp,
                        )
                        .unwrap();
                    if let Some(return_) = return_ {
                        let result = result.try_as_basic_value().left().unwrap();
                        self.store(return_, result);
                    }
                }
                Statement::Field(result_binding, object, field_id) => {
                    let struct_id = function.bindings[object.id].unwrap_struct();
                    let field_type =
                        self.convert_type(&self.vir.structs[struct_id].fields[*field_id]);
                    let field = self.field(object, *field_id);
                    let temp = self.temp();
                    let result = self.builder.build_load(field_type, field, &temp).unwrap();
                    self.store(result_binding, result);
                }
                Statement::Index(result_binding, list, index) => {
                    let element_type = self.convert_type(&function.bindings[list.id].dereference());
                    let pointer = self.index(list, index);
                    let temp = self.temp();
                    let result = self
                        .builder
                        .build_load(element_type, pointer, &temp)
                        .unwrap();
                    self.store(result_binding, result);
                }
                Statement::JumpAlways(block) => {
                    self.builder
                        .build_unconditional_branch(self.blocks.as_ref().unwrap()[*block])
                        .unwrap();
                }
                Statement::JumpConditional {
                    condition,
                    true_block,
                    false_block,
                } => {
                    let cond_val = self.load(condition);
                    let cond_i1_temp = self.temp();
                    let cond_i1 = self
                        .builder
                        .build_int_truncate(
                            cond_val.into_int_value(),
                            self.ctx.custom_width_int_type(1),
                            &cond_i1_temp,
                        )
                        .unwrap();
                    self.builder
                        .build_conditional_branch(
                            cond_i1,
                            self.blocks.as_ref().unwrap()[*true_block],
                            self.blocks.as_ref().unwrap()[*false_block],
                        )
                        .unwrap();
                }
                Statement::Return(binding) => {
                    if let Some(binding) = binding {
                        let value = self.load(binding);
                        if !function.is_main {
                            self.builder.build_return(Some(&value)).unwrap();
                        } else {
                            self.syscall(
                                None,
                                &[
                                    self.ctx.i64_type().const_int(60, false).into(),
                                    value.into(),
                                ],
                            );
                            self.builder.build_unreachable().unwrap();
                        }
                    } else {
                        self.builder.build_return(None).unwrap();
                    }
                }
                Statement::Syscall(binding, arg_bindings) => {
                    let args: Vec<_> = arg_bindings
                        .iter()
                        .map(|binding| self.load(binding).into())
                        .collect();
                    self.syscall(Some(binding), &args);
                }
                Statement::UnaryOperator(result_binding, op, input) => {
                    let input = self.load(input).into_int_value();
                    let temp = self.temp();
                    let output = match op {
                        UnaryOperator::Negate => {
                            self.builder
                                .build_int_sub(input.get_type().const_zero(), input, &temp)
                        }
                        UnaryOperator::BitNot => self.builder.build_xor(
                            input.get_type().const_int(u64::MAX, false),
                            input,
                            &temp,
                        ),
                        UnaryOperator::LogicNot => self.builder.build_xor(
                            input.get_type().const_int(1, false),
                            input,
                            &temp,
                        ),
                    };
                    let result = output.unwrap();
                    self.store(result_binding, result.into());
                }
            }
        }
    }

    fn field(&mut self, object: &Binding, field: usize) -> PointerValue<'a> {
        let object_type = &self.vir.functions[self.current_function].bindings[object.id];
        let object = self.bindings.as_ref().unwrap()[object.id].unwrap();
        let struct_id = object_type.unwrap_struct();
        let temp = self.temp();
        self.builder
            .build_struct_gep(self.struct_types[struct_id], object, field as u32, &temp)
            .unwrap()
    }

    fn index(&mut self, list: &Binding, index: &Value) -> PointerValue<'a> {
        let list_type = &self.vir.functions[self.current_function].bindings[list.id];
        let element_type = list_type.dereference();
        let list = self.load(&(*list).into()).into_pointer_value();
        let index = self.load(index).into_int_value();
        let temp = self.temp();
        unsafe {
            self.builder
                .build_gep(self.convert_type(&element_type), list, &[index], &temp)
        }
        .unwrap()
    }

    fn syscall(&mut self, return_binding: Option<&Binding>, args: &[BasicMetadataValueEnum<'a>]) {
        let arg_types = vec![self.ctx.i64_type().into(); args.len()];
        let func_type = if let Some(return_binding) = return_binding {
            self.convert_type(
                &self.vir.functions[self.current_function].bindings[return_binding.id],
            )
            .fn_type(&arg_types, false)
        } else {
            self.ctx.void_type().fn_type(&arg_types, false)
        };
        let return_constraint = if return_binding.is_some() { "=r," } else { "" };
        let registers =
            ["{rax}", "{rdi}", "{rsi}", "{rdx}", "{r10}", "{r8}", "{r9}"][..args.len()].join(",");

        let func = self.ctx.create_inline_asm(
            func_type,
            "syscall".to_owned(),
            format!("{return_constraint}{registers}"),
            true,
            false,
            None,
            false,
        );

        let result = self
            .builder
            .build_indirect_call(func_type, func, args, "exit")
            .unwrap();
        if let Some(return_binding) = return_binding {
            let result = result.try_as_basic_value().left().unwrap();
            self.store(return_binding, result);
        }
    }

    fn load(&mut self, source: &Value) -> BasicValueEnum<'a> {
        let source = match source {
            Value::Binding(binding) => binding,
            Value::ConstBool(value) => {
                return self.ctx.bool_type().const_int(*value as u64, false).into();
            }
            Value::ConstI64(value) => {
                return self.ctx.i64_type().const_int(*value as u64, true).into();
            }
            Value::Error => unreachable!(),
            Value::String(string_id) => {
                let temp = self.temp();
                return self
                    .builder
                    .build_global_string_ptr(&escape_string(self.vir.strings[*string_id]), &temp)
                    .unwrap()
                    .as_pointer_value()
                    .into();
            }
        };
        let type_ =
            self.convert_type(&self.vir.functions[self.current_function].bindings[source.id]);
        let temp = self.temp();
        self.builder
            .build_load(
                type_,
                self.bindings.as_ref().unwrap()[source.id].unwrap(),
                &temp,
            )
            .unwrap()
    }

    fn store(&mut self, dest: &Binding, source: BasicValueEnum<'a>) {
        self.builder
            .build_store(self.bindings.as_ref().unwrap()[dest.id].unwrap(), source)
            .unwrap();
    }

    fn temp(&mut self) -> String {
        let temp = self.temp_counter;
        self.temp_counter += 1;
        format!("t{temp}")
    }

    fn value_type(&self, value: &Value) -> Type {
        match value {
            Value::Binding(binding) => {
                self.vir.functions[self.current_function].bindings[binding.id].clone()
            }
            Value::ConstBool(_) => BOOL,
            Value::ConstI64(_) => I64,
            Value::Error => unreachable!(),
            Value::String(_) => I8.pointer(),
        }
    }

    fn convert_type(&self, type_: &Type) -> BasicTypeEnum<'a> {
        match type_.base {
            BaseType::I64 => self.ctx.i64_type().into(),
            BaseType::I8 => self.ctx.i8_type().into(),
            BaseType::Bool => self.ctx.i8_type().into(),
            BaseType::Pointer(_) => self.ctx.ptr_type(AddressSpace::default()).into(),
            BaseType::Struct(id, _) => self.struct_types[id].into(),
            BaseType::Void => unreachable!(),
            BaseType::TypeVariable(_) => unreachable!(),
            BaseType::Error => unreachable!(),
        }
    }
}

fn escape_string(text: &[&str]) -> String {
    let mut full = String::new();
    for text in text {
        full += text;
    }
    full
}

pub fn make_ir<'a>(vir: &'a Program, ctx: &'a Context) -> Module<'a> {
    let mut state = State {
        ctx,
        builder: ctx.create_builder(),
        module: ctx.create_module("main"),
        blocks: None,
        bindings: None,
        struct_types: Vec::new(),
        vir,
        current_function: 0,
        temp_counter: 0,
    };
    state.prologue_structs();
    state.all_functions();
    state.module
}
