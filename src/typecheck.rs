pub mod std;

use crate::ast::{BinaryOperator, FormatSegment, IncrementDecrementOperator, UnaryOperator};
use crate::error::{Error, Span, SpanExt, Spanned};
use crate::typecheck::std::{BOOL, ERROR, I8, I64, STD_AST, STRING, VOID};
use crate::vir::{Binding, Value};
use crate::{ast, vir};
use ::std::borrow::Cow;
use ::std::collections::{HashMap, hash_map};

#[derive(Debug)]
struct State<'a> {
    functions: Vec<vir::Function<'a>>,
    function_map: HashMap<&'a str, usize>,
    structs: Vec<vir::Struct<'a>>,
    struct_map: HashMap<&'a str, usize>,
    generic_function_map: HashMap<(usize, Vec<vir::Type>), usize>,
    generic_struct_map: HashMap<(usize, Vec<vir::Type>), usize>,
    strings: Vec<&'a [&'a str]>,
    errors: Vec<Error>,
    current_function: Option<usize>,
    current_block: usize,
}

#[derive(Eq, PartialEq)]
enum Fallthrough {
    Unreachable,
    Reachable,
}

#[derive(Clone, Copy)]
enum TypeArgContext<'a> {
    CurrentFunction,
    Custom(&'a HashMap<&'a str, usize>),
}

impl<'a> State<'a> {
    fn preprocess_function(&mut self, function: &'a ast::Function<'a>) {
        let id = self.functions.len();
        self.function_map.insert(function.name, id);

        let type_arg_map = collect_type_args(&function.args);
        let mut value_args = Vec::new();
        let mut all_args = Vec::new();
        let mut bindings = Vec::new();
        let mut binding_map = HashMap::new();
        for (name, type_) in &function.args {
            if !type_.is_type() {
                let binding = Binding { id: bindings.len() };
                let type_ = self.resolve_ast_type_nocf(type_, &type_arg_map);
                value_args.push(vir::Arg { binding });
                bindings.push(type_);
                binding_map.insert(**name, binding);
            }
        }
        for (_, type_) in &function.args {
            if !type_.is_type() {
                all_args.push(vir::AnyArg::Value);
            } else {
                all_args.push(vir::AnyArg::Type);
            }
        }
        let return_type = match &function.return_type {
            Some(return_type) => self.resolve_ast_type_nocf(return_type, &type_arg_map),
            None => VOID,
        };
        self.functions.push(vir::Function {
            exported: function.name == "_start",
            is_main: function.name == "_start",
            is_used: false,
            name: Cow::Borrowed(function.name),
            value_args,
            all_args,
            return_type,
            bindings,
            binding_map,
            type_arg_substitutions: if type_arg_map.is_empty() {
                Some(Vec::new())
            } else {
                None
            },
            type_arg_map,
            blocks: Vec::new(),
            ast_block: &function.body,
        });

        self.preprocess_block(&function.body);
    }

    fn preprocess_block(&mut self, statements: &'a [ast::Statement<'a>]) {
        for statement in statements {
            match statement {
                ast::Statement::Assignment { .. } => {}
                ast::Statement::AssignmentBinary { .. } => {}
                ast::Statement::Expression(_) => {}
                ast::Statement::ForRange { .. } => {}
                ast::Statement::Function(function) => self.preprocess_function(function),
                ast::Statement::If { .. } => {}
                ast::Statement::IncrementDecrement { .. } => {}
                ast::Statement::Print { .. } => {}
                ast::Statement::Return { .. } => {}
                ast::Statement::Struct(struct_) => self.preprocess_struct(struct_),
                ast::Statement::While { .. } => {}
            }
        }
    }

    fn preprocess_struct(&mut self, struct_: &'a ast::Struct<'a>) {
        let struct_id = self.structs.len();
        self.struct_map.insert(struct_.name, struct_id);

        let type_arg_map = collect_type_args(&struct_.args);
        let mut fields = Vec::new();
        let mut field_map = HashMap::new();
        for (name, type_) in &struct_.fields {
            let type_ = self.resolve_ast_type_nocf(type_, &type_arg_map);
            fields.push(type_);
            field_map.insert(*name, fields.len() - 1);
        }
        self.structs.push(vir::Struct {
            name: Cow::Borrowed(struct_.name),
            fields,
            field_map,
            is_instantiated: type_arg_map.is_empty(),
            is_used: false,
            type_arg_map,
            instantiation_info: None,
        });
    }

    fn process_all_functions(&mut self) {
        let mut current_function = 0;
        while current_function < self.functions.len() {
            self.current_function = Some(current_function);
            self.current_block = self.make_block();
            let fallthrough = self.process_block(self.functions[current_function].ast_block);
            if fallthrough == Fallthrough::Reachable {
                if self.current_function().is_main {
                    self.add_statement(vir::Statement::Return(Some(Value::ConstI64(0))));
                } else if self.current_function().return_type.is_void() {
                    self.add_statement(vir::Statement::Return(None));
                }
            }
            current_function += 1;
        }
        self.current_function = None;
    }

    fn process_block(&mut self, statements: &'a [ast::Statement<'a>]) -> Fallthrough {
        for statement in statements {
            match statement {
                ast::Statement::Assignment { left, type_, right } => {
                    let right_span = right.span;
                    let right = self.process_expression(right);
                    if let Some(type_) = type_ {
                        let type_ = self.resolve_ast_type(type_);
                        self.check_type_compatible(&type_, &self.value_type(right), right_span);
                    }
                    // TODO: Push down assignment to `left` as optional `process_expression` arg?
                    self.process_assignment(left, right.with_span(right_span));
                }
                ast::Statement::AssignmentBinary { op, left, right } => {
                    let left = self.process_expression(left).unwrap_binding();
                    let right = self.process_expression(right);
                    self.add_statement(vir::Statement::BinaryOperator(
                        left,
                        *op,
                        left.into(),
                        right,
                    ));
                }
                ast::Statement::Expression(expression) => {
                    self.process_expression(expression);
                }
                ast::Statement::ForRange {
                    index,
                    lower,
                    upper,
                    step,
                    body,
                } => {
                    let lower_span = lower.span;
                    let lower = self.process_expression(lower);
                    self.check_type_compatible(&I64, &self.value_type(lower), lower_span);

                    let upper_span = upper.span;
                    let upper = self.process_expression(upper);
                    self.check_type_compatible(&I64, &self.value_type(upper), upper_span);

                    let step = if let Some(step) = step {
                        let step_span = step.span;
                        let step = self.process_expression(step);
                        self.check_type_compatible(&I64, &self.value_type(step), step_span);
                        step
                    } else {
                        Value::ConstI64(1)
                    };

                    let index = self.make_variable(index, I64);
                    let condition = self.make_temporary(BOOL);
                    let condition_block = self.make_block();
                    let body_block = self.make_block();
                    let after_block = self.make_block();

                    self.add_statement(vir::Statement::Assignment(index, lower));
                    self.add_statement(vir::Statement::JumpAlways(condition_block));

                    self.current_block = condition_block;
                    self.add_statement(vir::Statement::BinaryOperator(
                        condition,
                        BinaryOperator::LessOrEqual,
                        index.into(),
                        upper,
                    ));
                    self.add_statement(vir::Statement::JumpConditional {
                        condition: condition.into(),
                        true_block: body_block,
                        false_block: after_block,
                    });

                    self.current_block = body_block;
                    if self.process_block(body) == Fallthrough::Reachable {
                        self.add_statement(vir::Statement::BinaryOperator(
                            index,
                            BinaryOperator::Add,
                            index.into(),
                            step,
                        ));
                        self.add_statement(vir::Statement::JumpAlways(condition_block));
                    }

                    self.current_block = after_block;
                }
                ast::Statement::Function { .. } => {}
                ast::Statement::If {
                    condition,
                    true_,
                    false_,
                } => {
                    let true_block = self.make_block();
                    let false_block = self.make_block();
                    let after_block = self.make_block();

                    let condition_binding = self.process_expression(condition);
                    let condition_type = self.value_type(condition_binding);
                    self.check_type_compatible(&BOOL, &condition_type, condition.span);
                    self.add_statement(vir::Statement::JumpConditional {
                        condition: condition_binding,
                        true_block,
                        false_block,
                    });

                    self.current_block = true_block;
                    if self.process_block(true_) == Fallthrough::Reachable {
                        self.add_statement(vir::Statement::JumpAlways(after_block));
                    }

                    self.current_block = false_block;
                    if self.process_block(false_) == Fallthrough::Reachable {
                        self.add_statement(vir::Statement::JumpAlways(after_block));
                    }

                    self.current_block = after_block;
                }
                ast::Statement::IncrementDecrement { value, op } => {
                    let value = self.process_expression(value).unwrap_binding();
                    let one = Value::ConstI64(1);
                    let op = match op {
                        IncrementDecrementOperator::Increment => BinaryOperator::Add,
                        IncrementDecrementOperator::Decrement => BinaryOperator::Subtract,
                    };
                    self.add_statement(vir::Statement::BinaryOperator(
                        value,
                        op,
                        value.into(),
                        one,
                    ));
                }
                ast::Statement::Print { fmt } => {
                    for segment in &fmt.segments {
                        match segment {
                            FormatSegment::Text(text) => {
                                let function = self.function_map["virtue_print_raw"];
                                let string_id = self.make_string(text);
                                let pointer = Value::String(string_id);
                                let length = Value::ConstI64(self.string_len(string_id) as i64);
                                self.add_statement(vir::Statement::Call {
                                    return_: None,
                                    function,
                                    args: vec![pointer, length],
                                });
                            }
                            FormatSegment::Variable(var) => {
                                let var = self.variable_binding(var);
                                let var_type = self.binding_type(var);
                                let function_name = match var_type.base {
                                    vir::BaseType::I64 => "virtue_print_int",
                                    vir::BaseType::Bool => "virtue_print_bool",
                                    vir::BaseType::Struct(0, _) => "virtue_print_str",
                                    vir::BaseType::TypeVariable(_) => continue,
                                    vir::BaseType::Error => continue,
                                    _ => todo!(),
                                };
                                let function = self.function_map[function_name];
                                self.add_statement(vir::Statement::Call {
                                    return_: None,
                                    function,
                                    args: vec![var.into()],
                                });
                            }
                        }
                    }
                    let newline_string_id = self.make_string(&["\n"]);
                    self.add_statement(vir::Statement::Call {
                        return_: None,
                        function: self.function_map["virtue_print_raw"],
                        args: vec![Value::String(newline_string_id), Value::ConstI64(1)],
                    });
                }
                ast::Statement::Return { value } => {
                    let value_span = value.span;
                    let value = self.process_expression(value);
                    let value_type = self.value_type(value);
                    let return_type = self.current_function().return_type.clone();
                    self.check_type_compatible(&return_type, &value_type, value_span);
                    self.add_statement(vir::Statement::Return(Some(value)));
                    return Fallthrough::Unreachable;
                }
                ast::Statement::Struct { .. } => {}
                ast::Statement::While { condition, body } => {
                    let condition_block = self.make_block();
                    let body_block = self.make_block();
                    let after_block = self.make_block();

                    self.add_statement(vir::Statement::JumpAlways(condition_block));

                    self.current_block = condition_block;
                    let condition = self.process_expression(condition);
                    self.add_statement(vir::Statement::JumpConditional {
                        condition,
                        true_block: body_block,
                        false_block: after_block,
                    });

                    self.current_block = body_block;
                    if self.process_block(body) == Fallthrough::Reachable {
                        self.add_statement(vir::Statement::JumpAlways(condition_block));
                    }

                    self.current_block = after_block;
                }
            }
        }
        Fallthrough::Reachable
    }

    fn process_expression(&mut self, expression: &'a ast::Expression<'a>) -> Value {
        match expression {
            ast::Expression::BinaryOperation(op, args) => {
                use BinaryOperator::*;
                let (left, right) = args.as_ref();
                let left = self.process_expression(left);
                let left_type = self.value_type(left);
                let right = self.process_expression(right);
                let right_type = self.value_type(right);
                let type_ = match (**op, &left_type.base, &right_type.base) {
                    (
                        Add | Subtract | Multiply | Divide | Modulo | BitAnd | BitOr | Xor
                        | ShiftLeft | ShiftRight,
                        vir::BaseType::I64,
                        vir::BaseType::I64,
                    ) => I64,
                    (Add | Subtract, vir::BaseType::Pointer(inner), vir::BaseType::I64) => {
                        inner.pointer()
                    }
                    (Add, vir::BaseType::Struct(0, _), vir::BaseType::Struct(0, _)) => {
                        let binding = self.make_temporary(STRING);
                        self.add_statement(vir::Statement::Call {
                            return_: Some(binding),
                            function: self.function_map["virtue_add_str"],
                            args: vec![left, right],
                        });
                        return binding.into();
                    }
                    (Add, _, _)
                        if let Some(element_type) = self.list_get_element(&left_type)
                            && left_type == right_type =>
                    {
                        let binding = self.make_temporary(left_type.clone());
                        if element_type.is_fully_substituted() {
                            let element_type = self.resolve_type(element_type);
                            let function = self.generic_request(
                                self.function_map["virtue_add_list"],
                                vec![element_type.clone()],
                            );
                            self.add_statement(vir::Statement::Call {
                                return_: Some(binding),
                                function,
                                args: vec![left, right],
                            });
                        }
                        return binding.into();
                    }
                    (
                        Less | LessOrEqual | Greater | GreaterOrEqual | Equal | NotEqual,
                        vir::BaseType::I64,
                        vir::BaseType::I64,
                    ) => BOOL,
                    (LogicAnd | LogicOr, vir::BaseType::Bool, vir::BaseType::Bool) => BOOL,
                    (_, vir::BaseType::Error, _) | (_, _, vir::BaseType::Error) => ERROR,
                    _ => {
                        let op_fmt = **op;
                        let left_fmt = self.format_type(&left_type);
                        let right_fmt = self.format_type(&right_type);
                        self.errors.push(Error {
                            message: "incompatible types",
                            note: format!("can't {op_fmt} {left_fmt} and {right_fmt}"),
                            note_span: op.span,
                        });
                        return Value::Error;
                    }
                };
                let binding = self.make_temporary(type_);
                self.add_statement(vir::Statement::BinaryOperator(binding, **op, left, right));
                binding.into()
            }
            ast::Expression::BoolLiteral(literal) => Value::ConstBool(*literal),
            ast::Expression::Call(callee_name, args_ast) => {
                if **callee_name == "alloc" {
                    let count = self.process_expression(&args_ast[0]);
                    let ast::Expression::Variable(element_type_name) = *args_ast[1] else {
                        todo!()
                    };
                    let element_type_segments = ::std::slice::from_ref(&element_type_name);
                    let element_type = self.resolve_ast_type_segments(element_type_segments);
                    let pointer = self.make_temporary(element_type.pointer());
                    self.add_statement(vir::Statement::Alloc(pointer, count));
                    return pointer.into();
                }

                if **callee_name == "syscall" {
                    let arg_bindings: Vec<_> = args_ast
                        .iter()
                        .map(|arg| self.process_expression(arg))
                        .collect();
                    let binding = self.make_temporary(I64);
                    self.add_statement(vir::Statement::Syscall(binding, arg_bindings));
                    return binding.into();
                }

                let Ok(callee_id) = self.function_by_name(*callee_name) else {
                    return Value::Error;
                };

                let mut value_args = Vec::new();
                let mut type_substitutions = Vec::new();
                for (arg_index, arg_ast) in (*args_ast).iter().enumerate() {
                    match self.functions[callee_id].all_args.get(arg_index) {
                        Some(vir::AnyArg::Value) => {
                            value_args.push(self.process_expression(arg_ast));
                        }
                        Some(vir::AnyArg::Type) => {
                            let ast::Expression::Variable(type_arg_name) = &**arg_ast else {
                                todo!()
                            };
                            let type_arg_segments = ::std::slice::from_ref(type_arg_name);
                            let type_arg = self.resolve_ast_type_segments(type_arg_segments);
                            type_substitutions.push(type_arg);
                        }
                        None => value_args.push(self.process_expression(arg_ast)),
                    }
                }

                let callee = &self.functions[callee_id];
                self.check_argument_count(callee.value_args.len(), value_args.len(), args_ast.span);

                let callee = &self.functions[callee_id];
                if callee.value_args.len() == value_args.len() {
                    for (arg_index, caller_arg_binding) in value_args.iter().enumerate() {
                        let caller_arg_type = self.value_type(*caller_arg_binding);
                        let callee = &self.functions[callee_id];
                        let callee_arg = callee.value_args[arg_index].binding;
                        let callee_arg_type =
                            callee.bindings[callee_arg.id].substitute_types(&type_substitutions);
                        let callee_arg_type = self.resolve_type(callee_arg_type);
                        self.check_type_compatible(
                            &callee_arg_type,
                            &caller_arg_type,
                            args_ast[arg_index].span,
                        );
                    }
                }

                let callee_id = if !type_substitutions.is_empty()
                    && type_substitutions
                        .iter()
                        .all(|type_| type_.is_fully_substituted())
                {
                    self.generic_request(callee_id, type_substitutions.clone())
                } else {
                    callee_id
                };

                let callee_return_type = self.functions[callee_id].return_type.clone();
                let callee_return_is_void = callee_return_type.is_void();
                let binding = self.make_temporary(callee_return_type);
                let call_binding = if !callee_return_is_void {
                    Some(binding)
                } else {
                    None
                };
                self.add_statement(vir::Statement::Call {
                    return_: call_binding,
                    function: callee_id,
                    args: value_args,
                });
                binding.into()
            }
            ast::Expression::CallMethod(object_expr, _method_name, _args_ast) => {
                let object_binding = self.process_expression(object_expr);
                let _struct_id = self.value_type(object_binding).unwrap_struct();
                todo!()
            }
            ast::Expression::Field(object_expr, field_name) => {
                let object_binding = self.process_expression(object_expr).unwrap_binding();
                let struct_type = self.binding_type(object_binding);
                if struct_type.is_error() {
                    return Value::Error;
                }
                let struct_id = struct_type.unwrap_struct();
                let Ok(field_id) = self.struct_field(struct_id, *field_name) else {
                    return Value::Error;
                };
                let binding = self.make_temporary(self.structs[struct_id].fields[field_id].clone());
                self.add_statement(vir::Statement::Field(binding, object_binding, field_id));
                binding.into()
            }
            ast::Expression::Index(indexing) => {
                let (list, index) = indexing.as_ref();
                let list = self.process_expression(list).unwrap_binding();
                let index = self.process_expression(index);
                if self.binding_type(list).base == STRING.base {
                    let string = list;
                    let string_pointer = self.make_temporary(I8.pointer());
                    let binding = self.make_temporary(I8);
                    self.add_statement(vir::Statement::Field(string_pointer, string, 0));
                    self.add_statement(vir::Statement::Index(binding, string_pointer, index));
                    binding.into()
                } else {
                    let list_type = self.binding_type(list);
                    if list_type.is_error() {
                        return Value::Error;
                    }
                    let vir::BaseType::Struct(list_struct_id, _) = list_type.base else {
                        unreachable!()
                    };
                    let pointer_type = &self.structs[list_struct_id].fields[0];
                    let element_type = pointer_type.dereference();
                    let pointer = self.make_temporary(pointer_type.clone());
                    let binding = self.make_temporary(element_type);
                    self.add_statement(vir::Statement::Field(pointer, list, 0));
                    self.add_statement(vir::Statement::Index(binding, pointer, index));
                    binding.into()
                }
            }
            ast::Expression::ListLiteral(initial_exprs) => {
                let initial_bindings: Vec<_> = initial_exprs
                    .iter()
                    .map(|initial| self.process_expression(initial))
                    .collect();
                let element_type = if let Some(initial_0) = initial_bindings.first() {
                    let first_type = self.value_type(*initial_0).base.into();
                    for (later_binding, later_expr) in initial_bindings.iter().zip(initial_exprs) {
                        self.check_type_compatible(
                            &first_type,
                            &self.value_type(*later_binding),
                            later_expr.span,
                        );
                    }
                    first_type
                } else {
                    I64
                };
                let list_type = self.resolve_type(element_type.list());
                let length = Value::ConstI64(initial_bindings.len() as i64);
                let pointer = self.make_temporary(element_type.pointer());
                self.add_statement(vir::Statement::Alloc(pointer, length));
                for (i, initial_binding) in initial_bindings.iter().enumerate() {
                    self.add_statement(vir::Statement::AssignmentIndex(
                        pointer,
                        Value::ConstI64(i as i64),
                        *initial_binding,
                    ));
                }
                let list = self.make_temporary(list_type);
                self.add_statement(vir::Statement::AssignmentField(list, 0, pointer.into()));
                self.add_statement(vir::Statement::AssignmentField(list, 1, length));
                list.into()
            }
            ast::Expression::ListRepeat(repeat) => {
                let (initial, length) = repeat.as_ref();
                let initial_binding = self.process_expression(initial);
                let element_type = self.value_type(initial_binding);

                let length_binding = self.process_expression(length);
                self.check_type_compatible(&I64, &self.value_type(length_binding), length.span);

                let pointer = self.make_temporary(element_type.pointer());
                self.add_statement(vir::Statement::Alloc(pointer, length_binding));

                let init_condition_block = self.make_block();
                let init_body_block = self.make_block();
                let init_after_block = self.make_block();
                let i_cmp = self.make_temporary(BOOL);
                let i = self.make_temporary(I64);
                self.add_statement(vir::Statement::Assignment(i, Value::ConstI64(0)));
                self.add_statement(vir::Statement::JumpAlways(init_condition_block));

                self.current_block = init_condition_block;
                self.add_statement(vir::Statement::BinaryOperator(
                    i_cmp,
                    BinaryOperator::Less,
                    i.into(),
                    length_binding,
                ));
                self.add_statement(vir::Statement::JumpConditional {
                    condition: i_cmp.into(),
                    true_block: init_body_block,
                    false_block: init_after_block,
                });

                self.current_block = init_body_block;
                self.add_statement(vir::Statement::AssignmentIndex(
                    pointer,
                    i.into(),
                    initial_binding,
                ));
                self.add_statement(vir::Statement::BinaryOperator(
                    i,
                    BinaryOperator::Add,
                    i.into(),
                    Value::ConstI64(1),
                ));
                self.add_statement(vir::Statement::JumpAlways(init_condition_block));

                self.current_block = init_after_block;
                let list_type = self.resolve_type(element_type.list());
                let list = self.make_temporary(list_type);
                self.add_statement(vir::Statement::AssignmentField(list, 0, pointer.into()));
                self.add_statement(vir::Statement::AssignmentField(list, 1, length_binding));
                list.into()
            }
            ast::Expression::Literal(literal) => Value::ConstI64(*literal),
            ast::Expression::New(type_) => {
                let type_ = self.resolve_ast_type(type_);
                self.make_temporary(type_).into()
            }
            ast::Expression::StringLiteral(literal) => {
                let string_id = self.make_string(literal);
                let binding = self.make_temporary(STRING);
                let pointer_binding = Value::String(string_id);
                let length_binding = Value::ConstI64(self.string_len(string_id) as i64);
                self.add_statement(vir::Statement::AssignmentField(binding, 0, pointer_binding));
                self.add_statement(vir::Statement::AssignmentField(binding, 1, length_binding));
                binding.into()
            }
            ast::Expression::UnaryOperation(op, arg) => {
                use UnaryOperator::*;
                let arg = self.process_expression(arg);
                let type_ = match op {
                    Negate | BitNot => I64,
                    LogicNot => BOOL,
                };
                let binding = self.make_temporary(type_);
                self.add_statement(vir::Statement::UnaryOperator(binding, *op, arg));
                binding.into()
            }
            ast::Expression::Variable(variable) => self.variable_binding(variable).into(),
        }
    }

    fn process_assignment(&mut self, dest: &'a ast::Expression<'a>, src: Spanned<Value>) {
        let src_type = self.value_type(*src);
        match dest {
            ast::Expression::Field(object, field_name) => {
                let object_binding = self.process_expression(object).unwrap_binding();
                let struct_type = self.binding_type(object_binding);
                if struct_type.is_error() {
                    return;
                }
                let struct_id = struct_type.unwrap_struct();
                let Ok(field_id) = self.struct_field(struct_id, *field_name) else {
                    return;
                };
                let field_type = self.structs[struct_id].fields[field_id].clone();
                self.check_type_compatible(&field_type, &src_type, src.span);
                self.add_statement(vir::Statement::AssignmentField(
                    object_binding,
                    field_id,
                    *src,
                ));
            }
            ast::Expression::Index(indexing) => {
                let (list, index) = indexing.as_ref();
                let list = self.process_expression(list).unwrap_binding();
                let index = self.process_expression(index);
                if self.binding_type(list).base == STRING.base {
                    let string = list;
                    let string_pointer = self.make_temporary(I8.pointer());
                    // TODO: Typecheck src is either I64 or I8, and fix the backends probably?
                    self.add_statement(vir::Statement::Field(string_pointer, string, 0));
                    self.add_statement(vir::Statement::AssignmentIndex(
                        string_pointer,
                        index,
                        *src,
                    ));
                } else {
                    let list_type = self.binding_type(list);
                    let vir::BaseType::Struct(list_struct_id, _) = list_type.base else {
                        unreachable!()
                    };
                    let pointer_type = &self.structs[list_struct_id].fields[0];
                    let element_type = pointer_type.dereference();
                    let pointer = self.make_temporary(pointer_type.clone());
                    self.check_type_compatible(&element_type, &src_type, src.span);
                    self.add_statement(vir::Statement::Field(pointer, list, 0));
                    self.add_statement(vir::Statement::AssignmentIndex(pointer, index, *src));
                }
            }
            ast::Expression::Variable(variable) => {
                let left = self.make_variable(variable, src_type.clone());
                let left_type = self.binding_type(left);
                self.check_type_compatible(&left_type, &src_type, src.span);
                self.add_statement(vir::Statement::Assignment(left, *src));
            }
            _ => todo!("vir assignment unimplemented {src:?}"),
        }
    }

    fn compute_used(&mut self) {
        for function_id in 0..self.functions.len() {
            let f = &self.functions[function_id];
            if f.is_main {
                self.compute_used_walk_function(function_id);
            }
        }
    }

    fn compute_used_walk_function(&mut self, function_id: usize) {
        self.functions[function_id].is_used = true;
        self.compute_used_walk_type(self.functions[function_id].return_type.clone());
        for binding_id in 0..self.functions[function_id].bindings.len() {
            self.compute_used_walk_type(self.functions[function_id].bindings[binding_id].clone());
        }
        for block_id in 0..self.functions[function_id].blocks.len() {
            for statement_id in 0..self.functions[function_id].blocks[block_id].len() {
                if let vir::Statement::Call {
                    function: callee, ..
                } = self.functions[function_id].blocks[block_id][statement_id]
                    && !self.functions[callee].is_used
                {
                    self.compute_used_walk_function(callee);
                }
            }
        }
    }

    fn compute_used_walk_type(&mut self, type_: vir::Type) {
        match type_.base {
            vir::BaseType::Struct(struct_id, _) => {
                if !self.structs[struct_id].is_used {
                    self.structs[struct_id].is_used = true;
                    for field_id in 0..self.structs[struct_id].fields.len() {
                        self.compute_used_walk_type(
                            self.structs[struct_id].fields[field_id].clone(),
                        );
                    }
                }
            }
            vir::BaseType::Pointer(inner) => {
                self.compute_used_walk_type((*inner).clone());
            }
            _ => {}
        }
    }

    fn list_get_element(&self, type_: &vir::Type) -> Option<vir::Type> {
        let vir::BaseType::Struct(early_struct_id, early_args) = &type_.base else {
            return None;
        };
        if *early_struct_id == 1 {
            return Some(early_args[0].clone());
        }
        let Some(instantiation_info) = &self.structs[*early_struct_id].instantiation_info else {
            return None;
        };
        if instantiation_info.generic_struct_id != 1 {
            return None;
        }
        Some(instantiation_info.type_args[0].clone())
    }

    fn resolve_type(&mut self, type_: vir::Type) -> vir::Type {
        let type_ =
            if let Some(type_substitutions) = &self.current_function().type_arg_substitutions {
                type_.substitute_types(type_substitutions)
            } else {
                type_
            };
        self.instantiate_type(type_)
    }

    fn resolve_ast_type(&mut self, type_: &ast::Type<'a>) -> vir::Type {
        let type_ = self.convert_type(type_, TypeArgContext::CurrentFunction);
        self.resolve_type(type_)
    }

    fn resolve_ast_type_segments(&mut self, type_: &[Spanned<&'a str>]) -> vir::Type {
        let type_ = self.convert_type_segments(type_, TypeArgContext::CurrentFunction);
        self.resolve_type(type_)
    }

    fn resolve_ast_type_nocf<'b>(
        &mut self,
        type_: &ast::Type<'a>,
        ctx: impl Into<TypeArgContext<'b>>,
    ) -> vir::Type {
        let type_ = self.convert_type(type_, ctx);
        self.instantiate_type(type_)
    }

    fn instantiate_type(&mut self, type_: vir::Type) -> vir::Type {
        use vir::BaseType::*;
        let base = match &type_.base {
            I64 | I8 | Bool | Void | Error => type_.base.clone(),
            Pointer(inner) => Pointer(Box::new(self.instantiate_type((**inner).clone()))),
            Struct(struct_id, args) if !args.is_empty() && type_.is_fully_substituted() => {
                let args = args
                    .iter()
                    .map(|arg| self.instantiate_type(arg.clone()))
                    .collect();
                Struct(self.generic_struct_request(*struct_id, args), Vec::new())
            }
            Struct(_, _) => type_.base.clone(),
            TypeVariable(_) => type_.base.clone(),
        };
        vir::Type {
            predicates: type_.predicates.clone(),
            base,
        }
    }

    fn generic_request(&mut self, function_id: usize, type_args: Vec<vir::Type>) -> usize {
        for type_ in &type_args {
            assert!(type_.is_fully_substituted());
        }
        match self
            .generic_function_map
            .entry((function_id, type_args.clone()))
        {
            hash_map::Entry::Vacant(entry) => {
                let instantiation_id = self.functions.len();
                entry.insert(instantiation_id);

                let f = &self.functions[function_id];
                let f_name = f.name.as_ref();
                let instantiated = vir::Function {
                    exported: false,
                    is_main: false,
                    is_used: false,
                    name: Cow::Owned(format!("g{instantiation_id}_{f_name}")),
                    value_args: f.value_args.clone(),
                    all_args: f.all_args.clone(),
                    binding_map: f.binding_map.clone(),
                    type_arg_map: f.type_arg_map.clone(),
                    type_arg_substitutions: Some(type_args.clone()),
                    blocks: f.blocks.clone(),
                    ast_block: f.ast_block,
                    bindings: (0..f.bindings.len())
                        .map(|binding_id| {
                            self.instantiate_type(
                                self.functions[function_id].bindings[binding_id]
                                    .substitute_types(&type_args),
                            )
                        })
                        .collect(),
                    return_type: self.instantiate_type(
                        self.functions[function_id]
                            .return_type
                            .substitute_types(&type_args),
                    ),
                };

                self.functions.push(instantiated);

                instantiation_id
            }
            hash_map::Entry::Occupied(entry) => *entry.get(),
        }
    }

    fn generic_struct_request(
        &mut self,
        struct_id: usize,
        type_substitutions: Vec<vir::Type>,
    ) -> usize {
        for type_substitution in &type_substitutions {
            assert!(type_substitution.is_fully_substituted());
        }
        match self
            .generic_struct_map
            .entry((struct_id, type_substitutions))
        {
            hash_map::Entry::Vacant(entry) => {
                let type_substitutions = &entry.key().1;
                let substitution_id = self.structs.len();
                let struct_name = self.structs[struct_id].name.as_ref();
                let mut substituted_struct = self.structs[struct_id].clone();
                substituted_struct.name = Cow::Owned(format!("g{substitution_id}_{struct_name}"));
                substituted_struct.is_instantiated = type_substitutions
                    .iter()
                    .all(|type_| type_.is_fully_substituted());
                for field in &mut substituted_struct.fields {
                    *field = field.substitute_types(type_substitutions);
                }
                substituted_struct.instantiation_info = Some(vir::InstantiationInfo {
                    generic_struct_id: struct_id,
                    type_args: type_substitutions.clone(),
                });
                entry.insert(substitution_id);
                self.structs.push(substituted_struct);
                substitution_id
            }
            hash_map::Entry::Occupied(entry) => *entry.get(),
        }
    }

    fn make_variable(&mut self, variable: &'a str, type_: vir::Type) -> Binding {
        let f = self.current_function_mut();
        if f.type_arg_substitutions.is_some() {
            assert!(
                type_.is_fully_substituted(),
                "type {type_:?} is not fully substituted"
            );
            assert!(
                type_.is_fully_instantiated(),
                "type {type_:?} is not fully instantiated"
            );
        }
        let binding_count = f.bindings.len();
        match f.binding_map.entry(variable) {
            hash_map::Entry::Occupied(entry) => *entry.get(),
            hash_map::Entry::Vacant(e) => {
                if f.type_arg_substitutions.is_some() {
                    assert!(type_.is_fully_substituted());
                }
                let binding = Binding { id: binding_count };
                f.bindings.push(type_);
                *e.insert(binding)
            }
        }
    }

    fn make_temporary(&mut self, type_: vir::Type) -> Binding {
        let f = self.current_function_mut();
        if f.type_arg_substitutions.is_some() {
            assert!(
                type_.is_fully_substituted(),
                "type {type_:?} is not fully substituted"
            );
        }
        let binding = Binding {
            id: f.bindings.len(),
        };
        f.bindings.push(type_);
        binding
    }

    fn make_string(&mut self, text: &'a [&'a str]) -> usize {
        let string_id = self.strings.len();
        self.strings.push(text);
        string_id
    }

    fn make_block(&mut self) -> usize {
        let block_id = self.current_function_mut().blocks.len();
        self.current_function_mut().blocks.push(Vec::new());
        block_id
    }

    fn add_statement(&mut self, statement: vir::Statement) {
        let block = self.current_block;
        self.current_function_mut().blocks[block].push(statement);
    }

    fn check_type_compatible(&mut self, dst: &vir::Type, src: &vir::Type, span: Span) {
        if dst != src {
            let dst_fmt = self.format_type(dst);
            let src_fmt = self.format_type(src);
            self.errors.push(Error {
                message: "type error",
                note: format!("expected {dst_fmt}, found {src_fmt}"),
                note_span: span,
            });
        }
    }

    fn check_argument_count(&mut self, expected: usize, actual: usize, span: Span) {
        if actual != expected {
            self.errors.push(Error {
                message: "wrong number of arguments",
                note: format!("expected {expected}, found {actual}"),
                note_span: span,
            });
        }
    }

    fn convert_type<'b>(
        &mut self,
        type_: &ast::Type<'a>,
        ctx: impl Into<TypeArgContext<'b>>,
    ) -> vir::Type {
        self.convert_type_segments(&type_.segments, ctx)
    }

    fn convert_type_segments<'b>(
        &mut self,
        type_: &[Spanned<&'a str>],
        ctx: impl Into<TypeArgContext<'b>>,
    ) -> vir::Type {
        self.convert_type_impl(type_, ctx.into()).0
    }

    fn convert_type_impl<'b>(
        &mut self,
        segments: &'b [Spanned<&'a str>],
        ctx: TypeArgContext,
    ) -> (vir::Type, &'b [Spanned<&'a str>]) {
        let (head, tail) = segments.split_first().unwrap();
        match (**head, tail) {
            ("int", []) => return (I64, &[]),
            ("i8", []) => return (I8, &[]),
            ("bool", []) => return (BOOL, &[]),
            ("ptr", tail) => {
                let (inner, tail) = self.convert_type_impl(tail, ctx);
                return (vir::BaseType::Pointer(Box::new(inner)).into(), tail);
            }
            _ => {}
        }
        if let Some(&function_id) = self.function_map.get(**head) {
            let (mut type_, tail) = self.convert_type_impl(tail, ctx);
            type_.predicates.push(function_id);
            return (type_, tail);
        }
        let mut type_ = self.struct_by_name(*head, ctx);
        if let vir::BaseType::Struct(struct_id, struct_args) = &mut type_.base {
            let mut tail = tail;
            for _ in 0..self.structs[*struct_id].type_arg_map.len() {
                let (arg, new_tail) = self.convert_type_impl(tail, ctx);
                struct_args.push(arg);
                tail = new_tail;
            }
        }
        (type_, tail)
    }

    fn current_function(&self) -> &vir::Function {
        &self.functions[self.current_function.unwrap()]
    }

    fn current_function_mut(&mut self) -> &mut vir::Function<'a> {
        &mut self.functions[self.current_function.unwrap()]
    }

    fn value_type(&self, value: Value) -> vir::Type {
        match value {
            Value::Binding(binding) => self.binding_type(binding),
            Value::ConstBool(_) => BOOL,
            Value::ConstI64(_) => I64,
            Value::Error => ERROR,
            Value::String(_) => STRING,
        }
    }

    fn variable_binding(&self, variable: &str) -> Binding {
        self.current_function().binding_map[variable]
    }

    fn binding_type(&self, binding: Binding) -> vir::Type {
        self.current_function().bindings[binding.id].clone()
    }

    fn function_by_name(&mut self, name: Spanned<&str>) -> Result<usize, ()> {
        match self.function_map.get(*name) {
            Some(id) => Ok(*id),
            None => {
                self.errors.push(Error {
                    message: "function does not exist",
                    note: String::new(),
                    note_span: name.span,
                });
                Err(())
            }
        }
    }

    fn struct_by_name(&mut self, name: Spanned<&str>, ctx: TypeArgContext) -> vir::Type {
        match self.try_struct_by_name(name, ctx) {
            Some(type_) => type_,
            None => {
                self.errors.push(Error {
                    message: "struct does not exist",
                    note: String::new(),
                    note_span: name.span,
                });
                vir::BaseType::Error.into()
            }
        }
    }

    fn try_struct_by_name(
        &mut self,
        name: Spanned<&str>,
        ctx: TypeArgContext,
    ) -> Option<vir::Type> {
        let type_variable_map = match ctx {
            TypeArgContext::CurrentFunction => &self.current_function().type_arg_map,
            TypeArgContext::Custom(custom) => custom,
        };
        if let Some(type_variable) = type_variable_map.get(*name) {
            return Some(vir::BaseType::TypeVariable(*type_variable).into());
        }
        self.struct_map
            .get(*name)
            .map(|id| vir::BaseType::Struct(*id, Vec::new()).into())
    }

    fn struct_field(&mut self, struct_id: usize, field_name: Spanned<&str>) -> Result<usize, ()> {
        match self.structs[struct_id].field_map.get(*field_name) {
            Some(id) => Ok(*id),
            None => {
                let struct_name = self.structs[struct_id].name.as_ref();
                self.errors.push(Error {
                    message: "field does not exist",
                    note: format!("struct {struct_name} does not have this field"),
                    note_span: field_name.span,
                });
                Err(())
            }
        }
    }

    fn string_len(&self, id: usize) -> usize {
        self.strings[id].iter().map(|s| s.len()).sum()
    }

    fn format_type(&self, type_: &vir::Type) -> String {
        let predicates: String = type_
            .predicates
            .iter()
            .rev()
            .flat_map(|id| format!("{} ", self.functions[*id].name).into_chars())
            .collect();
        let base = match &type_.base {
            vir::BaseType::I64 => "int".to_owned(),
            vir::BaseType::I8 => "i8".to_owned(),
            vir::BaseType::Bool => "bool".to_owned(),
            vir::BaseType::Pointer(inner) => format!("ptr {}", self.format_type(inner)),
            vir::BaseType::Struct(struct_id, args) => {
                let struct_ = &self.structs[*struct_id];
                let (name, args) = if let Some(instantiation_info) = &struct_.instantiation_info {
                    let generic_struct = &self.structs[instantiation_info.generic_struct_id];
                    (generic_struct.name.as_ref(), &instantiation_info.type_args)
                } else {
                    (struct_.name.as_ref(), args)
                };
                let args: String = args
                    .iter()
                    .flat_map(|arg| format!(" {}", self.format_type(arg)).into_chars())
                    .collect();
                format!("{name}{args}")
            }
            vir::BaseType::Void => "void".to_owned(),
            vir::BaseType::TypeVariable(id) => format!("(type variable {id})"),
            vir::BaseType::Error => "(type error)".to_owned(),
        };
        format!("{predicates}{base}")
    }
}

impl<'a, 'b> From<&'a HashMap<&'b str, usize>> for TypeArgContext<'a> {
    fn from(type_args: &'a HashMap<&'b str, usize>) -> TypeArgContext<'a> {
        TypeArgContext::Custom(type_args)
    }
}

fn collect_type_args<'a>(args: &'a [(Spanned<&'a str>, ast::Type<'a>)]) -> HashMap<&'a str, usize> {
    let mut map = HashMap::new();
    for (name, type_) in args {
        if type_.is_type() {
            map.insert(**name, map.len());
        }
    }
    map
}

pub fn typecheck<'a>(ast: &'a ast::Module<'a>) -> Result<vir::Program<'a>, Vec<Error>> {
    let mut state = State {
        functions: Vec::new(),
        function_map: HashMap::new(),
        structs: Vec::new(),
        struct_map: HashMap::new(),
        generic_function_map: HashMap::new(),
        generic_struct_map: HashMap::new(),
        strings: Vec::new(),
        errors: Vec::new(),
        current_function: None,
        current_block: 0,
    };

    state.preprocess_block(&STD_AST.statements);
    state.preprocess_block(&ast.statements);
    state.process_all_functions();
    state.compute_used();

    if !state.errors.is_empty() {
        return Err(state.errors);
    }

    Ok(vir::Program {
        functions: state.functions,
        structs: state.structs,
        strings: state.strings,
    })
}
