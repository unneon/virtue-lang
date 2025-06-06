use crate::ast::{BinaryOperator, FormatSegment, IncrementDecrementOperator, UnaryOperator};
use crate::error::{Error, Span, Spanned};
use crate::parser::parse;
use crate::vir::{AnyArg, Binding};
use crate::{ast, vir};
use std::borrow::Cow;
use std::collections::{HashMap, hash_map};
use std::sync::LazyLock;

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
    current_function: usize,
    current_block: usize,
}

#[derive(Eq, PartialEq)]
enum Fallthrough {
    Unreachable,
    Reachable,
}

#[derive(Clone, Copy)]
enum TypeSubstitutionContext<'a> {
    CurrentFunction,
    Custom(&'a HashMap<&'a str, usize>),
}

static STD_AST: LazyLock<ast::Module> =
    LazyLock::new(|| parse(include_str!("std.virtue")).unwrap());

const STRING_TYPE: vir::Type = vir::Type {
    base: vir::BaseType::Struct(0, Vec::new()),
    predicates: Vec::new(),
};

impl<'a> State<'a> {
    fn preprocess_function(
        &mut self,
        name: &'a str,
        ast_args: &[(&'a str, ast::Type<'a>)],
        return_type: Option<&ast::Type<'a>>,
        body: &'a [ast::Statement<'a>],
    ) {
        let mut value_args = Vec::new();
        let mut type_args = Vec::new();
        let mut all_args = Vec::new();
        let mut bindings = Vec::new();
        let mut binding_map = HashMap::new();
        let mut type_variable_map = HashMap::new();
        for (name, type_) in ast_args {
            if **type_.segments.last().unwrap() == "type" {
                type_args.push(());
                type_variable_map.insert(*name, type_variable_map.len());
            }
        }
        for (name, type_) in ast_args {
            if **type_.segments.last().unwrap() != "type" {
                let binding = Binding { id: bindings.len() };
                let type_ =
                    self.convert_type(type_, TypeSubstitutionContext::Custom(&type_variable_map));
                value_args.push(vir::Arg { binding });
                bindings.push(vir::BindingData { type_ });
                binding_map.insert(*name, binding);
            }
        }
        for (name, type_) in ast_args {
            let core_type = **type_.segments.last().unwrap();
            if core_type != "type" {
                all_args.push(vir::AnyArg::Value(binding_map[name].id));
            } else {
                all_args.push(vir::AnyArg::Type(type_variable_map[name]));
            }
        }
        let return_type = match return_type {
            Some(return_type) => self.convert_type(
                return_type,
                TypeSubstitutionContext::Custom(&type_variable_map),
            ),
            None => vir::BaseType::Void.into(),
        };
        self.functions.push(vir::Function {
            exported: name == "main",
            is_main: name == "main",
            is_fully_substituted: type_args.is_empty(),
            name: Cow::Borrowed(name),
            value_args,
            type_args,
            all_args,
            return_type,
            bindings,
            binding_map,
            type_variable_map: HashMap::new(),
            blocks: Vec::new(),
            ast_block: body,
        });
        self.function_map.insert(name, self.functions.len() - 1);

        self.preprocess_block(body);
    }

    fn preprocess_block(&mut self, statements: &'a [ast::Statement<'a>]) {
        for statement in statements {
            match statement {
                ast::Statement::Assignment { .. } => {}
                ast::Statement::AssignmentBinary { .. } => {}
                ast::Statement::Expression(_) => {}
                ast::Statement::ForRange { body, .. } => {
                    self.preprocess_block(body);
                }
                ast::Statement::Function(function) => self.preprocess_function(
                    function.name,
                    &function.args,
                    function.return_type.as_ref(),
                    &function.body,
                ),
                ast::Statement::If { true_, false_, .. } => {
                    self.preprocess_block(true_);
                    self.preprocess_block(false_);
                }
                ast::Statement::IncrementDecrement { .. } => {}
                ast::Statement::Print { .. } => {}
                ast::Statement::Return { .. } => {}
                ast::Statement::Struct {
                    name,
                    args: ast_args,
                    fields: ast_fields,
                } => {
                    let mut type_variable_map = HashMap::new();
                    if let Some(ast_args) = ast_args {
                        for (name, type_) in ast_args {
                            if **type_.segments.last().unwrap() == "type" {
                                type_variable_map.insert(**name, type_variable_map.len());
                            }
                        }
                    }

                    let struct_id = self.structs.len();
                    let mut fields = Vec::new();
                    let mut field_map = HashMap::new();
                    for (name, type_) in ast_fields {
                        fields.push(self.convert_type(
                            type_,
                            TypeSubstitutionContext::Custom(&type_variable_map),
                        ));
                        field_map.insert(*name, fields.len() - 1);
                    }
                    let struct_ = vir::Struct {
                        name: Cow::Borrowed(name),
                        fields,
                        field_map,
                        is_fully_substituted: type_variable_map.is_empty(),
                        type_variable_map,
                    };
                    self.structs.push(struct_);
                    self.struct_map.insert(name, struct_id);
                }
                ast::Statement::While { body, .. } => self.preprocess_block(body),
            }
        }
    }

    fn process_all_functions(&mut self) {
        self.current_function = 0;
        while self.current_function < self.functions.len() {
            self.current_block = self.make_block();
            let fallthrough = self.process_block(self.functions[self.current_function].ast_block);
            if fallthrough == Fallthrough::Reachable {
                if self.functions[self.current_function].name == "main" {
                    let default_return = self.make_temporary(vir::Type::I64);
                    self.add_statement(vir::Statement::Literal(default_return, 0));
                    self.add_statement(vir::Statement::Return(Some(default_return)));
                } else if self.functions[self.current_function].return_type.base
                    == vir::BaseType::Void
                {
                    self.add_statement(vir::Statement::Return(None));
                }
            }
            self.current_function += 1;
        }
    }

    fn process_block(&mut self, statements: &'a [ast::Statement<'a>]) -> Fallthrough {
        for statement in statements {
            match statement {
                ast::Statement::Assignment { left, type_, right } => {
                    let right_span = right.span;
                    let right = self.process_expression(right);
                    if let Some(type_) = type_ {
                        let type_ =
                            self.convert_type(type_, TypeSubstitutionContext::CurrentFunction);
                        self.check_type_compatible(&type_, &self.binding_type(right), right_span);
                    }
                    self.process_assignment(left, right);
                }
                ast::Statement::AssignmentBinary { op, left, right } => {
                    let left_binding = self.process_expression(left);
                    let left_type = self.binding_type(left_binding);
                    let right_binding = self.process_expression(right);
                    let temp = self.make_temporary(left_type);
                    self.add_statement(vir::Statement::BinaryOperator(
                        temp,
                        *op,
                        left_binding,
                        right_binding,
                    ));
                    self.process_assignment(left, temp);
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
                    let lower_binding = self.process_expression(lower);
                    let upper_binding = self.process_expression(upper);
                    let step_binding = if let Some(step) = step {
                        self.process_expression(step)
                    } else {
                        let step = self.make_temporary(vir::Type::I64);
                        self.add_statement(vir::Statement::Literal(step, 1));
                        step
                    };
                    let index_binding = self.make_variable(index, vir::Type::I64);
                    let condition = self.make_temporary(vir::BaseType::Bool.into());
                    let condition_block = self.make_block();
                    let body_block = self.make_block();
                    let after_block = self.make_block();

                    self.add_statement(vir::Statement::Assignment(index_binding, lower_binding));
                    self.add_statement(vir::Statement::JumpAlways(condition_block));

                    self.current_block = condition_block;
                    self.add_statement(vir::Statement::BinaryOperator(
                        condition,
                        BinaryOperator::LessOrEqual,
                        index_binding,
                        upper_binding,
                    ));
                    self.add_statement(vir::Statement::JumpConditional {
                        condition,
                        true_block: body_block,
                        false_block: after_block,
                    });

                    self.current_block = body_block;
                    if self.process_block(body) == Fallthrough::Reachable {
                        let index_next_binding = self.make_temporary(vir::Type::I64);
                        self.add_statement(vir::Statement::BinaryOperator(
                            index_next_binding,
                            BinaryOperator::Add,
                            index_binding,
                            step_binding,
                        ));
                        self.add_statement(vir::Statement::Assignment(
                            index_binding,
                            index_next_binding,
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

                    let condition_span = condition.span;
                    let condition_binding = self.process_expression(condition);
                    self.check_type_compatible(
                        &vir::BaseType::Bool.into(),
                        &self.binding_type(condition_binding),
                        condition_span,
                    );
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
                    let before = self.process_expression(value);
                    let type_ = self.binding_type(before);
                    let one = self.make_temporary(type_.clone());
                    let after = self.make_temporary(type_);
                    let op = match op {
                        IncrementDecrementOperator::Increment => BinaryOperator::Add,
                        IncrementDecrementOperator::Decrement => BinaryOperator::Subtract,
                    };
                    self.add_statement(vir::Statement::Literal(one, 1));
                    self.add_statement(vir::Statement::BinaryOperator(after, op, before, one));
                    self.process_assignment(value, after);
                }
                ast::Statement::Print { fmt } => {
                    for segment in &fmt.segments {
                        match segment {
                            FormatSegment::Text(text) => {
                                let function = self.function_map["virtue_print_str"];
                                let text = self.make_string(text);
                                let text_binding = self.make_temporary(STRING_TYPE);
                                self.add_statement(vir::Statement::StringConstant(
                                    text_binding,
                                    text,
                                ));
                                self.add_statement(vir::Statement::Call {
                                    return_: None,
                                    function,
                                    type_substitutions: Vec::new(),
                                    args: vec![text_binding],
                                });
                            }
                            FormatSegment::Variable(var) => {
                                let var = self.variable_binding(var);
                                let var_type = self.binding_type(var);
                                let function = match var_type.base {
                                    vir::BaseType::I64 => self.function_map["virtue_print_int"],
                                    vir::BaseType::Bool => self.function_map["virtue_print_bool"],
                                    vir::BaseType::Struct(0, _) => {
                                        self.function_map["virtue_print_str"]
                                    }
                                    vir::BaseType::Void => continue,
                                    vir::BaseType::TypeVariable(_) => continue,
                                    vir::BaseType::Error => continue,
                                    _ => todo!(),
                                };
                                self.add_statement(vir::Statement::Call {
                                    return_: None,
                                    function,
                                    type_substitutions: Vec::new(),
                                    args: vec![var],
                                });
                            }
                        }
                    }
                    let newline_text = self.make_string(&["\n"]);
                    let newline_binding = self.make_temporary(STRING_TYPE);
                    self.add_statement(vir::Statement::StringConstant(
                        newline_binding,
                        newline_text,
                    ));
                    self.add_statement(vir::Statement::Call {
                        return_: None,
                        function: self.function_map["virtue_print_str"],
                        type_substitutions: Vec::new(),
                        args: vec![newline_binding],
                    });
                }
                ast::Statement::Return { value } => {
                    let value_span = value.span;
                    let value_binding = self.process_expression(value);
                    let value_type = self.binding_type(value_binding);
                    let return_type = self.functions[self.current_function].return_type.clone();
                    self.check_type_compatible(&return_type, &value_type, value_span);
                    self.add_statement(vir::Statement::Return(Some(value_binding)));
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

    fn process_expression(&mut self, expression: &'a ast::Expression<'a>) -> Binding {
        match expression {
            ast::Expression::ArrayLiteral(initial_exprs) => {
                let initial_bindings: Vec<_> = initial_exprs
                    .iter()
                    .map(|initial| self.process_expression(initial))
                    .collect();
                let type_ = if let Some(initial_0) = initial_bindings.first() {
                    let type_ = self.binding_type(*initial_0).base.into();
                    for (initial_binding, initial_expr) in
                        initial_bindings.iter().zip(initial_exprs)
                    {
                        self.check_type_compatible(
                            &type_,
                            &self.binding_type(*initial_binding),
                            initial_expr.span,
                        );
                    }
                    type_
                } else {
                    vir::BaseType::I64.into()
                };
                let list_struct_id = self.generic_struct_request(1, vec![type_.clone()]);
                let list_type = vir::BaseType::Struct(list_struct_id, Vec::new()).into();
                let length_binding = self.make_temporary(vir::Type::I64);
                let pointer_binding =
                    self.make_temporary(vir::BaseType::Pointer(Box::new(type_.clone())).into());
                self.add_statement(vir::Statement::Literal(
                    length_binding,
                    initial_bindings.len() as i64,
                ));
                self.add_statement(vir::Statement::Alloc(pointer_binding, length_binding));
                for (i, initial_binding) in initial_bindings.iter().enumerate() {
                    let i_binding = self.make_temporary(vir::Type::I64);
                    self.add_statement(vir::Statement::Literal(i_binding, i as i64));
                    self.add_statement(vir::Statement::AssignmentIndex(
                        pointer_binding,
                        i_binding,
                        *initial_binding,
                    ));
                }
                let list_binding = self.make_temporary(list_type);
                self.add_statement(vir::Statement::New(list_binding, list_struct_id));
                self.add_statement(vir::Statement::AssignmentField(
                    list_binding,
                    0,
                    pointer_binding,
                ));
                self.add_statement(vir::Statement::AssignmentField(
                    list_binding,
                    1,
                    length_binding,
                ));
                list_binding
            }
            ast::Expression::ArrayRepeat(repeat) => {
                let (initial, length) = repeat.as_ref();

                let initial_binding = self.process_expression(initial);
                let element_type = self.binding_type(initial_binding);
                let pointer_type = element_type.pointer();
                let list_struct_id = self.generic_struct_request(1, vec![element_type.clone()]);
                let list_type = vir::BaseType::Struct(list_struct_id, Vec::new()).into();

                let length_binding = self.process_expression(length);
                self.check_type_compatible(
                    &vir::Type::I64,
                    &self.binding_type(length_binding),
                    length.span,
                );

                let pointer_binding = self.make_temporary(pointer_type);
                self.add_statement(vir::Statement::Alloc(pointer_binding, length_binding));

                let init_condition_block = self.make_block();
                let init_body_block = self.make_block();
                let init_after_block = self.make_block();
                let i_binding = self.make_temporary(vir::Type::I64);
                let i_cmp_binding = self.make_temporary(vir::BaseType::Bool.into());
                let i_step_binding = self.make_temporary(vir::Type::I64);
                self.add_statement(vir::Statement::Literal(i_binding, 0));
                self.add_statement(vir::Statement::Literal(i_step_binding, 1));
                self.add_statement(vir::Statement::JumpAlways(init_condition_block));

                self.current_block = init_condition_block;
                self.add_statement(vir::Statement::BinaryOperator(
                    i_cmp_binding,
                    BinaryOperator::Less,
                    i_binding,
                    length_binding,
                ));
                self.add_statement(vir::Statement::JumpConditional {
                    condition: i_cmp_binding,
                    true_block: init_body_block,
                    false_block: init_after_block,
                });

                self.current_block = init_body_block;
                self.add_statement(vir::Statement::AssignmentIndex(
                    pointer_binding,
                    i_binding,
                    initial_binding,
                ));
                self.add_statement(vir::Statement::BinaryOperator(
                    i_binding,
                    BinaryOperator::Add,
                    i_binding,
                    i_step_binding,
                ));
                self.add_statement(vir::Statement::JumpAlways(init_condition_block));

                self.current_block = init_after_block;
                let list_binding = self.make_temporary(list_type);
                self.add_statement(vir::Statement::New(list_binding, list_struct_id));
                self.add_statement(vir::Statement::AssignmentField(
                    list_binding,
                    0,
                    pointer_binding,
                ));
                self.add_statement(vir::Statement::AssignmentField(
                    list_binding,
                    1,
                    length_binding,
                ));
                list_binding
            }
            ast::Expression::BinaryOperation(op, args) => {
                use BinaryOperator::*;
                let (left, right) = args.as_ref();
                let left_binding = self.process_expression(left);
                let left_type = self.binding_type(left_binding);
                let right_binding = self.process_expression(right);
                let right_type = self.binding_type(right_binding);
                let type_ = match (**op, &left_type.base, &right_type.base) {
                    (
                        Add | Subtract | Multiply | Divide | Modulo | BitAnd | BitOr | Xor
                        | ShiftLeft | ShiftRight,
                        vir::BaseType::I64,
                        vir::BaseType::I64,
                    ) => vir::Type::I64,
                    (Add | Subtract, vir::BaseType::Pointer(inner), vir::BaseType::I64) => {
                        vir::BaseType::Pointer(inner.clone()).into()
                    }
                    (Add, vir::BaseType::Struct(0, _), vir::BaseType::Struct(0, _)) => {
                        let binding = self.make_temporary(STRING_TYPE);
                        self.add_statement(vir::Statement::Call {
                            return_: Some(binding),
                            function: self.function_map["virtue_add_str"],
                            type_substitutions: Vec::new(),
                            args: vec![left_binding, right_binding],
                        });
                        return binding;
                    }
                    (
                        Less | LessOrEqual | Greater | GreaterOrEqual | Equal | NotEqual,
                        vir::BaseType::I64,
                        vir::BaseType::I64,
                    ) => vir::BaseType::Bool.into(),
                    (LogicAnd | LogicOr, vir::BaseType::Bool, vir::BaseType::Bool) => {
                        vir::BaseType::Bool.into()
                    }
                    _ => {
                        let op_fmt = **op;
                        let left_fmt = self.format_type(&left_type);
                        let right_fmt = self.format_type(&right_type);
                        self.errors.push(Error {
                            message: "incompatible types",
                            note: format!("can't {op_fmt} {left_fmt} and {right_fmt}"),
                            note_span: op.span,
                        });
                        return self.make_temporary(vir::BaseType::Void.into());
                    }
                };
                let binding = self.make_temporary(type_);
                self.add_statement(vir::Statement::BinaryOperator(
                    binding,
                    **op,
                    left_binding,
                    right_binding,
                ));
                binding
            }
            ast::Expression::BoolLiteral(literal) => {
                let binding = self.make_temporary(vir::BaseType::Bool.into());
                self.add_statement(vir::Statement::Literal(
                    binding,
                    if *literal { 1 } else { 0 },
                ));
                binding
            }
            ast::Expression::Call(callee_name, args_ast) => {
                if **callee_name == "alloc" {
                    let count = self.process_expression(&args_ast[0]);
                    let type_ = match &*args_ast[1] {
                        ast::Expression::Variable(name) => {
                            let inner = self
                                .convert_type_impl(
                                    &[*name],
                                    TypeSubstitutionContext::CurrentFunction,
                                )
                                .0;
                            vir::BaseType::Pointer(Box::new(inner)).into()
                        }
                        _ => todo!(),
                    };
                    let binding = self.make_temporary(type_);
                    self.add_statement(vir::Statement::Alloc(binding, count));
                    return binding;
                }

                if **callee_name == "syscall" {
                    let arg_bindings: Vec<_> = args_ast
                        .iter()
                        .map(|arg| self.process_expression(arg))
                        .collect();
                    let binding = self.make_temporary(vir::Type::I64);
                    self.add_statement(vir::Statement::Syscall(binding, arg_bindings));
                    return binding;
                }

                let Ok(callee_id) = self.function_by_name(*callee_name) else {
                    let fake_binding = self.make_temporary(vir::BaseType::Void.into());
                    return fake_binding;
                };

                let mut value_arguments = Vec::new();
                let mut type_substitutions = Vec::new();
                for (arg_index, arg_ast) in (*args_ast).iter().enumerate() {
                    match self.functions[callee_id].all_args.get(arg_index) {
                        Some(AnyArg::Value(_value_arg_index)) => {
                            value_arguments.push(self.process_expression(arg_ast));
                        }
                        Some(AnyArg::Type(_type_arg_index)) => {
                            let ast::Expression::Variable(name) = &**arg_ast else {
                                todo!()
                            };
                            let caller_ast_type = ast::Type {
                                segments: vec![*name],
                            };
                            type_substitutions.push(self.convert_type(
                                &caller_ast_type,
                                TypeSubstitutionContext::CurrentFunction,
                            ));
                        }
                        None => value_arguments.push(self.process_expression(arg_ast)),
                    }
                }

                let callee = &self.functions[callee_id];
                self.check_argument_count(
                    callee.value_args.len(),
                    value_arguments.len(),
                    args_ast.span,
                );
                let callee = &self.functions[callee_id];
                let callee_return_type = callee.return_type.clone();
                if callee.value_args.len() == value_arguments.len() {
                    for (arg_index, caller_arg_binding) in value_arguments.iter().enumerate() {
                        let caller_arg_type = self.binding_type(*caller_arg_binding);
                        let callee = &self.functions[callee_id];
                        let callee_arg = callee.value_args[arg_index].binding;
                        let callee_arg_type = callee.bindings[callee_arg.id]
                            .type_
                            .substitute_types(&type_substitutions);
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

                let binding = self.make_temporary(callee_return_type.clone());
                let call_binding = if callee_return_type.base != vir::BaseType::Void {
                    Some(binding)
                } else {
                    None
                };
                self.add_statement(vir::Statement::Call {
                    return_: call_binding,
                    function: callee_id,
                    type_substitutions,
                    args: value_arguments,
                });
                binding
            }
            ast::Expression::CallMethod(object_expr, _method_name, _args_ast) => {
                let object_binding = self.process_expression(object_expr);
                let _struct_id = self.binding_type(object_binding).unwrap_struct();
                todo!()
            }
            ast::Expression::Field(object_expr, field_name) => {
                let object_binding = self.process_expression(object_expr);
                let struct_id = self.binding_type(object_binding).unwrap_struct();
                let Ok(field_id) = self.struct_field(struct_id, *field_name) else {
                    return self.make_temporary(vir::BaseType::Void.into());
                };
                let binding = self.make_temporary(self.structs[struct_id].fields[field_id].clone());
                self.add_statement(vir::Statement::Field(binding, object_binding, field_id));
                binding
            }
            ast::Expression::Index(indexing) => {
                let (array, index) = indexing.as_ref();
                let array = self.process_expression(array);
                let index = self.process_expression(index);
                if self.binding_type(array).base == STRING_TYPE.base {
                    let string = array;
                    let string_pointer = self.make_temporary(
                        vir::BaseType::Pointer(Box::new(vir::BaseType::I8.into())).into(),
                    );
                    let binding = self.make_temporary(vir::BaseType::I8.into());
                    self.add_statement(vir::Statement::Field(string_pointer, string, 0));
                    self.add_statement(vir::Statement::Index(binding, string_pointer, index));
                    binding
                } else {
                    let array_type = self.binding_type(array);
                    let vir::BaseType::Struct(array_struct_id, _) = array_type.base else {
                        unreachable!()
                    };
                    let pointer_type = &self.structs[array_struct_id].fields[0];
                    let element_type = pointer_type.dereference();
                    let pointer = self.make_temporary(pointer_type.clone());
                    let binding = self.make_temporary(element_type);
                    self.add_statement(vir::Statement::Field(pointer, array, 0));
                    self.add_statement(vir::Statement::Index(binding, pointer, index));
                    binding
                }
            }
            ast::Expression::Literal(literal) => {
                let binding = self.make_temporary(vir::Type::I64);
                self.add_statement(vir::Statement::Literal(binding, *literal));
                binding
            }
            ast::Expression::New(struct_name) => {
                let struct_type =
                    self.convert_type(struct_name, TypeSubstitutionContext::CurrentFunction);
                if struct_type.is_error() {
                    return self.error_binding();
                }
                let vir::BaseType::Struct(struct_id, struct_args) = struct_type.base else {
                    unreachable!()
                };
                let struct_id = if !struct_args.is_empty()
                    && struct_args.iter().all(vir::Type::is_fully_substituted)
                {
                    self.generic_struct_request(struct_id, struct_args.clone())
                } else {
                    struct_id
                };
                let struct_type = vir::Type {
                    predicates: struct_type.predicates,
                    base: vir::BaseType::Struct(struct_id, Vec::new()),
                };
                let binding = self.make_temporary(struct_type);
                self.add_statement(vir::Statement::New(binding, struct_id));
                binding
            }
            ast::Expression::StringLiteral(literal) => {
                let string_id = self.make_string(literal);
                let binding = self.make_temporary(STRING_TYPE);
                self.add_statement(vir::Statement::StringConstant(binding, string_id));
                binding
            }
            ast::Expression::UnaryOperation(op, arg) => {
                use UnaryOperator::*;
                let arg = self.process_expression(arg);
                let type_ = match op {
                    Negate | BitNot => vir::Type::I64,
                    LogicNot => vir::BaseType::Bool.into(),
                };
                let binding = self.make_temporary(type_);
                self.add_statement(vir::Statement::UnaryOperator(binding, *op, arg));
                binding
            }
            ast::Expression::Variable(variable) => self.variable_binding(variable),
        }
    }

    fn process_assignment(&mut self, dest: &'a ast::Expression<'a>, src: Binding) {
        let src_type = self.binding_type(src);
        match dest {
            ast::Expression::Field(object, field_name) => {
                let object_binding = self.process_expression(object);
                let struct_type = self.binding_type(object_binding);
                if struct_type.is_error() {
                    return;
                }
                let struct_id = struct_type.unwrap_struct();
                let Ok(field_id) = self.struct_field(struct_id, *field_name) else {
                    return;
                };
                self.add_statement(vir::Statement::AssignmentField(
                    object_binding,
                    field_id,
                    src,
                ));
            }
            ast::Expression::Index(indexing) => {
                let (array, index) = indexing.as_ref();
                let array = self.process_expression(array);
                let index = self.process_expression(index);
                if self.binding_type(array).base == STRING_TYPE.base {
                    let string = array;
                    let string_pointer = self.make_temporary(
                        vir::BaseType::Pointer(Box::new(vir::BaseType::I8.into())).into(),
                    );
                    self.add_statement(vir::Statement::Field(string_pointer, string, 0));
                    self.add_statement(vir::Statement::AssignmentIndex(string_pointer, index, src));
                } else {
                    let array_type = self.binding_type(array);
                    let vir::BaseType::Struct(array_struct_id, _) = array_type.base else {
                        unreachable!()
                    };
                    let pointer_type = &self.structs[array_struct_id].fields[0];
                    let pointer = self.make_temporary(pointer_type.clone());
                    self.add_statement(vir::Statement::Field(pointer, array, 0));
                    self.add_statement(vir::Statement::AssignmentIndex(pointer, index, src));
                }
            }
            ast::Expression::Variable(variable) => {
                let left_binding = self.make_variable(variable, src_type);
                self.add_statement(vir::Statement::Assignment(left_binding, src));
            }
            _ => todo!("vir assignment unimplemented {src:?}"),
        }
    }

    fn generic_request(&mut self, function_id: usize, type_substitutions: Vec<vir::Type>) -> usize {
        match self
            .generic_function_map
            .entry((function_id, type_substitutions))
        {
            hash_map::Entry::Vacant(entry) => {
                let type_substitutions = &entry.key().1;
                let substituted_id = self.functions.len();
                let function_name = self.functions[function_id].name.as_ref();
                let mut substituted_function = self.functions[function_id].clone();
                substituted_function.is_fully_substituted = true;
                substituted_function.name =
                    Cow::Owned(format!("g{substituted_id}_{function_name}"));
                for arg in &mut substituted_function.value_args {
                    let old_arg_type = &substituted_function.bindings[arg.binding.id].type_;
                    let new_arg_type = old_arg_type.substitute_types(type_substitutions);
                    substituted_function.bindings[arg.binding.id].type_ = new_arg_type;
                }
                entry.insert(substituted_id);
                self.functions.push(substituted_function);
                substituted_id
            }
            hash_map::Entry::Occupied(entry) => *entry.get(),
        }
    }

    fn generic_struct_request(
        &mut self,
        struct_id: usize,
        type_substitutions: Vec<vir::Type>,
    ) -> usize {
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
                substituted_struct.is_fully_substituted = true;
                for field in &mut substituted_struct.fields {
                    *field = field.substitute_types(type_substitutions);
                }
                entry.insert(substitution_id);
                self.structs.push(substituted_struct);
                substitution_id
            }
            hash_map::Entry::Occupied(entry) => *entry.get(),
        }
    }

    fn make_variable(&mut self, variable: &'a str, type_: vir::Type) -> Binding {
        let f = &mut self.functions[self.current_function];
        let binding_count = f.bindings.len();
        match f.binding_map.entry(variable) {
            hash_map::Entry::Occupied(entry) => {
                let binding = *entry.get();
                let binding_type = &f.bindings[binding.id].type_;
                assert_eq!(type_, *binding_type);
                binding
            }
            hash_map::Entry::Vacant(e) => {
                let binding = Binding { id: binding_count };
                f.bindings.push(vir::BindingData { type_ });
                *e.insert(binding)
            }
        }
    }

    fn make_temporary(&mut self, type_: vir::Type) -> Binding {
        let f = &mut self.functions[self.current_function];
        let binding = Binding {
            id: f.bindings.len(),
        };
        let binding_data = vir::BindingData { type_ };
        f.bindings.push(binding_data);
        binding
    }

    fn make_string(&mut self, text: &'a [&'a str]) -> usize {
        let string_id = self.strings.len();
        self.strings.push(text);
        string_id
    }

    fn make_block(&mut self) -> usize {
        let current_function = &mut self.functions[self.current_function];
        let block_id = current_function.blocks.len();
        current_function.blocks.push(Vec::new());
        block_id
    }

    fn error_binding(&mut self) -> Binding {
        self.make_temporary(vir::BaseType::Error.into())
    }

    fn add_statement(&mut self, statement: vir::Statement) {
        self.functions[self.current_function].blocks[self.current_block].push(statement);
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

    fn convert_type(&mut self, type_: &ast::Type<'a>, ctx: TypeSubstitutionContext) -> vir::Type {
        self.convert_type_impl(&type_.segments, ctx).0
    }

    fn convert_type_impl<'b>(
        &mut self,
        segments: &'b [Spanned<&'a str>],
        ctx: TypeSubstitutionContext,
    ) -> (vir::Type, &'b [Spanned<&'a str>]) {
        let (head, tail) = segments.split_first().unwrap();
        match (**head, tail) {
            ("int", []) => return (vir::Type::I64, &[]),
            ("i8", []) => return (vir::Type::I8, &[]),
            ("bool", []) => return (vir::Type::BOOL, &[]),
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
            for _ in 0..self.structs[*struct_id].type_variable_map.len() {
                let (arg, new_tail) = self.convert_type_impl(tail, ctx);
                struct_args.push(arg);
                tail = new_tail;
            }
        }
        (type_, tail)
    }

    fn variable_binding(&self, variable: &str) -> Binding {
        self.functions[self.current_function].binding_map[variable]
    }

    fn binding_type(&self, binding: Binding) -> vir::Type {
        self.functions[self.current_function].bindings[binding.id]
            .type_
            .clone()
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

    fn struct_by_name(&mut self, name: Spanned<&str>, ctx: TypeSubstitutionContext) -> vir::Type {
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
        ctx: TypeSubstitutionContext,
    ) -> Option<vir::Type> {
        let type_variable_map = match ctx {
            TypeSubstitutionContext::CurrentFunction => {
                &self.functions[self.current_function].type_variable_map
            }
            TypeSubstitutionContext::Custom(custom) => custom,
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
                let struct_name = self.structs[*struct_id].name.as_ref();
                let args: String = args
                    .iter()
                    .flat_map(|arg| format!(" {}", self.format_type(arg)).into_chars())
                    .collect();
                format!("{struct_name}{args}")
            }
            vir::BaseType::Void => "void".to_owned(),
            vir::BaseType::TypeVariable(id) => format!("(type variable {id})"),
            vir::BaseType::Error => "(type error)".to_owned(),
        };
        format!("{predicates}{base}")
    }
}

pub fn typecheck<'a>(ast: &'a ast::Module<'a>) -> Result<vir::Program<'a>, Vec<Error>> {
    let mut state = State {
        functions: Vec::new(),
        function_map: HashMap::new(),
        structs: Vec::new(),
        struct_map: HashMap::new(),
        generic_function_map: HashMap::new(),
        generic_struct_map: HashMap::new(),
        strings: vec![&["\n"]],
        errors: Vec::new(),
        current_function: usize::MAX,
        current_block: 0,
    };
    let main_type = ast::Type {
        segments: vec![Spanned::fake("int")],
    };

    state.preprocess_block(&STD_AST.statements);
    state.preprocess_function("main", &[], Some(&main_type), &ast.statements);
    state.process_all_functions();

    if state.errors.is_empty() {
        Ok(vir::Program {
            functions: state.functions,
            structs: state.structs,
            strings: state.strings,
        })
    } else {
        Err(state.errors)
    }
}
