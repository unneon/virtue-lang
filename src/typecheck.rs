use crate::ast::{BinaryOperator, IncrementDecrementOperator, UnaryOperator};
use crate::error::{Error, SpanExt, Spanned};
use crate::parser::parse;
use crate::vir::Binding;
use crate::{ast, vir};
use std::collections::{HashMap, hash_map};
use std::sync::LazyLock;

#[derive(Debug)]
struct State<'a> {
    functions: Vec<vir::Function<'a>>,
    function_map: HashMap<&'a str, usize>,
    structs: Vec<vir::Struct<'a>>,
    struct_map: HashMap<&'a str, usize>,
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

const NEWLINE_STRING: usize = 0;

static STD_AST: LazyLock<ast::Module> =
    LazyLock::new(|| parse(include_str!("std.virtue")).unwrap());

impl<'a> State<'a> {
    fn preprocess_function(
        &mut self,
        name: &'a str,
        ast_args: &[(&'a str, ast::Type<'a>)],
        return_type: Option<&ast::Type<'a>>,
        body: &'a [ast::Statement<'a>],
    ) {
        let mut vir_args = Vec::new();
        let mut bindings = Vec::new();
        let mut binding_map = HashMap::new();
        for (name, type_) in ast_args {
            let binding = Binding { id: bindings.len() };
            vir_args.push(vir::Arg { binding });
            bindings.push(vir::BindingData {
                type_: self.convert_type(type_),
            });
            binding_map.insert(*name, binding);
        }
        self.functions.push(vir::Function {
            exported: name == "main",
            is_main: name == "main",
            name,
            args: vir_args,
            return_type: match return_type {
                Some(return_type) => self.convert_type(return_type),
                None => vir::BaseType::Void.into(),
            },
            bindings,
            binding_map,
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
                    fields: ast_fields,
                } => {
                    let mut fields = Vec::new();
                    let mut field_map = HashMap::new();
                    for (name, type_) in ast_fields {
                        fields.push(self.convert_type(type_));
                        field_map.insert(*name, fields.len() - 1);
                    }
                    let struct_id = self.structs.len();
                    let struct_ = vir::Struct {
                        name,
                        fields,
                        field_map,
                    };
                    self.structs.push(struct_);
                    self.struct_map.insert(name, struct_id);
                }
                ast::Statement::While { body, .. } => self.preprocess_block(body),
            }
        }
    }

    fn process_all_functions(&mut self) {
        for i in 0..self.functions.len() {
            self.current_function = i;
            self.current_block = self.make_block();
            let fallthrough = self.process_block(self.functions[i].ast_block);
            if fallthrough == Fallthrough::Reachable {
                if self.functions[i].name == "main" {
                    let default_return = self.make_temporary(vir::Type::I32);
                    self.add_statement(vir::Statement::Literal(default_return, 0));
                    self.add_statement(vir::Statement::Return(Some(default_return)));
                } else if self.functions[i].return_type.base == vir::BaseType::Void {
                    self.add_statement(vir::Statement::Return(None));
                }
            }
        }
    }

    fn process_block(&mut self, statements: &'a [ast::Statement<'a>]) -> Fallthrough {
        for statement in statements {
            match statement {
                ast::Statement::Assignment { left, type_, right } => {
                    let right_span = right.span;
                    let right = self.process_expression(right);
                    if let Some(type_) = type_ {
                        let type_ = self.convert_type(type_);
                        self.check_type_compatible(
                            &type_,
                            (&self.binding_type(right)).with_span(right_span),
                        );
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
                    let condition = self.make_temporary(vir::Type::I64);
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
                        (&self.binding_type(condition_binding)).with_span(condition_span),
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
                    let segments = fmt
                        .segments
                        .iter()
                        .map(|segment| self.convert_format_segment(segment))
                        .chain(std::iter::once(vir::FormatSegment::Text(NEWLINE_STRING)))
                        .collect();
                    self.add_statement(vir::Statement::Print(vir::FormatString { segments }));
                }
                ast::Statement::Return { value } => {
                    let value_span = value.span;
                    let value_binding = self.process_expression(value);
                    let value_type = self.binding_type(value_binding);
                    let return_type = self.functions[self.current_function].return_type.clone();
                    self.check_type_compatible(&return_type, (&value_type).with_span(value_span));
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
                            (&self.binding_type(*initial_binding)).with_span(initial_expr.span),
                        );
                    }
                    type_
                } else {
                    vir::BaseType::I64.into()
                };
                let length_binding = self.make_temporary(vir::Type::I64);
                let binding =
                    self.make_temporary(vir::BaseType::Array(Box::new(type_.clone())).into());
                self.add_statement(vir::Statement::Literal(
                    length_binding,
                    initial_bindings.len() as i64,
                ));
                self.add_statement(vir::Statement::NewArray(binding, length_binding));
                for (i, initial) in initial_bindings.iter().enumerate() {
                    let i_binding = self.make_temporary(vir::Type::I64);
                    self.add_statement(vir::Statement::Literal(i_binding, i as i64));
                    self.add_statement(vir::Statement::AssignmentIndex(
                        binding, i_binding, *initial,
                    ));
                }
                binding
            }
            ast::Expression::ArrayRepeat(repeat) => {
                let (initial, length) = repeat.as_ref();
                let initial = self.process_expression(initial);
                let length = self.process_expression(length);
                let type_ = self.binding_type(initial);
                let binding =
                    self.make_temporary(vir::BaseType::Array(Box::new(type_.clone())).into());
                self.add_statement(vir::Statement::NewArray(binding, length));

                let init_condition_block = self.make_block();
                let init_body_block = self.make_block();
                let init_after_block = self.make_block();
                let i_binding = self.make_temporary(vir::Type::I64);
                let i_cmp_binding = self.make_temporary(vir::Type::I64);
                let i_step_binding = self.make_temporary(vir::Type::I64);
                self.add_statement(vir::Statement::Literal(i_binding, 0));
                self.add_statement(vir::Statement::Literal(i_step_binding, 1));
                self.add_statement(vir::Statement::JumpAlways(init_condition_block));

                self.current_block = init_condition_block;
                self.add_statement(vir::Statement::BinaryOperator(
                    i_cmp_binding,
                    BinaryOperator::Less,
                    i_binding,
                    length,
                ));
                self.add_statement(vir::Statement::JumpConditional {
                    condition: i_cmp_binding,
                    true_block: init_body_block,
                    false_block: init_after_block,
                });

                self.current_block = init_body_block;
                self.add_statement(vir::Statement::AssignmentIndex(binding, i_binding, initial));
                self.add_statement(vir::Statement::BinaryOperator(
                    i_binding,
                    BinaryOperator::Add,
                    i_binding,
                    i_step_binding,
                ));
                self.add_statement(vir::Statement::JumpAlways(init_condition_block));

                self.current_block = init_after_block;
                binding
            }
            ast::Expression::BinaryOperation(op, args) => {
                use BinaryOperator::*;
                let (left, right) = args.as_ref();
                let left = self.process_expression(left);
                let right = self.process_expression(right);
                let type_ = match op {
                    Add | Subtract | Multiply | Divide | Modulo | BitAnd | BitOr | Xor
                    | ShiftLeft | ShiftRight => self.binding_type(left),
                    Less | LessOrEqual | Greater | GreaterOrEqual | Equal | NotEqual | LogicAnd
                    | LogicOr => vir::BaseType::Bool.into(),
                };
                let binding = self.make_temporary(type_);
                self.add_statement(vir::Statement::BinaryOperator(binding, *op, left, right));
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
                if *callee_name == "alloc" {
                    let count = match &*args_ast[0] {
                        ast::Expression::Literal(count) => *count,
                        _ => todo!(),
                    };
                    let type_ = match &*args_ast[1] {
                        ast::Expression::Variable("i8") => vir::BaseType::PointerI8.into(),
                        _ => todo!(),
                    };
                    let binding = self.make_temporary(type_);
                    self.add_statement(vir::Statement::Alloc(binding, count as usize));
                    return binding;
                }

                let arg_bindings: Vec<_> = args_ast
                    .iter()
                    .map(|arg| self.process_expression(arg))
                    .collect();

                if *callee_name == "syscall" {
                    let binding = self.make_temporary(vir::Type::I64);
                    self.add_statement(vir::Statement::Syscall(binding, arg_bindings));
                    return binding;
                }

                let callee_id = self.function_map[callee_name];
                let callee = &self.functions[callee_id];
                let callee_return_type = callee.return_type.clone();
                assert_eq!(arg_bindings.len(), callee.args.len());
                for (arg_index, caller_arg_binding) in arg_bindings.iter().enumerate() {
                    let caller_arg_type = self.binding_type(*caller_arg_binding);
                    let caller_arg_type = (&caller_arg_type).with_span(args_ast[arg_index].span);
                    let callee = &self.functions[callee_id];
                    let callee_arg = callee.args[arg_index].binding;
                    let callee_arg_type = callee.bindings[callee_arg.id].type_.clone();
                    self.check_type_compatible(&callee_arg_type, caller_arg_type);
                }
                let binding = self.make_temporary(callee_return_type);
                self.add_statement(vir::Statement::Call(binding, callee_id, arg_bindings));
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
                let field_id = self.structs[struct_id].field_map[field_name];
                let binding = self.make_temporary(self.structs[struct_id].fields[field_id].clone());
                self.add_statement(vir::Statement::Field(binding, object_binding, field_id));
                binding
            }
            ast::Expression::Index(indexing) => {
                let (array, index) = indexing.as_ref();
                let array = self.process_expression(array);
                let index = self.process_expression(index);
                let binding = self.make_temporary(self.binding_type(array).unwrap_list().clone());
                self.add_statement(vir::Statement::Index(binding, array, index));
                binding
            }
            ast::Expression::Literal(literal) => {
                let binding = self.make_temporary(vir::Type::I64);
                self.add_statement(vir::Statement::Literal(binding, *literal));
                binding
            }
            ast::Expression::New(struct_name) => {
                let struct_type = self.convert_type(struct_name);
                let struct_id = struct_type.unwrap_struct();
                let binding = self.make_temporary(struct_type);
                self.add_statement(vir::Statement::New(binding, struct_id));
                binding
            }
            ast::Expression::StringLiteral(literal) => {
                let string_id = self.make_string(literal);
                let binding = self.make_temporary(self.struct_by_name("string"));
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
                let struct_id = self.binding_type(object_binding).unwrap_struct();
                let field_id = self.structs[struct_id].field_map[field_name];
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
                if self.binding_type(array).base == vir::BaseType::Struct(0) {
                    let string = array;
                    let string_pointer = self.make_temporary(vir::BaseType::PointerI8.into());
                    self.add_statement(vir::Statement::Field(string_pointer, string, 0));
                    self.add_statement(vir::Statement::AssignmentIndex(string_pointer, index, src));
                } else {
                    self.add_statement(vir::Statement::AssignmentIndex(array, index, src));
                }
            }
            ast::Expression::Variable(variable) => {
                let left_binding = self.make_variable(variable, src_type);
                self.add_statement(vir::Statement::Assignment(left_binding, src));
            }
            _ => todo!("vir assignment unimplemented {src:?}"),
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

    fn add_statement(&mut self, statement: vir::Statement) {
        self.functions[self.current_function].blocks[self.current_block].push(statement);
    }

    fn check_type_compatible(&mut self, dst: &vir::Type, src: Spanned<&vir::Type>) {
        if dst != *src {
            let dst_fmt = self.format_type(dst);
            let src_fmt = self.format_type(*src);
            self.errors.push(Error {
                message: "type error",
                note: format!("expected {dst_fmt}, found {src_fmt}"),
                note_span: src.span,
            });
        }
    }

    fn convert_type(&self, type_: &ast::Type) -> vir::Type {
        match type_.segments.as_slice() {
            [.., "int"] => vir::Type::I64,
            [.., "i64"] => vir::Type::I64,
            [.., "i32"] => vir::Type::I32,
            [.., "bool"] => vir::BaseType::Bool.into(),
            [.., "pointer_i8"] => vir::BaseType::PointerI8.into(),
            [.., struct_name] => vir::BaseType::Struct(self.struct_map[struct_name]).into(),
            segments => todo!("convert_type {segments:?}"),
        }
    }

    fn convert_format_segment(
        &mut self,
        segment: &'a ast::FormatSegment<'a>,
    ) -> vir::FormatSegment {
        match segment {
            ast::FormatSegment::Text(text) => vir::FormatSegment::Text(self.make_string(text)),
            ast::FormatSegment::Variable(variable) => {
                vir::FormatSegment::Arg(self.variable_binding(variable))
            }
        }
    }

    fn variable_binding(&self, variable: &str) -> Binding {
        self.functions[self.current_function].binding_map[variable]
    }

    fn binding_type(&self, binding: Binding) -> vir::Type {
        self.functions[self.current_function].bindings[binding.id]
            .type_
            .clone()
    }

    fn struct_by_name(&self, name: &str) -> vir::Type {
        vir::BaseType::Struct(self.struct_map[name]).into()
    }

    fn format_type(&self, type_: &vir::Type) -> String {
        assert!(type_.predicates.is_empty());
        match &type_.base {
            vir::BaseType::Array(inner) => {
                let inner = self.format_type(inner);
                format!("list {inner}")
            }
            vir::BaseType::I64 => "int".to_owned(),
            vir::BaseType::I32 => "i32".to_owned(),
            vir::BaseType::I8 => "i8".to_owned(),
            vir::BaseType::Bool => "bool".to_owned(),
            vir::BaseType::PointerI8 => "pointer_i8".to_owned(),
            vir::BaseType::Struct(struct_id) => self.structs[*struct_id].name.to_owned(),
            vir::BaseType::Void => "void".to_owned(),
        }
    }
}

pub fn typecheck<'a>(ast: &'a ast::Module<'a>) -> Result<vir::Program<'a>, Vec<Error>> {
    let mut state = State {
        functions: Vec::new(),
        function_map: HashMap::new(),
        structs: Vec::new(),
        struct_map: HashMap::new(),
        strings: vec![&["\n"]],
        errors: Vec::new(),
        current_function: 0,
        current_block: 0,
    };
    let main_type = ast::Type {
        segments: vec!["i32"],
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
