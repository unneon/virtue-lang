use crate::ast::BinaryOperator;
use crate::vir::Binding;
use crate::{ast, vir};
use std::collections::{HashMap, hash_map};

#[derive(Debug)]
struct State<'a> {
    functions: Vec<vir::Function<'a>>,
    function_map: HashMap<&'a str, usize>,
    structs: Vec<vir::Struct<'a>>,
    struct_map: HashMap<&'a str, usize>,
    strings: Vec<&'a str>,
    current_function: usize,
    current_block: usize,
}

#[derive(Eq, PartialEq)]
enum Fallthrough {
    Unreachable,
    Reachable,
}

impl<'a> State<'a> {
    fn preprocess_function(
        &mut self,
        name: &'a str,
        ast_args: &[(&'a str, ast::Type<'a>)],
        return_type: &ast::Type<'a>,
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
            name,
            args: vir_args,
            return_type: self.convert_type(return_type),
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
                ast::Statement::ForRange { body, .. } => {
                    self.preprocess_block(body);
                }
                ast::Statement::Function(function) => self.preprocess_function(
                    function.name,
                    &function.args,
                    &function.return_type,
                    &function.body,
                ),
                ast::Statement::If { true_, false_, .. } => {
                    self.preprocess_block(true_);
                    self.preprocess_block(false_);
                }
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
                    let struct_ = vir::Struct { fields, field_map };
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
            if self.process_block(self.functions[i].ast_block) == Fallthrough::Reachable
                && self.functions[i].name == "main"
            {
                let default_return = self.make_temporary(vir::Type::I32);
                self.add_statement(vir::Statement::Literal(default_return, 0));
                self.add_statement(vir::Statement::Return(default_return));
            }
        }
    }

    fn process_block(&mut self, statements: &'a [ast::Statement<'a>]) -> Fallthrough {
        for statement in statements {
            match statement {
                ast::Statement::Assignment { left, right } => {
                    let right_binding = self.process_expression(right);
                    let right_type = self.binding_type(right_binding);
                    match left {
                        ast::Expression::Field(object, field_name) => {
                            let object_binding = self.process_expression(object);
                            let struct_id = self.binding_type(object_binding).unwrap_struct();
                            let field_id = self.structs[struct_id].field_map[field_name];
                            self.add_statement(vir::Statement::AssignmentField(
                                object_binding,
                                field_id,
                                right_binding,
                            ));
                        }
                        ast::Expression::Index(indexing) => {
                            let (array, index) = indexing.as_ref();
                            let array = self.process_expression(array);
                            let index = self.process_expression(index);
                            self.add_statement(vir::Statement::AssignmentIndex(
                                array,
                                index,
                                right_binding,
                            ));
                        }
                        ast::Expression::InternalBinding(left_binding) => {
                            self.add_statement(vir::Statement::Assignment(
                                *left_binding,
                                right_binding,
                            ));
                        }
                        ast::Expression::Variable(variable) => {
                            let left_binding = self.make_variable(variable, right_type);
                            self.add_statement(vir::Statement::Assignment(
                                left_binding,
                                right_binding,
                            ));
                        }
                        _ => todo!("vir assignment unimplemented {statement:?}"),
                    }
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
                        BinaryOperator::Less,
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

                    let condition = self.process_expression(condition);
                    self.add_statement(vir::Statement::JumpConditional {
                        condition,
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
                ast::Statement::Print { fmt } => {
                    let segments = fmt
                        .segments
                        .iter()
                        .map(|segment| self.convert_format_segment(segment))
                        .collect();
                    self.add_statement(vir::Statement::Print(vir::FormatString { segments }));
                }
                ast::Statement::Return { value } => {
                    let value = self.process_expression(value);
                    self.add_statement(vir::Statement::Return(value));
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

    fn process_expression(&mut self, expression: &ast::Expression<'a>) -> Binding {
        match expression {
            ast::Expression::ArrayLiteral(initials) => {
                let initials: Vec<_> = initials
                    .iter()
                    .map(|initial| self.process_expression(initial))
                    .collect();
                let type_ = self.binding_type(initials[0]);
                for initial in &initials {
                    assert_eq!(self.binding_type(*initial), type_);
                }
                let length_binding = self.make_temporary(vir::Type::I64);
                let binding =
                    self.make_temporary(vir::BaseType::Array(Box::new(type_.clone())).into());
                self.add_statement(vir::Statement::Literal(
                    length_binding,
                    initials.len() as i64,
                ));
                self.add_statement(vir::Statement::NewArray(binding, length_binding));
                for (i, initial) in initials.iter().enumerate() {
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
                let i_binding = self.make_variable("__i", vir::Type::I64);
                self.add_statement(vir::Statement::Literal(i_binding, 0));
                self.process_block(
                    vec![ast::Statement::While {
                        condition: ast::Expression::BinaryOperation(
                            BinaryOperator::Less,
                            Box::new((
                                ast::Expression::InternalBinding(i_binding),
                                ast::Expression::InternalBinding(length),
                            )),
                        ),
                        body: vec![
                            ast::Statement::Assignment {
                                left: ast::Expression::Index(Box::new((
                                    ast::Expression::InternalBinding(binding),
                                    ast::Expression::InternalBinding(i_binding),
                                ))),
                                right: ast::Expression::InternalBinding(initial),
                            },
                            ast::Statement::Assignment {
                                left: ast::Expression::InternalBinding(i_binding),
                                right: ast::Expression::BinaryOperation(
                                    BinaryOperator::Add,
                                    Box::new((
                                        ast::Expression::InternalBinding(i_binding),
                                        ast::Expression::Literal(1),
                                    )),
                                ),
                            },
                        ],
                    }]
                    .leak(),
                );
                binding
            }
            ast::Expression::BinaryOperation(op, args) => {
                let (left, right) = args.as_ref();
                let left = self.process_expression(left);
                let right = self.process_expression(right);
                let binding = self.make_temporary(vir::Type::I64);
                self.add_statement(vir::Statement::BinaryOperator(binding, *op, left, right));
                binding
            }
            ast::Expression::Call(function_name, args_ast) => {
                let arg_bindings: Vec<_> = args_ast
                    .iter()
                    .map(|arg| self.process_expression(arg))
                    .collect();
                let function_id = self.function_map[function_name];
                let function = &self.functions[function_id];
                assert_eq!(arg_bindings.len(), function.args.len());
                for (arg, arg_binding) in function.args.iter().zip(&arg_bindings) {
                    let arg_type = &function.bindings[arg.binding.id].type_;
                    let arg_binding_type = self.binding_type(*arg_binding);
                    assert_eq!(arg_binding_type, *arg_type);
                }
                let binding = self.make_temporary(function.return_type.clone());
                self.add_statement(vir::Statement::Call(binding, function_id, arg_bindings));
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
                let binding = self.make_temporary(vir::Type::I64);
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
            ast::Expression::InternalBinding(binding) => *binding,
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
                let binding = self.make_temporary(vir::BaseType::String.into());
                self.add_statement(vir::Statement::StringConstant(binding, string_id));
                binding
            }
            ast::Expression::UnaryOperation(op, arg) => {
                let arg = self.process_expression(arg);
                let binding = self.make_temporary(vir::Type::I64);
                self.add_statement(vir::Statement::UnaryOperator(binding, *op, arg));
                binding
            }
            ast::Expression::Variable(variable) => self.variable_binding(variable),
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

    fn make_string(&mut self, text: &'a str) -> usize {
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

    fn add_statement(&mut self, statement: vir::Statement<'a>) {
        self.functions[self.current_function].blocks[self.current_block].push(statement);
    }

    fn convert_type(&self, type_: &ast::Type) -> vir::Type {
        match type_.segments.as_slice() {
            [.., "int"] => vir::Type::I64,
            [.., "i32"] => vir::Type::I32,
            [.., "bool"] => vir::Type::I64,
            [.., "string"] => vir::BaseType::String.into(),
            [.., struct_name] => vir::BaseType::Struct(self.struct_map[struct_name]).into(),
            segments => todo!("convert_type {segments:?}"),
        }
    }

    fn convert_format_segment(&self, segment: &ast::FormatSegment<'a>) -> vir::FormatSegment<'a> {
        match segment {
            ast::FormatSegment::Text(text) => vir::FormatSegment::Text(text),
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
}

pub fn typecheck<'a>(ast: &'a ast::Module<'a>) -> vir::Program<'a> {
    let mut state = State {
        functions: Vec::new(),
        function_map: HashMap::new(),
        structs: Vec::new(),
        struct_map: HashMap::new(),
        strings: Vec::new(),
        current_function: 0,
        current_block: 0,
    };

    let main_type = ast::Type {
        segments: vec!["i32"],
    };
    state.preprocess_function("main", &[], &main_type, &ast.statements);
    state.process_all_functions();

    vir::Program {
        functions: state.functions,
        structs: state.structs,
        strings: state.strings,
    }
}
