use crate::{ast, hir};
use std::collections::{HashMap, hash_map};

#[derive(Debug)]
struct State<'a> {
    functions: Vec<hir::Function<'a>>,
    function_map: HashMap<&'a str, usize>,
    function_queue: Vec<&'a ast::Function<'a>>,
    structs: Vec<hir::Struct>,
    struct_map: HashMap<&'a str, usize>,
    strings: Vec<&'a str>,
    current_function: usize,
}

impl<'a> State<'a> {
    fn declare_function(
        &mut self,
        f_name: &'a str,
        f_args: &[(&'a str, &'a str)],
        f_return_type: &'a str,
        f_body: &'a [ast::Statement<'a>],
    ) {
        let mut args = Vec::new();
        let mut bindings = Vec::new();
        let mut binding_map = HashMap::new();
        for (name, type_) in f_args {
            let binding = bindings.len();
            args.push(hir::Arg { binding });
            bindings.push(hir::Binding {
                type_: Some(self.lookup_type(type_)),
            });
            binding_map.insert(*name, binding);
        }
        let return_type = self.lookup_type(f_return_type);
        self.functions.push(hir::Function {
            exported: f_name == "main",
            name: f_name,
            args,
            return_type,
            bindings,
            binding_map,
            block: vec![],
            ast_block: f_body,
        });
        self.function_map.insert(f_name, self.functions.len() - 1);

        self.declare_block(f_body);
    }

    fn declare_block(&mut self, statements: &'a [ast::Statement<'a>]) {
        for statement in statements {
            if let ast::Statement::Function(function) = statement {
                self.declare_function(
                    function.name,
                    &function.args,
                    &function.return_type,
                    &function.body,
                );
            }
        }
    }

    fn define_functions(&mut self) {
        for i in 0..self.functions.len() {
            self.function(i);
        }
    }

    fn function(&mut self, index: usize) {
        let block = self.functions[index].ast_block;
        self.current_function = index;
        self.block(block);
    }

    fn block(&mut self, statements: &'a [ast::Statement<'a>]) {
        for statement in statements {
            match statement {
                ast::Statement::Assignment { left, right } => {
                    let left = self.expression(left);
                    let right = self.expression(right);
                    let f = &mut self.functions[self.current_function];
                    if let Some(left_type) = f.bindings[left].type_.as_ref() {
                        assert_eq!(left_type, f.bindings[right].type_.as_ref().unwrap());
                    } else {
                        f.bindings[left].type_ = Some(f.bindings[right].type_.clone().unwrap());
                    }
                    self.add_statement(hir::Statement::Assignment(left, right));
                }
                ast::Statement::Function { .. } => {}
                ast::Statement::If { .. } => {}
                ast::Statement::Print { fmt } => {
                    self.add_statement(hir::Statement::Print(
                        fmt.segments
                            .iter()
                            .map(|segment| match segment {
                                ast::FormatSegment::Text(text) => hir::FormatSegment::Text(text),
                                ast::FormatSegment::Variable(variable) => {
                                    hir::FormatSegment::Arg(self.lookup_variable(variable))
                                }
                            })
                            .collect(),
                    ));
                }
                ast::Statement::Return { value } => {
                    let value = self.expression(value);
                    self.add_statement(hir::Statement::Return(value));
                }
                ast::Statement::Struct { .. } => {}
                ast::Statement::While { .. } => {}
            }
        }
    }

    fn expression(&mut self, expression: &ast::Expression<'a>) -> usize {
        match expression {
            ast::Expression::BinaryOperation(op, args) => {
                let (left, right) = args.as_ref();
                let left = self.expression(left);
                let right = self.expression(right);
                let binding = self.make_temporary(hir::Type::I64);
                self.add_statement(hir::Statement::BinaryOperator(
                    binding,
                    op.clone(),
                    left,
                    right,
                ));
                binding
            }
            ast::Expression::Call(function_name, ast_args) => {
                let function_id = self.function_map[function_name];
                let function = &self.functions[function_id];
                let mut hir_args = Vec::new();
                assert_eq!(function.args.len(), ast_args.len());
                let decl_types: Vec<_> = function
                    .args
                    .iter()
                    .map(|decl_arg| {
                        self.functions[function_id].bindings[decl_arg.binding]
                            .type_
                            .clone()
                            .unwrap()
                    })
                    .collect();
                let return_temp = self.make_temporary(function.return_type.clone());
                for (decl_type, call_arg) in decl_types.into_iter().zip(ast_args) {
                    let call_binding = self.expression(call_arg);
                    let call_type = self.lookup_binding_type(call_binding);
                    assert_eq!(call_type, decl_type);
                    hir_args.push(call_binding);
                }
                self.add_statement(hir::Statement::Call(return_temp, function_id, hir_args));
                return_temp
            }
            ast::Expression::Literal(literal) => {
                let binding = self.make_temporary(hir::Type::I64);
                self.add_statement(hir::Statement::Literal(binding, *literal));
                binding
            }
            ast::Expression::StringLiteral(literal) => {
                let string_id = self.strings.len();
                self.strings.push(literal);

                let binding = self.make_temporary(hir::Type::String);
                self.add_statement(hir::Statement::StringConstant(binding, string_id));
                binding
            }
            ast::Expression::Variable(variable) => {
                let f = &mut self.functions[self.current_function];
                match f.binding_map.entry(variable) {
                    hash_map::Entry::Occupied(entry) => *entry.get(),
                    hash_map::Entry::Vacant(entry) => {
                        let binding = f.bindings.len();
                        let binding_data = hir::Binding { type_: None };
                        f.bindings.push(binding_data);
                        *entry.insert(binding)
                    }
                }
            }
            _ => todo!("hir typecheck expression {expression:?}"),
        }
    }

    fn make_temporary(&mut self, type_: hir::Type) -> usize {
        let f = &mut self.functions[self.current_function];
        let binding = f.bindings.len();
        let binding_data = hir::Binding { type_: Some(type_) };
        f.bindings.push(binding_data);
        binding
    }

    fn add_statement(&mut self, statement: hir::Statement<'a>) {
        self.functions[self.current_function].block.push(statement);
    }

    fn lookup_type(&self, type_: &str) -> hir::Type {
        match type_ {
            "int" => hir::Type::I64,
            "i32" => hir::Type::I32,
            "string" => hir::Type::String,
            _ => hir::Type::Struct(self.struct_map[type_]),
        }
    }

    fn lookup_variable(&self, variable: &str) -> usize {
        self.functions[self.current_function].binding_map[variable]
    }

    fn lookup_binding_type(&self, binding: usize) -> hir::Type {
        self.functions[self.current_function].bindings[binding]
            .type_
            .as_ref()
            .unwrap()
            .clone()
    }
}

pub fn typecheck<'a>(ast: &'a ast::Module<'a>) -> hir::Program<'a> {
    let mut state = State {
        functions: Vec::new(),
        function_map: HashMap::new(),
        function_queue: Vec::new(),
        structs: Vec::new(),
        struct_map: HashMap::new(),
        strings: Vec::new(),
        current_function: 0,
    };

    state.declare_function("main", &[], "i32", &ast.statements);
    state.define_functions();

    hir::Program {
        functions: state.functions,
        structs: state.structs,
        strings: state.strings,
    }
}
