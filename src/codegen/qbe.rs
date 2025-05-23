mod subprocess;

pub use subprocess::compile_il;

use crate::ast;
use crate::ast::{BinaryOperator, Expression, FormatSegment, Statement};
use qbe::Type::{Byte, Long, Word};
use qbe::{Cmp, DataDef, DataItem, Function, Instr, Linkage, Value};
use std::collections::HashMap;

struct State<'a> {
    il: qbe::Module<'static>,
    func: Function<'static>,
    functions: Vec<(&'a str, &'a [&'a str], &'a [Statement<'a>])>,
    types: HashMap<&'a str, Type<'a>>,
    variables: HashMap<&'a str, Variable<'a>>,
    block_counter: usize,
    temp_counter: usize,
    print_counter: usize,
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
    fn block(&mut self, statements: &'a [Statement<'a>]) -> Fallthrough {
        for statement in statements {
            match statement {
                Statement::Assignment { left, right } => match left {
                    Expression::Variable(variable) => {
                        self.expression(right, Some(variable));
                    }
                    Expression::Field(object, field) => {
                        let temporary = self.expression(right, None);
                        let field_offset = self.types[self.type_(object)].fields[field];
                        let field = self.expression(
                            &Expression::add(
                                object.as_ref().clone(),
                                Expression::Literal(field_offset as i64),
                            ),
                            None,
                        );
                        self.func.add_instr(Instr::Store(Long, field, temporary));
                    }
                    _ => todo!("unsupported assignment to {left:?}"),
                },
                Statement::Function {
                    name,
                    args: parameters,
                    body,
                } => {
                    self.functions.push((name, parameters, body));
                }
                Statement::If {
                    condition,
                    true_block,
                    false_block,
                } => {
                    let true_block_id = format!("_{}", self.block_counter);
                    let false_block_id = format!("_{}", self.block_counter + 1);
                    let after_block_id = format!("_{}", self.block_counter + 2);
                    self.block_counter += 3;

                    let condition = self.expression(condition, None);
                    self.func.add_instr(Instr::Jnz(
                        condition,
                        true_block_id.clone(),
                        false_block_id.clone(),
                    ));

                    self.func.add_block(true_block_id);
                    if self.block(true_block) == Fallthrough::Reachable {
                        self.func.add_instr(Instr::Jmp(after_block_id.clone()));
                    }

                    self.func.add_block(false_block_id);
                    if self.block(false_block) == Fallthrough::Reachable {
                        self.func.add_instr(Instr::Jmp(after_block_id.clone()));
                    }

                    self.func.add_block(after_block_id);
                }
                Statement::Print { fmt } => {
                    let print_id = self.print_counter;
                    self.print_counter += 1;

                    let mut c_fmt = String::new();
                    let mut c_args = Vec::new();

                    c_args.push((Long, Value::Global(format!("print_{print_id}"))));

                    for segment in &fmt.segments {
                        match segment {
                            FormatSegment::Text(text) => c_fmt.push_str(text),
                            FormatSegment::Variable(variable) => {
                                c_fmt.push_str("%lld");
                                c_args.push((Long, Value::Temporary(variable.to_string())));
                            }
                        }
                    }

                    c_fmt.push_str("\\n");

                    self.il.add_data(DataDef::new(
                        Linkage::private(),
                        format!("print_{print_id}"),
                        None,
                        vec![(Byte, DataItem::Str(c_fmt)), (Byte, DataItem::Const(0))],
                    ));

                    self.func
                        .add_instr(Instr::Call("printf".into(), c_args, None));
                }
                Statement::Return { value } => {
                    let value = self.expression(value, None);
                    self.func.add_instr(Instr::Ret(Some(value)));
                    return Fallthrough::Unreachable;
                }
                Statement::Struct { name, fields } => {
                    let size = 8 * fields.len();
                    let fields = fields
                        .iter()
                        .enumerate()
                        .map(|(i, (name, _))| (*name, i * 8))
                        .collect();
                    self.types.insert(name, Type { size, fields });
                }
                Statement::While { condition, body } => {
                    let condition_block = format!("_{}", self.block_counter);
                    let body_block = format!("_{}", self.block_counter + 1);
                    let after_block = format!("_{}", self.block_counter + 2);
                    self.block_counter += 3;

                    self.func.add_instr(Instr::Jmp(condition_block.clone()));

                    self.func.add_block(condition_block.clone());
                    let condition = self.expression(condition, None);
                    self.func.add_instr(Instr::Jnz(
                        condition,
                        body_block.clone(),
                        after_block.clone(),
                    ));

                    self.func.add_block(body_block);
                    if self.block(body) == Fallthrough::Reachable {
                        self.func.add_instr(Instr::Jmp(condition_block));
                    }

                    self.func.add_block(after_block);
                }
            }
        }
        Fallthrough::Reachable
    }

    fn expression(&mut self, expression: &Expression<'a>, assignment: Option<&'a str>) -> Value {
        match expression {
            Expression::BinaryOperation(op, args) => {
                let (left, right) = args.as_ref();
                let left = self.expression(left, None);
                let right = self.expression(right, None);
                let temp = self.get_temporary(assignment);
                let instr = match op {
                    BinaryOperator::Add => Instr::Add(left, right),
                    BinaryOperator::Subtract => Instr::Sub(left, right),
                    BinaryOperator::Multiply => Instr::Mul(left, right),
                    BinaryOperator::Divide => Instr::Div(left, right),
                    BinaryOperator::Modulo => Instr::Rem(left, right),
                    BinaryOperator::Less => Instr::Cmp(Long, Cmp::Slt, left, right),
                    BinaryOperator::LessOrEqual => Instr::Cmp(Long, Cmp::Sle, left, right),
                    BinaryOperator::Greater => Instr::Cmp(Long, Cmp::Sgt, left, right),
                    BinaryOperator::GreaterOrEqual => Instr::Cmp(Long, Cmp::Sge, left, right),
                    BinaryOperator::Equal => Instr::Cmp(Long, Cmp::Eq, left, right),
                    BinaryOperator::NotEqual => Instr::Cmp(Long, Cmp::Ne, left, right),
                };
                self.func.assign_instr(temp.clone(), Long, instr);
                temp
            }
            Expression::Call(func, args) => {
                let temp = self.get_temporary(assignment);
                let args = args
                    .iter()
                    .map(|arg| (Long, self.expression(arg, None)))
                    .collect();
                self.func.assign_instr(
                    temp.clone(),
                    Long,
                    Instr::Call(func.to_string(), args, None),
                );
                temp
            }
            Expression::Field(object, field) => {
                let field_offset = self.types[self.type_(object)].fields[field];
                let temp = self.get_temporary(assignment);
                let field = self.expression(
                    &Expression::add(
                        object.as_ref().clone(),
                        Expression::Literal(field_offset as i64),
                    ),
                    None,
                );
                self.func
                    .assign_instr(temp.clone(), Long, Instr::Load(Long, field));
                temp
            }
            Expression::Literal(literal) => {
                let value = Value::Const(*literal as u64);
                if let Some(assignment) = assignment {
                    self.func.assign_instr(
                        Value::Temporary(assignment.to_string()),
                        Long,
                        Instr::Copy(value.clone()),
                    );
                }
                value
            }
            Expression::New(type_) => {
                self.variables
                    .insert(assignment.unwrap(), Variable { type_ });
                let temp = self.get_temporary(assignment);
                self.func.assign_instr(
                    temp.clone(),
                    Long,
                    Instr::Alloc8(self.types[type_].size as u64),
                );
                temp
            }
            Expression::Variable(source) => {
                let value = Value::Temporary(source.to_string());
                if let Some(assignment) = assignment {
                    self.func.assign_instr(
                        Value::Temporary(assignment.to_string()),
                        Long,
                        Instr::Copy(value.clone()),
                    );
                }
                value
            }
        }
    }

    fn type_(&self, expression: &'a Expression) -> &'a str {
        match expression {
            Expression::Variable(variable) => self.variables[variable].type_,
            _ => todo!("type of {expression:?}"),
        }
    }

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

pub fn make_il(ast: &ast::Module) -> qbe::Module<'static> {
    let mut state = State {
        il: qbe::Module::new(),
        func: Function::new(Linkage::public(), "main", Vec::new(), Some(Word)),
        functions: Vec::new(),
        types: HashMap::new(),
        variables: HashMap::new(),
        block_counter: 0,
        temp_counter: 0,
        print_counter: 0,
    };

    state.func.add_block("start");
    state.block(&ast.statements);
    state.func.add_instr(Instr::Ret(Some(Value::Const(0))));
    state.il.add_function(state.func);
    while let Some((name, args, body)) = state.functions.pop() {
        let args = args
            .iter()
            .map(|arg| (Long, Value::Temporary(arg.to_string())))
            .collect();
        state.func = Function::new(Linkage::private(), name, args, Some(Long));
        state.func.add_block("start");
        state.block(body);
        state.il.add_function(state.func);
    }
    state.il
}
