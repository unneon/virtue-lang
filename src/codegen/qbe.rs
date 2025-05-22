use crate::ast;
use crate::ast::{BinaryOperator, Expression, FormatSegment, Statement};
use qbe::{Cmp, DataDef, DataItem, Function, Instr, Linkage, Type, Value};
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

struct State<'a> {
    il: qbe::Module<'static>,
    func: Function<'static>,
    functions: Vec<(&'a str, &'a [&'a str], &'a [Statement<'a>])>,
    block_counter: usize,
    temp_counter: usize,
    print_counter: usize,
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
                Statement::Assignment {
                    variable,
                    expression,
                } => {
                    self.expression(expression, Some(variable));
                }
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

                    c_args.push((Type::Long, Value::Global(format!("print_{print_id}"))));

                    for segment in &fmt.segments {
                        match segment {
                            FormatSegment::Text(text) => c_fmt.push_str(text),
                            FormatSegment::Variable(variable) => {
                                c_fmt.push_str("%lld");
                                c_args.push((Type::Long, Value::Temporary(variable.to_string())));
                            }
                        }
                    }

                    c_fmt.push_str("\\n");

                    self.il.add_data(DataDef::new(
                        Linkage::private(),
                        format!("print_{print_id}"),
                        None,
                        vec![
                            (Type::Byte, DataItem::Str(c_fmt)),
                            (Type::Byte, DataItem::Const(0)),
                        ],
                    ));

                    self.func
                        .add_instr(Instr::Call("printf".into(), c_args, None));
                }
                Statement::Return { value } => {
                    let value = self.expression(value, None);
                    self.func.add_instr(Instr::Ret(Some(value)));
                    return Fallthrough::Unreachable;
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

    fn expression(&mut self, expression: &Expression, assignment: Option<&str>) -> Value {
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
                    BinaryOperator::Less => Instr::Cmp(Type::Long, Cmp::Slt, left, right),
                    BinaryOperator::LessOrEqual => Instr::Cmp(Type::Long, Cmp::Sle, left, right),
                    BinaryOperator::Greater => Instr::Cmp(Type::Long, Cmp::Sgt, left, right),
                    BinaryOperator::GreaterOrEqual => Instr::Cmp(Type::Long, Cmp::Sge, left, right),
                    BinaryOperator::Equal => Instr::Cmp(Type::Long, Cmp::Eq, left, right),
                    BinaryOperator::NotEqual => Instr::Cmp(Type::Long, Cmp::Ne, left, right),
                };
                self.func.assign_instr(temp.clone(), Type::Long, instr);
                temp
            }
            Expression::Call(func, args) => {
                let temp = self.get_temporary(assignment);
                let args = args
                    .iter()
                    .map(|arg| (Type::Long, self.expression(arg, None)))
                    .collect();
                self.func.assign_instr(
                    temp.clone(),
                    Type::Long,
                    Instr::Call(func.to_string(), args, None),
                );
                temp
            }
            Expression::Literal(literal) => {
                let value = Value::Const(*literal as u64);
                if let Some(assignment) = assignment {
                    self.func.assign_instr(
                        Value::Temporary(assignment.to_string()),
                        Type::Long,
                        Instr::Copy(value.clone()),
                    );
                }
                value
            }
            Expression::Variable(source) => {
                let value = Value::Temporary(source.to_string());
                if let Some(assignment) = assignment {
                    self.func.assign_instr(
                        Value::Temporary(assignment.to_string()),
                        Type::Long,
                        Instr::Copy(value.clone()),
                    );
                }
                value
            }
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

pub fn make_intermediate(ast: &ast::Module) -> qbe::Module<'static> {
    let mut state = State {
        il: qbe::Module::new(),
        func: Function::new(Linkage::public(), "main", Vec::new(), Some(Type::Word)),
        functions: Vec::new(),
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
            .map(|arg| (Type::Long, Value::Temporary(arg.to_string())))
            .collect();
        state.func = Function::new(Linkage::private(), name, args, Some(Type::Long));
        state.func.add_block("start");
        state.block(body);
        state.il.add_function(state.func);
    }
    state.il
}

pub fn compile_intermediate(module: &qbe::Module, output_path: Option<&Path>) {
    let mut qbe_process = Command::new("qbe")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    let qbe_stdout = qbe_process.stdout.take().unwrap();

    let mut cc_command = Command::new("cc");
    if let Some(output_path) = output_path {
        cc_command.arg("-o").arg(output_path);
    }
    let mut cc_process = cc_command
        .args(["-x", "assembler", "-"])
        .stdin(qbe_stdout)
        .spawn()
        .unwrap();

    let mut qbe_stdin = qbe_process.stdin.take().unwrap();
    write!(qbe_stdin, "{module}").unwrap();
    qbe_stdin.flush().unwrap();
    drop(qbe_stdin);

    assert!(qbe_process.wait().unwrap().success());
    assert!(cc_process.wait().unwrap().success());
}
