use crate::ast;
use crate::ast::{FormatSegment, Statement};
use qbe::{DataDef, DataItem, Function, Instr, Linkage, Type, Value};
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

pub fn make_intermediate(ast: &ast::Module) -> qbe::Module<'static> {
    let mut il = qbe::Module::new();
    let mut main = Function::new(Linkage::public(), "main", Vec::new(), Some(Type::Word));
    main.add_block("start");

    let mut print_counter = 0;

    for statement in &ast.statements {
        match statement {
            Statement::Print { fmt } => {
                let print_id = print_counter;
                print_counter += 1;

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

                il.add_data(DataDef::new(
                    Linkage::private(),
                    format!("print_{print_id}"),
                    None,
                    vec![
                        (Type::Byte, DataItem::Str(c_fmt)),
                        (Type::Byte, DataItem::Const(0)),
                    ],
                ));

                main.add_instr(Instr::Call("printf".into(), c_args, None));
            }
            _ => todo!("{statement:?}"),
        }
    }

    main.add_instr(Instr::Ret(Some(Value::Const(0))));
    il.add_function(main);
    il
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
