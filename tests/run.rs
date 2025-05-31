#![feature(string_into_chars)]

use libtest_mimic::{Arguments, Failed, Trial};
use std::collections::HashSet;
use std::error::Error;
use std::fs;
use std::process::{Command, ExitCode, Stdio};
use std::sync::Arc;
use virtue::codegen::{ALL_BACKENDS, Backend};
use virtue::util::tempfile;

struct TestFile {
    kind: TestKind,
    name: String,
    source: String,
    directives: Directives,
}

#[derive(Clone)]
enum TestKind {
    Pass,
    Fail,
}

struct Directives {
    output: String,
    ignore: bool,
    ignore_backend: HashSet<Backend>,
}

fn main() -> Result<ExitCode, Box<dyn Error>> {
    let args = Arguments::from_args();
    let tests = find_tests()?;
    Ok(libtest_mimic::run(&args, tests).exit_code())
}

fn find_tests() -> Result<Vec<Trial>, Box<dyn Error>> {
    let mut tests = Vec::new();
    for file in walk_test_files() {
        match file.kind {
            TestKind::Pass => {
                let file = Arc::new(file);
                for backend in ALL_BACKENDS {
                    let name = &file.name;
                    let backend_name = match backend {
                        Backend::C => "c",
                        Backend::Llvm => "llvm",
                        Backend::Qbe => "qbe",
                    };
                    let name = format!("pass::{name}::{backend_name}");
                    let file = Arc::clone(&file);
                    let ignore =
                        file.directives.ignore || file.directives.ignore_backend.contains(&backend);
                    let runner = move || run_pass(file, Backend::C);
                    let trial = Trial::test(name, runner).with_ignored_flag(ignore);
                    tests.push(trial);
                }
            }
            TestKind::Fail => {
                let name = &file.name;
                let name = format!("fail::{name}");
                let runner = move || run_fail(file);
                let trial = Trial::test(name, runner);
                tests.push(trial);
            }
        }
    }
    Ok(tests)
}

fn walk_test_files() -> impl Iterator<Item = TestFile> {
    let pass = fs::read_dir("tests/pass")
        .unwrap()
        .zip(std::iter::repeat(TestKind::Pass));
    let fail = fs::read_dir("tests/fail")
        .unwrap()
        .zip(std::iter::repeat(TestKind::Fail));
    pass.chain(fail).map(|(entry, kind)| {
        let path = entry.unwrap().path();
        let stem = path.file_stem().unwrap().to_str().unwrap();
        let name = stem.replace('-', "_");
        let source = fs::read_to_string(&path).unwrap();
        let directives = parse_directives(&source);
        TestFile {
            kind,
            name,
            source,
            directives,
        }
    })
}

fn parse_directives(source: &str) -> Directives {
    let lines: Vec<_> = source
        .lines()
        .rev()
        .take_while(|line| line.starts_with("#"))
        .collect();
    let mut directives = Directives {
        output: String::new(),
        ignore: false,
        ignore_backend: HashSet::new(),
    };
    for line in lines.into_iter().rev() {
        if line == "# ignore" {
            directives.ignore = true;
        } else if line == "# ignore-c" {
            directives.ignore_backend.insert(Backend::C);
        } else if line == "# ignore-llvm" {
            directives.ignore_backend.insert(Backend::Llvm);
        } else if line == "# ignore-qbe" {
            directives.ignore_backend.insert(Backend::Qbe);
        } else {
            directives.output += line.strip_prefix("# ").unwrap();
            directives.output.push('\n');
        }
    }
    directives
}

fn run_pass(test: Arc<TestFile>, backend: Backend) -> Result<(), Failed> {
    let ast = match virtue::parser::parse(&test.source) {
        Ok(ast) => ast,
        Err(e) => return Err(format!("\x1B[1mparse error:\x1B[0m\n{e}").into()),
    };
    let vir = virtue::typecheck::typecheck(&ast).unwrap();
    let intermediate_name = match backend {
        Backend::C => "C",
        Backend::Llvm => "LLVM IR",
        Backend::Qbe => "QBE IL",
    };
    let output_file = tempfile();
    let intermediate = match backend {
        Backend::C => {
            let c = virtue::codegen::c::make_c(&vir);
            if let Err(e) = virtue::codegen::c::compile_c(&c, Some(output_file.path())) {
                return Err(format!(
                    "\x1B[1m{intermediate_name}:\x1B[0m\n{c}\n\x1B[1mc error:\x1B[0m\n{e}"
                )
                .into());
            }
            c
        }
        Backend::Llvm => {
            let ir = virtue::codegen::llvm::make_ir(&vir);
            if let Err(e) = virtue::codegen::llvm::compile_ir(&ir, Some(output_file.path())) {
                return Err(format!(
                    "\x1B[1m{intermediate_name}:\x1B[0m\n{ir}\n\x1B[1mllvm error:\x1B[0m\n{e}"
                )
                .into());
            }
            ir
        }
        Backend::Qbe => {
            let il = virtue::codegen::qbe::make_il(&vir);
            if let Err(e) = virtue::codegen::qbe::compile_il(&il, Some(output_file.path())) {
                return Err(format!(
                    "\x1B[1m{intermediate_name}:\x1B[0m\n{il}\n\x1B[1mqbe error:\x1B[0m\n{e}"
                )
                .into());
            }
            il.to_string()
        }
    };
    let child = Command::new(output_file.path())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    let output = child.wait_with_output().unwrap();
    if !output.status.success() {
        return Err(format!(
            "\x1B[1m{intermediate_name}:\x1B[0m\n{intermediate}\n\x1B[1;31mcrash\x1B[0m"
        )
        .into());
    }
    assert!(output.status.success());
    let actual_stdout = String::from_utf8_lossy(&output.stdout);
    let expected_stdout = &test.directives.output;
    if actual_stdout != *expected_stdout {
        return Err(format!("\x1B[1m{intermediate_name}:\x1B[0m\n{intermediate}\n\x1B[1;31mactual stdout:\x1B[0m\n{actual_stdout}\n\x1B[1;32mexpected stdout:\x1B[0m\n{expected_stdout}").into());
    }
    Ok(())
}

fn run_fail(test: TestFile) -> Result<(), Failed> {
    let ast = match virtue::parser::parse(&test.source) {
        Ok(ast) => ast,
        Err(_) => todo!(),
    };
    let vir = match virtue::typecheck::typecheck(&ast) {
        Ok(vir) => vir,
        Err(errors) => {
            let actual_stderr: String = errors
                .iter()
                .flat_map(|error| format!("{}\n", error.message).into_chars())
                .collect();
            let expected_stderr = &test.directives.output;
            if actual_stderr != *expected_stderr {
                return Err(format!("\x1B[1;31mactual stderr:\x1B[0m\n{actual_stderr}\n\x1B[1;32mexpected stderr:\x1B[0m\n{expected_stderr}").into());
            }
            return Ok(());
        }
    };
    Err(format!("\x1B[1mVIR:\x1B[0m\n{vir:?}").into())
}
