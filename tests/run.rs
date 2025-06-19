#![feature(string_into_chars)]

use libtest_mimic::{Arguments, Failed, Trial};
use std::collections::HashSet;
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::process::{Command, ExitCode, Stdio};
use std::sync::Arc;
use virtue::codegen::{ALL_BACKENDS, Backend};
use virtue::error::terminal::{NO_COLORS, format_errors};
use virtue::util::tempfile;

struct TestFile {
    kind: TestKind,
    name: String,
    source: String,
    directives: Directives,
    path: PathBuf,
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
                        #[cfg(feature = "c")]
                        Backend::C => "c",
                        #[cfg(feature = "llvm")]
                        Backend::Llvm => "llvm",
                        #[cfg(feature = "qbe")]
                        Backend::Qbe => "qbe",
                    };
                    let name = format!("pass::{name}::{backend_name}");
                    let file = Arc::clone(&file);
                    let ignore =
                        file.directives.ignore || file.directives.ignore_backend.contains(&backend);
                    let runner = move || run_pass(file, *backend);
                    let trial = Trial::test(name, runner).with_ignored_flag(ignore);
                    tests.push(trial);
                }
            }
            TestKind::Fail => {
                let name = &file.name;
                let name = format!("fail::{name}");
                let ignore = file.directives.ignore;
                let runner = move || run_fail(file);
                let trial = Trial::test(name, runner).with_ignored_flag(ignore);
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
            path,
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
            #[cfg(feature = "c")]
            directives.ignore_backend.insert(Backend::C);
        } else if line == "# ignore-llvm" {
            #[cfg(feature = "llvm")]
            directives.ignore_backend.insert(Backend::Llvm);
        } else if line == "# ignore-qbe" {
            #[cfg(feature = "qbe")]
            directives.ignore_backend.insert(Backend::Qbe);
        } else if let Some(line) = line.strip_prefix("# ") {
            directives.output += line;
            directives.output.push('\n');
        } else if line == "#" {
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
    let vir = match virtue::typecheck::typecheck(&ast) {
        Ok(vir) => vir,
        Err(e) => {
            let e = format_errors(&e, &test.source, "test.virtue", &NO_COLORS);
            return Err(format!("\x1B[1mtypecheck error:\x1B[0m\n{e}").into());
        }
    };
    let intermediate_name = match backend {
        #[cfg(feature = "c")]
        Backend::C => "C",
        #[cfg(feature = "llvm")]
        Backend::Llvm => "LLVM IR",
        #[cfg(feature = "qbe")]
        Backend::Qbe => "QBE IL",
    };
    let output_file = tempfile();
    let intermediate = match backend {
        #[cfg(feature = "c")]
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
        #[cfg(feature = "llvm")]
        Backend::Llvm => {
            let ctx = inkwell::context::Context::create();
            let ir = virtue::codegen::llvm::make_ir(&vir, &ctx);
            if let Err(e) = virtue::codegen::llvm::compile_ir(&ir, Some(output_file.path())) {
                let ir = ir.to_string();
                return Err(format!(
                    "\x1B[1m{intermediate_name}:\x1B[0m\n{ir}\n\x1B[1mllvm error:\x1B[0m\n{e}"
                )
                .into());
            }
            ir.to_string()
        }
        #[cfg(feature = "qbe")]
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
            let actual_stderr: String =
                format_errors(&errors, &test.source, "test.virtue", &NO_COLORS);
            let expected_stderr = &test.directives.output;
            if actual_stderr != *expected_stderr {
                if std::env::var("VIRTUE_LANG_BLESS")
                    .ok()
                    .as_ref()
                    .map(String::as_str)
                    == Some("1")
                {
                    let source = &test.source;
                    let directives = actual_stderr
                        .lines()
                        .map(|line| {
                            if line.is_empty() {
                                "#\n".to_owned()
                            } else {
                                format!("# {line}\n")
                            }
                        })
                        .collect::<Vec<_>>()
                        .join("");
                    fs::write(test.path, format!("{source}\n{directives}")).unwrap();
                } else {
                    return Err(format!("\x1B[1;31mactual stderr:\x1B[0m\n{actual_stderr}\n\x1B[1;32mexpected stderr:\x1B[0m\n{expected_stderr}").into());
                }
            }
            return Ok(());
        }
    };
    Err(format!("\x1B[1mVIR:\x1B[0m\n{vir:?}").into())
}
