use libtest_mimic::{Arguments, Failed, Trial};
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::process::{Command, ExitCode, Stdio};
use std::sync::Arc;
use virtue::util::tempfile;

enum Backend {
    C,
    Llvm,
    Qbe,
}

struct Directives {
    stdout: String,
    ignore: bool,
    ignore_c: bool,
    ignore_llvm: bool,
    ignore_qbe: bool,
}

fn main() -> Result<ExitCode, Box<dyn Error>> {
    let args = Arguments::from_args();
    let tests = find_tests()?;
    Ok(libtest_mimic::run(&args, tests).exit_code())
}

fn find_tests() -> Result<Vec<Trial>, Box<dyn Error>> {
    let mut tests = Vec::new();
    for entry in fs::read_dir("tests").unwrap() {
        let entry = entry.unwrap().path();
        if entry.extension().unwrap() == "virtue" {
            let stem = entry.file_stem().unwrap().to_str().unwrap();
            let name = stem.replace('-', "_");
            let source = fs::read_to_string(&entry).unwrap();
            let directives = parse_directives(&source);
            let entry2 = entry.clone();
            let entry3 = entry.clone();
            let stdout = Arc::new(directives.stdout);
            let stdout2 = stdout.clone();
            let stdout3 = stdout.clone();
            tests.push(
                Trial::test(format!("{name}::c"), move || {
                    run_test(entry, Backend::C, stdout)
                })
                .with_ignored_flag(directives.ignore || directives.ignore_c),
            );
            tests.push(
                Trial::test(format!("{name}::llvm"), move || {
                    run_test(entry2, Backend::Llvm, stdout2)
                })
                .with_ignored_flag(directives.ignore || directives.ignore_llvm),
            );
            tests.push(
                Trial::test(format!("{name}::qbe"), move || {
                    run_test(entry3, Backend::Qbe, stdout3)
                })
                .with_ignored_flag(directives.ignore || directives.ignore_qbe),
            );
        }
    }
    Ok(tests)
}

fn parse_directives(source: &str) -> Directives {
    let lines: Vec<_> = source
        .lines()
        .rev()
        .take_while(|line| line.starts_with("#"))
        .collect();
    let mut directives = Directives {
        stdout: String::new(),
        ignore: false,
        ignore_c: false,
        ignore_llvm: false,
        ignore_qbe: false,
    };
    for line in lines.into_iter().rev() {
        if line == "# ignore" {
            directives.ignore = true;
        } else if line == "# ignore-c" {
            directives.ignore_c = true;
        } else if line == "# ignore-llvm" {
            directives.ignore_llvm = true;
        } else if line == "# ignore-qbe" {
            directives.ignore_qbe = true;
        } else {
            directives.stdout += line.strip_prefix("# ").unwrap();
            directives.stdout.push('\n');
        }
    }
    directives
}

fn run_test(path: PathBuf, backend: Backend, expected_stdout: Arc<String>) -> Result<(), Failed> {
    let source = fs::read_to_string(&path).unwrap();
    let ast = match virtue::parser::parse(&source) {
        Ok(ast) => ast,
        Err(e) => return Err(format!("\x1B[1mparse error:\x1B[0m\n{e}").into()),
    };
    let intermediate_name = match backend {
        Backend::C => "C",
        Backend::Llvm => "LLVM IR",
        Backend::Qbe => "QBE IL",
    };
    let output_file = tempfile();
    let intermediate = match backend {
        Backend::C => {
            let vir = virtue::typecheck::typecheck(&ast);
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
            let vir = virtue::typecheck::typecheck(&ast);
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
            let vir = virtue::typecheck::typecheck(&ast);
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
    if actual_stdout != *expected_stdout {
        return Err(format!("\x1B[1m{intermediate_name}:\x1B[0m\n{intermediate}\n\x1B[1;31mactual stdout:\x1B[0m\n{actual_stdout}\n\x1B[1;32mexpected stdout:\x1B[0m\n{expected_stdout}").into());
    }
    Ok(())
}
