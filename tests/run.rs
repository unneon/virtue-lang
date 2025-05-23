use libtest_mimic::{Arguments, Failed, Trial};
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::process::{Command, ExitCode, Stdio};

enum Backend {
    Llvm,
    Qbe,
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
            let directives: Vec<_> = source
                .lines()
                .rev()
                .take_while(|line| line.starts_with("#"))
                .collect();
            let entry2 = entry.clone();
            tests.push(
                Trial::test(format!("{name}::llvm"), move || {
                    run_test(entry, Backend::Llvm)
                })
                .with_ignored_flag(directives.contains(&"# ignore-llvm")),
            );
            tests.push(
                Trial::test(format!("{name}::qbe"), move || {
                    run_test(entry2, Backend::Qbe)
                })
                .with_ignored_flag(directives.contains(&"# ignore-qbe")),
            );
        }
    }
    Ok(tests)
}

fn run_test(path: PathBuf, backend: Backend) -> Result<(), Failed> {
    let source = fs::read_to_string(&path).unwrap();
    let expected_stdout = fs::read_to_string(path.with_extension("stdout")).unwrap();
    let ast = virtue::parse(&source);
    let intermediate_name = match backend {
        Backend::Llvm => "LLVM IR",
        Backend::Qbe => "QBE IL",
    };
    let output_path = tempfile::NamedTempFile::new().unwrap().into_temp_path();
    let intermediate = match backend {
        Backend::Llvm => {
            let ir = virtue::codegen::llvm::make_ir(&ast);
            if let Err(e) = virtue::codegen::llvm::compile_ir(&ir, Some(&output_path)) {
                return Err(format!(
                    "\x1B[1m{intermediate_name}:\x1B[0m\n{ir}\n\x1B[1mllvm error:\x1B[0m\n{e}"
                )
                .into());
            }
            ir
        }
        Backend::Qbe => {
            let il = virtue::codegen::qbe::make_il(&ast);
            if let Err(e) = virtue::codegen::qbe::compile_il(&il, Some(&output_path)) {
                return Err(format!(
                    "\x1B[1m{intermediate_name}:\x1B[0m\n{il}\n\x1B[1mqbe error:\x1B[0m\n{e}"
                )
                .into());
            }
            il.to_string()
        }
    };
    let child = Command::new(&output_path)
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(output.status.success());
    let actual_stdout = String::from_utf8(output.stdout).unwrap();
    if actual_stdout != expected_stdout {
        return Err(format!("\x1B[1m{intermediate_name}:\x1B[0m\n{intermediate}\n\x1B[1;31mactual stdout:\x1B[0m\n{actual_stdout}\n\x1B[1;32mexpected stdout:\x1B[0m\n{expected_stdout}").into());
    }
    Ok(())
}
