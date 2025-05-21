use libtest_mimic::{Arguments, Failed, Trial};
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

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
            tests.push(Trial::test(name, move || run_test(entry)));
        }
    }
    Ok(tests)
}

fn run_test(path: PathBuf) -> Result<(), Failed> {
    let source = fs::read_to_string(&path).unwrap();
    let expected_stdout = fs::read_to_string(path.with_extension("stdout")).unwrap();
    let ast = virtue::parse(&source);
    let actual_stdout = virtue::interpret(&ast);
    if actual_stdout != expected_stdout {
        return Err(format!("{ast:#?}\n\n\x1B[1;31mactual stdout:\x1B[0m\n{actual_stdout}\n\n\x1B[1;32mexpected stdout:\x1B[0m\n{expected_stdout}\n").into());
    }
    Ok(())
}
