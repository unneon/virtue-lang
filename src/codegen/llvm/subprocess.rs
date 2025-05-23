use std::path::Path;
use std::process::{Command, Stdio};

pub fn compile_ir(ir: &str, output_path: Option<&Path>) -> Result<(), String> {
    use std::io::Write;

    let mut llvm_as_process = Command::new("llvm-as")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let llvm_as_stdout = llvm_as_process.stdout.take().unwrap();

    let mut clang_command = Command::new("clang");
    if let Some(output_path) = output_path {
        clang_command.arg("-o").arg(output_path);
    }
    let clang_process = clang_command
        .args(["-x", "ir", "-"])
        .stdin(llvm_as_stdout)
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let mut llvm_as_stdin = llvm_as_process.stdin.take().unwrap();
    llvm_as_stdin.write_all(ir.as_bytes()).unwrap();
    llvm_as_stdin.flush().unwrap();
    drop(llvm_as_stdin);

    let llvm_as_output = llvm_as_process.wait_with_output().unwrap();
    let clang_output = clang_process.wait_with_output().unwrap();

    if !llvm_as_output.status.success() {
        return Err(String::from_utf8(llvm_as_output.stderr).unwrap());
    }
    if !clang_output.status.success() {
        return Err(String::from_utf8_lossy(&clang_output.stderr).into_owned());
    }
    Ok(())
}
