use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

pub fn compile_il(module: &qbe::Module, output_path: Option<&Path>) -> Result<(), String> {
    let mut qbe_process = Command::new("qbe")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
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
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let mut qbe_stdin = qbe_process.stdin.take().unwrap();
    write!(qbe_stdin, "{module}").unwrap();
    qbe_stdin.flush().unwrap();
    drop(qbe_stdin);

    let qbe_output = qbe_process.wait_with_output().unwrap();
    let cc_output = cc_process.wait_with_output().unwrap();

    if !qbe_output.status.success() {
        return Err(String::from_utf8(qbe_output.stderr).unwrap());
    }
    if !cc_output.status.success() {
        return Err(String::from_utf8(cc_output.stderr).unwrap());
    }
    Ok(())
}
