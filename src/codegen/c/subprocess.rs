use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

pub fn compile_c(c: &str, output_path: Option<&Path>) -> Result<(), String> {
    let mut command = Command::new("cc");
    if let Some(output_path) = output_path {
        command.arg("-o").arg(output_path);
    }
    let mut child = command
        .args(["-nostdlib", "-static", "-x", "c", "-"])
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(c.as_bytes()).unwrap();
    drop(stdin);

    let output = child.wait_with_output().unwrap();

    if !output.status.success() {
        return Err(String::from_utf8(output.stderr).unwrap());
    }
    Ok(())
}
