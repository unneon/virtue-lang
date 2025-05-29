use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

pub fn compile_ir(ir: &str, output_path: Option<&Path>) -> Result<(), String> {
    let object_path = tempfile::NamedTempFile::new().unwrap().into_temp_path();
    llc(ir, &object_path)?;
    ld(&object_path, output_path)?;
    Ok(())
}

fn llc(ir: &str, object_path: &Path) -> Result<(), String> {
    let mut child = Command::new("llc")
        .args(["--filetype", "obj", "-", "-o"])
        .arg(object_path)
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(ir.as_bytes()).unwrap();
    drop(stdin);

    let output = child.wait_with_output().unwrap();
    if !output.status.success() {
        return Err(String::from_utf8(output.stderr).unwrap());
    }
    Ok(())
}

fn ld(object_path: &Path, output_path: Option<&Path>) -> Result<(), String> {
    let mut command = Command::new("ld");
    command.arg(object_path);
    if let Some(output_path) = output_path {
        command.arg("-o").arg(output_path);
    }
    command.stderr(Stdio::piped());

    let output = command.output().unwrap();
    if !output.status.success() {
        return Err(String::from_utf8(output.stderr).unwrap());
    }
    Ok(())
}
