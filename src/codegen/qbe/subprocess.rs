use crate::util::tempfile;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

pub fn compile_il(module: &qbe::Module, output_path: Option<&Path>) -> Result<(), String> {
    let (_object_file, object_path) = tempfile();
    let assembly = qbe(module)?;
    as_(&assembly, &object_path)?;
    ld(&object_path, output_path)?;
    Ok(())
}

fn qbe(module: &qbe::Module) -> Result<Vec<u8>, String> {
    let mut child = Command::new("qbe")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let mut stdin = child.stdin.take().unwrap();
    write!(stdin, "{module}").unwrap();
    drop(stdin);

    let output = child.wait_with_output().unwrap();
    if !output.status.success() {
        return Err(String::from_utf8(output.stderr).unwrap());
    }
    Ok(output.stdout)
}

fn as_(assembly: &[u8], object_path: &Path) -> Result<(), String> {
    let mut child = Command::new("as")
        .arg("-o")
        .arg(object_path)
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(assembly).unwrap();
    stdin.write_all(include_bytes!("syscall.s")).unwrap();
    drop(stdin);

    let output = child.wait_with_output().unwrap();
    if !output.status.success() {
        return Err(String::from_utf8(output.stderr).unwrap());
    }
    Ok(())
}

fn ld(object_path: &Path, output_path: Option<&Path>) -> Result<(), String> {
    let mut ld_command = Command::new("ld");
    ld_command.arg(object_path);
    if let Some(output_path) = output_path {
        ld_command.arg("-o").arg(output_path);
    }

    let ld_output = ld_command.output().unwrap();
    if !ld_output.status.success() {
        return Err(String::from_utf8(ld_output.stderr).unwrap());
    }
    Ok(())
}
