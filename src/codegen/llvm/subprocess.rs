use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

pub fn compile_ir(ir: &str, output_path: Option<&Path>) -> Result<(), String> {
    let object_path = tempfile::NamedTempFile::new().unwrap().into_temp_path();

    let mut llc_child = Command::new("llc")
        .args(["--filetype", "obj", "-", "-o"])
        .arg(&object_path)
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let mut llc_stdin = llc_child.stdin.take().unwrap();
    llc_stdin.write_all(ir.as_bytes()).unwrap();
    drop(llc_stdin);

    let llc_output = llc_child.wait_with_output().unwrap();
    if !llc_output.status.success() {
        return Err(String::from_utf8(llc_output.stderr).unwrap());
    }

    let mut lld_command = Command::new("ld.lld");
    lld_command.arg(&object_path);
    if let Some(output_path) = output_path {
        lld_command.arg("-o").arg(output_path);
    }
    let lld_output = lld_command.output().unwrap();
    assert!(lld_output.status.success());

    Ok(())
}
