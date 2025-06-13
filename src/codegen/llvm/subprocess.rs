use crate::util::tempfile;
use inkwell::OptimizationLevel;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use std::path::Path;
use std::process::{Command, Stdio};

pub fn compile_ir(ir: &Module, output_path: Option<&Path>) -> Result<(), String> {
    let object_file = tempfile();
    ir_to_object(ir, object_file.path());
    ld(object_file.path(), output_path)?;
    Ok(())
}

fn ir_to_object(ir: &Module, object_path: &Path) {
    Target::initialize_x86(&InitializationConfig::default());
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64"),
            "",
            "",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();
    target_machine
        .write_to_file(ir, FileType::Object, object_path)
        .unwrap();
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
