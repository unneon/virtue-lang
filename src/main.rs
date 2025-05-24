use std::fmt::Display;
use std::fs::File;
use std::path::{Path, PathBuf};

struct Options {
    source_path: PathBuf,
    output_path: Option<PathBuf>,
    format: Format,
    backend: Backend,
}

#[derive(Eq, PartialEq)]
enum Backend {
    Llvm,
    Qbe,
}

#[derive(Eq, PartialEq)]
enum Format {
    DebugAst,
    DebugHir,
    LlvmIr,
    QbeIl,
    Executable,
}

const USAGE: &str = r#"virtue [OPTIONS] file.virtue
    -h           prints this help
    -o file      output to file
    -f <format>  generate format among:
        debug-ast, debug-hir, llvm-ir, qbe-il, executable (default)
    -b <backend> generate backend among:
        llvm (default), qbe"#;

fn main() {
    let options = options();
    let source = std::fs::read_to_string(options.source_path).unwrap();
    let output_path = options.output_path.as_ref().map(PathBuf::as_ref);

    let ast = virtue::parser::parse(&source).unwrap();
    if options.format == Format::DebugAst {
        output(format_args!("{ast:#?}"), output_path);
    }

    let hir = virtue::typecheck::typecheck(&ast);
    if options.format == Format::DebugHir {
        output(format_args!("{hir:#?}"), output_path);
    }

    match options.backend {
        Backend::Llvm => {
            let ir = virtue::codegen::llvm::make_ir(&hir);
            if options.format == Format::LlvmIr {
                output(ir, output_path);
            }

            virtue::codegen::llvm::compile_ir(&ir, output_path).unwrap();
        }
        Backend::Qbe => {
            let il = virtue::codegen::qbe::make_il(&hir);
            if options.format == Format::QbeIl {
                output(il, output_path);
            }

            virtue::codegen::qbe::compile_il(&il, output_path).unwrap();
        }
    }
}

fn options() -> Options {
    let mut source_path = None;
    let mut output_path = None;
    let mut format = None;
    let mut backend = None;

    let mut args = std::env::args_os().skip(1);
    while let Some(arg) = args.next() {
        if arg == "-h" {
            println!("{USAGE}");
            std::process::exit(0);
        } else if arg == "-o" {
            if output_path.is_some() {
                error("option -o specified more than once");
            }
            let Some(arg) = args.next() else {
                error("option -o requires an argument");
            };
            output_path = Some(PathBuf::from(arg));
        } else if arg == "-f" {
            if format.is_some() {
                error("option -f specified more than once");
            }
            let Some(arg) = args.next() else {
                error("option -f requires an argument");
            };
            format = Some(if arg == "debug-ast" {
                Format::DebugAst
            } else if arg == "debug-hir" {
                Format::DebugHir
            } else if arg == "llvm-ir" {
                Format::LlvmIr
            } else if arg == "qbe-il" {
                Format::QbeIl
            } else if arg == "executable" {
                Format::Executable
            } else {
                error("option -f specified to unknown format");
            });
        } else if arg == "-b" {
            if backend.is_some() {
                error("option -b specified more than once");
            }
            let Some(arg) = args.next() else {
                error("option -b requires an argument");
            };
            backend = Some(if arg == "llvm" {
                Backend::Llvm
            } else if arg == "qbe" {
                Backend::Qbe
            } else {
                error("option -b specified to unknown backend");
            });
        } else if let Some(arg) = arg.to_str()
            && arg.starts_with('-')
        {
            error("unknown option specified");
        } else {
            if source_path.is_some() {
                error("more than one source file specified");
            }
            source_path = Some(PathBuf::from(arg));
        }
    }

    let Some(source_path) = source_path else {
        error("no source file specified");
    };
    let format = format.unwrap_or(Format::Executable);
    let backend = match backend {
        Some(backend) => backend,
        None if format == Format::QbeIl => Backend::Qbe,
        None => Backend::Llvm,
    };

    if format == Format::LlvmIr && backend != Backend::Llvm {
        error("format llvm-ir requires llvm backend");
    } else if format == Format::QbeIl && backend != Backend::Qbe {
        error("format qbe-il requires qbe backend");
    }

    Options {
        source_path,
        output_path,
        format,
        backend,
    }
}

fn output(data: impl Display, output_path: Option<&Path>) -> ! {
    let mut output_file;
    let output: &mut dyn std::io::Write = if let Some(output_path) = output_path {
        match File::create(output_path) {
            Ok(file) => {
                output_file = file;
                &mut output_file
            }
            Err(_) => error("failed to open output file"),
        }
    } else {
        &mut std::io::stdout().lock()
    };
    writeln!(output, "{data}").unwrap();
    std::process::exit(0);
}

fn error(message: &str) -> ! {
    eprintln!("virtue: {message}\n{USAGE}");
    std::process::exit(1);
}
