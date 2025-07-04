use std::fmt::Display;
use std::fs::File;
use std::path::{Path, PathBuf};
use virtue::codegen::Backend;
use virtue::error::terminal::ANSI_COLORS;
use virtue::error::{Error, ErrorFormat, github_actions, terminal};

struct Options {
    source_path: PathBuf,
    output_path: Option<PathBuf>,
    format: Format,
    backend: Backend,
    error_format: ErrorFormat,
}

#[derive(Eq, PartialEq)]
enum Format {
    DebugAst,
    DebugVir,
    #[cfg(feature = "c")]
    C,
    #[cfg(feature = "llvm")]
    LlvmIr,
    #[cfg(feature = "qbe")]
    QbeIl,
    Executable,
}

const USAGE: &str = r#"virtue [OPTIONS] file.virtue
    -h                prints this help
    -o file           output to file
    -f <format>       generate format among:
        debug-ast
        debug-vir
        c
        llvm-ir
        qbe-il
        executable (default)
    -b <backend>      generate backend among:
        c
        llvm (default)
        qbe
    -e <error format> generate error format among:
        github-actions (default if GITHUB_ACTIONS set)
        terminal (default otherwise)"#;

fn main() {
    let options = options();
    let source = std::fs::read_to_string(&options.source_path).unwrap();
    let output_path = options.output_path.as_ref().map(PathBuf::as_ref);

    let ast = match virtue::parser::parse(&source) {
        Ok(ast) => ast,
        Err(errors) => output_errors(&errors, &source, &options),
    };
    if options.format == Format::DebugAst {
        output(format_args!("{ast:#?}"), output_path);
    }

    let vir = match virtue::typecheck::typecheck(&ast) {
        Ok(vir) => vir,
        Err(errors) => output_errors(&errors, &source, &options),
    };
    if options.format == Format::DebugVir {
        output(format_args!("{vir:#?}"), output_path);
    }

    match options.backend {
        #[cfg(feature = "c")]
        Backend::C => {
            let c = virtue::codegen::c::make_c(&vir);
            if options.format == Format::C {
                output(c, output_path);
            }

            virtue::codegen::c::compile_c(&c, output_path).unwrap();
        }
        #[cfg(feature = "llvm")]
        Backend::Llvm => {
            let ctx = inkwell::context::Context::create();
            let ir = virtue::codegen::llvm::make_ir(&vir, &ctx);
            if options.format == Format::LlvmIr {
                let text_ir = ir.to_string();
                output(text_ir, output_path);
            }

            virtue::codegen::llvm::compile_ir(&ir, output_path).unwrap();
        }
        #[cfg(feature = "qbe")]
        Backend::Qbe => {
            let il = virtue::codegen::qbe::make_il(&vir);
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
    let mut error_format = None;

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
            } else if arg == "debug-vir" {
                Format::DebugVir
            } else if arg == "c" {
                #[cfg(not(feature = "c"))]
                error("c backend not enabled");
                #[cfg(feature = "c")]
                Format::C
            } else if arg == "llvm-ir" {
                #[cfg(not(feature = "llvm"))]
                error("llvm backend not enabled");
                #[cfg(feature = "llvm")]
                Format::LlvmIr
            } else if arg == "qbe-il" {
                #[cfg(not(feature = "qbe"))]
                error("qbe backend not enabled");
                #[cfg(feature = "qbe")]
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
            backend = Some(if arg == "c" {
                #[cfg(not(feature = "c"))]
                error("c backend not enabled");
                #[cfg(feature = "c")]
                Backend::C
            } else if arg == "llvm" {
                #[cfg(not(feature = "llvm"))]
                error("llvm backend not enabled");
                #[cfg(feature = "llvm")]
                Backend::Llvm
            } else if arg == "qbe" {
                #[cfg(not(feature = "qbe"))]
                error("qbe backend not enabled");
                #[cfg(feature = "qbe")]
                Backend::Qbe
            } else {
                error("option -b specified to unknown backend");
            });
        } else if arg == "-e" {
            if error_format.is_some() {
                error("option -e specified more than once");
            }
            let Some(arg) = args.next() else {
                error("option -e requires an argument");
            };
            error_format = Some(if arg == "github-actions" {
                ErrorFormat::GithubActions
            } else if arg == "terminal" {
                ErrorFormat::Terminal
            } else {
                error("option -e specified to unknown error format")
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
        #[cfg(feature = "c")]
        None if format == Format::C => Backend::C,
        #[cfg(feature = "qbe")]
        None if format == Format::QbeIl => Backend::Qbe,
        #[cfg(feature = "llvm")]
        None => Backend::Llvm,
        #[cfg(not(feature = "llvm"))]
        None => error("default llvm backend not enabled"),
    };
    let error_format = match error_format {
        Some(error_format) => error_format,
        None if std::env::var_os("GITHUB_ACTIONS").is_some() => ErrorFormat::GithubActions,
        None => ErrorFormat::Terminal,
    };

    #[cfg(feature = "c")]
    if format == Format::C && backend != Backend::C {
        error("format c requires c backend");
    }
    #[cfg(feature = "llvm")]
    if format == Format::LlvmIr && backend != Backend::Llvm {
        error("format llvm-ir requires llvm backend");
    }
    #[cfg(feature = "qbe")]
    if format == Format::QbeIl && backend != Backend::Qbe {
        error("format qbe-il requires qbe backend");
    }

    Options {
        source_path,
        output_path,
        format,
        backend,
        error_format,
    }
}

fn output_errors(errors: &[Error], source: &str, options: &Options) -> ! {
    match options.error_format {
        ErrorFormat::GithubActions => print!(
            "{}",
            github_actions::format_errors(errors, source, options.source_path.to_str().unwrap())
        ),
        ErrorFormat::Terminal => eprint!(
            "{}",
            terminal::format_errors(
                errors,
                source,
                options.source_path.to_str().unwrap(),
                &ANSI_COLORS
            )
        ),
    }
    std::process::exit(1);
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
