mod subprocess;

pub use subprocess::compile_ir;

use crate::ast;

pub fn make_ir(_ast: &ast::Module) -> String {
    use std::fmt::Write;

    let mut ir = String::new();
    write!(
        &mut ir,
        r#"declare i32 @printf(i8*, ...)

@fmt = internal constant [15 x i8] c"Hello, world!\0A\00"

define i32 @main() {{
    %fmt = getelementptr [15 x i8], [15 x i8]* @fmt, i32 0, i32 0
    call i32 (i8*, ...) @printf(i8* %fmt)
    ret i32 0
}}
"#
    )
    .unwrap();
    ir
}
