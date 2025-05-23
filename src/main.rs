fn main() {
    let source_path = std::env::args().nth(1).unwrap();
    let source = std::fs::read_to_string(source_path).unwrap();
    let ast = virtue::parser::parse(&source).unwrap();
    let hir = virtue::typecheck::typecheck(&ast);
    // let il = virtue::codegen::qbe::make_il(&ast);
    // virtue::codegen::qbe::compile_il(&il, None).unwrap();
    let ir = virtue::codegen::llvm::make_ir(&hir);
    virtue::codegen::llvm::compile_ir(&ir, None).unwrap();
}
