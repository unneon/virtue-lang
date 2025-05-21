fn main() {
    let source_path = std::env::args().nth(1).unwrap();
    let source = std::fs::read_to_string(source_path).unwrap();
    let ast = virtue::parse(&source);
    let stdout = virtue::interpret(&ast);
    print!("{stdout}");
}
