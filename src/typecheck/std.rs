use crate::ast;
use crate::parser::parse;
use crate::vir::Type;
use std::sync::LazyLock;

pub static STD_AST: LazyLock<ast::Module> = LazyLock::new(|| {
    let module = parse(include_str!("std.virtue")).unwrap();
    let ast::Statement::Function(main) = module.statements.into_iter().next().unwrap() else {
        unreachable!()
    };
    ast::Module {
        statements: main.body,
    }
});

pub const STRING: Type = Type::Struct(0, Vec::new());
