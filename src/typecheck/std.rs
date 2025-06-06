use crate::ast;
use crate::parser::parse;
use crate::vir::{BaseType, Type};
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

pub const I64: Type = Type {
    predicates: Vec::new(),
    base: BaseType::I64,
};

pub const I8: Type = Type {
    predicates: Vec::new(),
    base: BaseType::I8,
};

pub const BOOL: Type = Type {
    predicates: Vec::new(),
    base: BaseType::Bool,
};

pub const STRING: Type = Type {
    predicates: Vec::new(),
    base: BaseType::Struct(0, Vec::new()),
};

pub const VOID: Type = Type {
    predicates: Vec::new(),
    base: BaseType::Void,
};

pub const ERROR: Type = Type {
    predicates: Vec::new(),
    base: BaseType::Error,
};
