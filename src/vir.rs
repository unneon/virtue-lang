use crate::ast;
use crate::ast::{BinaryOperator, UnaryOperator};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<Function<'a>>,
    pub structs: Vec<Struct<'a>>,
    pub strings: Vec<&'a [&'a str]>,
}

#[derive(Debug)]
pub struct Function<'a> {
    pub exported: bool,
    pub is_main: bool,
    pub name: &'a str,
    pub args: Vec<Arg>,
    pub return_type: Type,
    pub bindings: Vec<BindingData>,
    pub binding_map: HashMap<&'a str, Binding>,
    pub blocks: Vec<Vec<Statement>>,
    pub ast_block: &'a [ast::Statement<'a>],
}

#[derive(Debug)]
pub struct Arg {
    pub binding: Binding,
}

#[derive(Debug)]
pub struct BindingData {
    pub type_: Type,
}

#[derive(Debug)]
pub enum Statement {
    Alloc(Binding, usize),
    Assignment(Binding, Binding),
    AssignmentField(Binding, usize, Binding),
    AssignmentIndex(Binding, Binding, Binding),
    BinaryOperator(Binding, BinaryOperator, Binding, Binding),
    Call(Binding, usize, Vec<Binding>),
    Field(Binding, Binding, usize),
    Index(Binding, Binding, Binding),
    JumpAlways(usize),
    JumpConditional {
        condition: Binding,
        true_block: usize,
        false_block: usize,
    },
    Literal(Binding, i64),
    New(Binding, usize),
    NewArray(Binding, Binding),
    Print(FormatString),
    Return(Binding),
    StringConstant(Binding, usize),
    Syscall(Binding, Vec<Binding>),
    UnaryOperator(Binding, UnaryOperator, Binding),
}

#[derive(Clone, Copy, Debug)]
pub struct Binding {
    pub id: usize,
}

#[derive(Debug)]
pub struct FormatString {
    pub segments: Vec<FormatSegment>,
}

#[derive(Debug)]
pub enum FormatSegment {
    Text(usize),
    Arg(Binding),
}

#[derive(Debug)]
pub struct Struct<'a> {
    pub name: &'a str,
    pub fields: Vec<Type>,
    pub field_map: HashMap<&'a str, usize>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Type {
    pub predicates: Vec<usize>,
    pub base: BaseType,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BaseType {
    Array(Box<Type>),
    I64,
    I32,
    I8,
    PointerI8,
    Struct(usize),
}

impl Program<'_> {
    pub fn string_len(&self, id: usize) -> usize {
        self.strings[id].iter().map(|s| s.len()).sum()
    }
}

impl Type {
    pub const I64: Type = Type {
        predicates: Vec::new(),
        base: BaseType::I64,
    };
    pub const I32: Type = Type {
        predicates: Vec::new(),
        base: BaseType::I32,
    };

    pub fn unwrap_list(&self) -> &Type {
        match &self.base {
            BaseType::Array(i) => i,
            _ => panic!("expected array, got {self:?}"),
        }
    }

    pub fn unwrap_struct(&self) -> usize {
        match &self.base {
            BaseType::Struct(i) => *i,
            _ => panic!("expected struct, got {self:?}"),
        }
    }

    pub fn dereference(&self) -> Type {
        match &self.base {
            BaseType::PointerI8 => BaseType::I8.into(),
            BaseType::Array(element_type) => element_type.as_ref().clone(),
            _ => panic!("expected pointer, got {self:?}"),
        }
    }

    pub fn byte_size(&self) -> usize {
        match &self.base {
            BaseType::Array(_) => 8,
            BaseType::I64 => 8,
            BaseType::I32 => 4,
            BaseType::I8 => 1,
            BaseType::PointerI8 => 8,
            // TODO: QBE and LLVM work differently here.
            BaseType::Struct(_) => 8,
        }
    }
}

impl From<BaseType> for Type {
    fn from(base: BaseType) -> Type {
        Type {
            predicates: Vec::new(),
            base,
        }
    }
}
