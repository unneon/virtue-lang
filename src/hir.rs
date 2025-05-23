use crate::ast;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<Function<'a>>,
    pub structs: Vec<Struct<'a>>,
    pub strings: Vec<&'a str>,
}

#[derive(Debug)]
pub struct Function<'a> {
    pub exported: bool,
    pub name: &'a str,
    pub args: Vec<Arg>,
    pub return_type: Type,
    pub bindings: Vec<Binding>,
    pub binding_map: HashMap<&'a str, usize>,
    pub blocks: Vec<Vec<Statement<'a>>>,
    pub ast_block: &'a [ast::Statement<'a>],
}

#[derive(Debug)]
pub struct Arg {
    pub binding: usize,
}

#[derive(Debug)]
pub struct Binding {
    pub type_: Option<Type>,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Assignment(usize, usize),
    AssignmentField(usize, usize, usize),
    BinaryOperator(usize, ast::BinaryOperator, usize, usize),
    Call(usize, usize, Vec<usize>),
    Field(usize, usize, usize),
    JumpAlways(usize),
    JumpConditional {
        condition: usize,
        true_block: usize,
        false_block: usize,
    },
    Literal(usize, i64),
    New(usize, usize),
    Print(Vec<FormatSegment<'a>>),
    Return(usize),
    StringConstant(usize, usize),
}

#[derive(Debug)]
pub enum FormatSegment<'a> {
    Text(&'a str),
    Arg(usize),
}

#[derive(Debug)]
pub struct Struct<'a> {
    pub fields: Vec<Type>,
    pub field_map: HashMap<&'a str, usize>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    I64,
    I32,
    String,
    Struct(usize),
}
