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
    pub bindings: Vec<BindingData>,
    pub binding_map: HashMap<&'a str, Binding>,
    pub blocks: Vec<Vec<Statement<'a>>>,
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
pub enum Statement<'a> {
    Assignment(Binding, Binding),
    AssignmentField(Binding, usize, Binding),
    AssignmentIndex(Binding, Binding, Binding),
    BinaryOperator(Binding, ast::BinaryOperator, Binding, Binding),
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
    NewArray(Binding, Type, Binding),
    Print(FormatString<'a>),
    Return(Binding),
    StringConstant(Binding, usize),
}

#[derive(Clone, Copy, Debug)]
pub struct Binding {
    pub id: usize,
}

#[derive(Debug)]
pub struct FormatString<'a> {
    pub segments: Vec<FormatSegment<'a>>,
}

#[derive(Debug)]
pub enum FormatSegment<'a> {
    Text(&'a str),
    Arg(Binding),
}

#[derive(Debug)]
pub struct Struct<'a> {
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
    String,
    Struct(usize),
}

impl FormatString<'_> {
    pub fn printf_format(&self, function: &Function, suffix: &str) -> String {
        let mut fmt = String::new();
        for segment in &self.segments {
            fmt.push_str(match segment {
                FormatSegment::Text(text) => text,
                FormatSegment::Arg(arg) => function.bindings[arg.id].type_.printf_format(),
            });
        }
        fmt.push_str(suffix);
        fmt
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

    fn printf_format(&self) -> &'static str {
        match self.base {
            BaseType::I64 => "%lld",
            BaseType::I32 => "%d",
            BaseType::String => "%s",
            BaseType::Array(_) | BaseType::Struct(_) => panic!("print not supported for {self:?}"),
        }
    }

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
}

impl From<BaseType> for Type {
    fn from(base: BaseType) -> Type {
        Type {
            predicates: Vec::new(),
            base,
        }
    }
}
