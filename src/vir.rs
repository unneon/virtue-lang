use crate::ast;
use crate::ast::{BinaryOperator, UnaryOperator};
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<Function<'a>>,
    pub structs: Vec<Struct<'a>>,
    pub strings: Vec<&'a [&'a str]>,
}

#[derive(Clone, Debug)]
pub struct Function<'a> {
    pub exported: bool,
    pub is_main: bool,
    pub is_fully_substituted: bool,
    pub name: Cow<'a, str>,
    pub value_args: Vec<Arg>,
    pub type_args: Vec<()>,
    pub all_args: Vec<AnyArg>,
    pub return_type: Type,
    pub bindings: Vec<BindingData>,
    pub binding_map: HashMap<&'a str, Binding>,
    pub type_variable_map: HashMap<&'a str, usize>,
    pub blocks: Vec<Vec<Statement>>,
    pub ast_block: &'a [ast::Statement<'a>],
}

#[derive(Clone, Debug)]
pub struct Arg {
    pub binding: Binding,
}

#[derive(Clone, Debug)]
pub enum AnyArg {
    Value(usize),
    Type(usize),
}

#[derive(Clone, Debug)]
pub struct BindingData {
    pub type_: Type,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Alloc(Binding, Binding),
    Assignment(Binding, Binding),
    AssignmentField(Binding, usize, Binding),
    AssignmentIndex(Binding, Binding, Binding),
    BinaryOperator(Binding, BinaryOperator, Binding, Binding),
    Call {
        return_: Option<Binding>,
        function: usize,
        type_substitutions: Vec<Type>,
        args: Vec<Binding>,
    },
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
    Return(Option<Binding>),
    StringConstant(Binding, usize),
    Syscall(Binding, Vec<Binding>),
    UnaryOperator(Binding, UnaryOperator, Binding),
}

#[derive(Clone, Copy, Debug)]
pub struct Binding {
    pub id: usize,
}

#[derive(Debug)]
pub struct Struct<'a> {
    pub name: &'a str,
    pub fields: Vec<Type>,
    pub field_map: HashMap<&'a str, usize>,
}

// TODO: Sort predicates or make comparisons order independent.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Type {
    pub predicates: Vec<usize>,
    pub base: BaseType,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BaseType {
    Array(Box<Type>),
    I64,
    I8,
    Bool,
    PointerI8,
    Struct(usize),
    Void,
    TypeVariable(usize),
    Error,
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

    pub fn substitute_types(&self, substitutions: &[Type]) -> Type {
        use BaseType::*;
        match &self.base {
            Array(inner) => Type {
                predicates: self.predicates.clone(),
                base: Array(Box::new(inner.substitute_types(substitutions))),
            },
            TypeVariable(i) => substitutions[*i].clone(),
            _ => self.clone(),
        }
    }

    pub fn is_fully_substituted(&self) -> bool {
        use BaseType::*;
        match &self.base {
            Array(inner) => inner.is_fully_substituted(),
            I64 | I8 | Bool | PointerI8 | Struct(_) | Void => true,
            TypeVariable(_) | Error => false,
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self.base, BaseType::Error)
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
            BaseType::I8 => 1,
            BaseType::Bool => 1,
            BaseType::PointerI8 => 8,
            // TODO: QBE and LLVM work differently here.
            BaseType::Struct(_) => 8,
            BaseType::Void => 0,
            BaseType::TypeVariable(_) => unreachable!(),
            BaseType::Error => unreachable!(),
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
