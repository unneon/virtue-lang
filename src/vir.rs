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
    pub name: Cow<'a, str>,
    pub value_args: Vec<Arg>,
    pub all_args: Vec<AnyArg>,
    pub return_type: Type,
    pub bindings: Vec<BindingData>,
    pub binding_map: HashMap<&'a str, Binding>,
    pub type_arg_map: HashMap<&'a str, usize>,
    pub type_arg_substitutions: Option<Vec<Type>>,
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
    Return(Option<Binding>),
    StringConstant(Binding, usize),
    Syscall(Binding, Vec<Binding>),
    UnaryOperator(Binding, UnaryOperator, Binding),
}

#[derive(Clone, Copy)]
pub struct Binding {
    pub id: usize,
}

#[derive(Clone, Debug)]
pub struct Struct<'a> {
    pub name: Cow<'a, str>,
    pub fields: Vec<Type>,
    pub field_map: HashMap<&'a str, usize>,
    pub type_arg_map: HashMap<&'a str, usize>,
    pub is_instantiated: bool,
}

// TODO: Sort predicates or make comparisons order independent.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Type {
    pub predicates: Vec<usize>,
    pub base: BaseType,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BaseType {
    I64,
    I8,
    Bool,
    Pointer(Box<Type>),
    Struct(usize, Vec<Type>),
    Void,
    TypeVariable(usize),
    Error,
}

impl Program<'_> {
    pub fn struct_byte_size(&self, id: usize) -> usize {
        self.structs[id]
            .fields
            .iter()
            .map(|type_| match type_.base {
                BaseType::Struct(id, _) => self.struct_byte_size(id),
                _ => type_.byte_size(),
            })
            .sum()
    }

    pub fn string_len(&self, id: usize) -> usize {
        self.strings[id].iter().map(|s| s.len()).sum()
    }
}

impl Struct<'_> {
    pub fn field_offset(&self, field: usize) -> usize {
        self.fields[..field].iter().map(Type::byte_size).sum()
    }
}

impl Type {
    pub fn pointer(&self) -> Type {
        Type {
            predicates: Vec::new(),
            base: BaseType::Pointer(Box::new(self.clone())),
        }
    }

    pub fn list(&self) -> Type {
        Type {
            predicates: Vec::new(),
            base: BaseType::Struct(1, vec![self.clone()]),
        }
    }

    pub fn substitute_types(&self, substitutions: &[Type]) -> Type {
        use BaseType::*;
        match &self.base {
            I64 | I8 | Bool => self.clone(),
            Pointer(inner) => Type {
                predicates: self.predicates.clone(),
                base: Pointer(Box::new(inner.substitute_types(substitutions))),
            },
            Struct(id, args) => Type {
                predicates: self.predicates.clone(),
                base: Struct(
                    *id,
                    args.iter()
                        .map(|arg| arg.substitute_types(substitutions))
                        .collect(),
                ),
            },
            Void => self.clone(),
            TypeVariable(i) => substitutions[*i].clone(),
            Error => self.clone(),
        }
    }

    pub fn is_fully_substituted(&self) -> bool {
        use BaseType::*;
        match &self.base {
            Pointer(inner) => inner.is_fully_substituted(),
            I64 | I8 | Bool | Void => true,
            Struct(_, args) => args.iter().all(Type::is_fully_substituted),
            TypeVariable(_) => false,
            Error => true,
        }
    }

    pub fn is_fully_instantiated(&self) -> bool {
        use BaseType::*;
        match &self.base {
            Pointer(inner) => inner.is_fully_instantiated(),
            I64 | I8 | Bool | Void => true,
            Struct(_, args) => args.is_empty(),
            TypeVariable(_) => true,
            Error => true,
        }
    }

    pub fn is_void(&self) -> bool {
        matches!(self.base, BaseType::Void)
    }

    pub fn is_error(&self) -> bool {
        matches!(self.base, BaseType::Error)
    }

    pub fn get_struct(&self) -> Option<usize> {
        match &self.base {
            BaseType::Struct(i, _) => Some(*i),
            _ => None,
        }
    }

    pub fn unwrap_struct(&self) -> usize {
        match &self.base {
            BaseType::Struct(i, _) => *i,
            _ => panic!("expected struct, got {self:?}"),
        }
    }

    pub fn dereference(&self) -> Type {
        match &self.base {
            BaseType::Pointer(inner) => inner.as_ref().clone(),
            _ => panic!("expected pointer, got {self:?}"),
        }
    }

    pub fn byte_size(&self) -> usize {
        match &self.base {
            BaseType::I64 => 8,
            BaseType::I8 => 1,
            BaseType::Bool => 1,
            BaseType::Pointer(_) => 8,
            BaseType::Struct(_, _) => unreachable!(),
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

impl std::fmt::Debug for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let id = self.id;
        write!(f, "_{id}")
    }
}
