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
    pub is_used: bool,
    pub name: Cow<'a, str>,
    pub value_args: Vec<Arg>,
    pub all_args: Vec<AnyArg>,
    pub return_type: Type,
    pub bindings: Vec<Type>,
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
    Value,
    Type,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Alloc(Binding, Value),
    Assignment(Binding, Value),
    AssignmentField(Binding, usize, Value),
    AssignmentIndex(Binding, Value, Value),
    BinaryOperator(Binding, BinaryOperator, Value, Value),
    Call {
        return_: Option<Binding>,
        function: usize,
        args: Vec<Value>,
    },
    Field(Binding, Binding, usize),
    Index(Binding, Binding, Value),
    JumpAlways(usize),
    JumpConditional {
        condition: Value,
        true_block: usize,
        false_block: usize,
    },
    Return(Option<Value>),
    Syscall(Binding, Vec<Value>),
    UnaryOperator(Binding, UnaryOperator, Value),
}

#[derive(Clone, Copy)]
pub struct Binding {
    pub id: usize,
}

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Binding(Binding),
    ConstBool(bool),
    ConstI64(i64),
    Error,
    String(usize),
}

#[derive(Clone, Debug)]
pub struct Struct<'a> {
    pub name: Cow<'a, str>,
    pub fields: Vec<Type>,
    pub field_map: HashMap<&'a str, usize>,
    pub type_arg_map: HashMap<&'a str, usize>,
    pub is_instantiated: bool,
    pub is_used: bool,
    pub instantiation_info: Option<InstantiationInfo>,
}

// TODO: Sort predicates or make comparisons order independent.
#[derive(Clone, Eq, Hash, PartialEq)]
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

#[derive(Clone, Debug)]
pub struct InstantiationInfo {
    pub generic_struct_id: usize,
    pub type_args: Vec<Type>,
}

impl Program<'_> {
    pub fn byte_size(&self, type_: &Type) -> usize {
        match &type_.base {
            BaseType::I64 => 8,
            BaseType::I8 => 1,
            BaseType::Bool => 1,
            BaseType::Pointer(_) => 8,
            BaseType::Struct(struct_id, _) => self.structs[*struct_id]
                .fields
                .iter()
                .map(|type_| self.byte_size(type_))
                .sum(),
            BaseType::Void => 0,
            BaseType::TypeVariable(_) => unreachable!(),
            BaseType::Error => unreachable!(),
        }
    }

    pub fn field_offset(&self, struct_id: usize, field: usize) -> usize {
        self.structs[struct_id].fields[..field]
            .iter()
            .map(|type_| self.byte_size(type_))
            .sum()
    }
}

impl Function<'_> {
    pub fn should_codegen(&self) -> bool {
        self.type_arg_substitutions.is_some() && self.is_used
    }
}

impl Value {
    pub fn unwrap_binding(&self) -> Binding {
        match self {
            Value::Binding(binding) => *binding,
            _ => unreachable!(),
        }
    }
}

impl Struct<'_> {
    pub fn should_codegen(&self) -> bool {
        self.is_instantiated && self.is_used
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

    pub fn is_i64(&self) -> bool {
        matches!(self.base, BaseType::I64)
    }

    pub fn is_i8(&self) -> bool {
        matches!(self.base, BaseType::I8)
    }

    pub fn is_struct(&self) -> bool {
        matches!(self.base, BaseType::Struct(_, _))
    }

    pub fn is_void(&self) -> bool {
        matches!(self.base, BaseType::Void)
    }

    pub fn is_error(&self) -> bool {
        matches!(self.base, BaseType::Error)
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
}

impl From<Binding> for Value {
    fn from(binding: Binding) -> Value {
        Value::Binding(binding)
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

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        for predicate in &self.predicates {
            write!(f, "f{predicate} ")?;
        }
        match &self.base {
            BaseType::I64 => write!(f, "i64")?,
            BaseType::I8 => write!(f, "i8")?,
            BaseType::Bool => write!(f, "bool")?,
            BaseType::Pointer(inner) => write!(f, "ptr {inner:?}")?,
            BaseType::Struct(struct_id, args) => {
                write!(f, "struct{struct_id}")?;
                for arg in args {
                    write!(f, " {arg:?}")?;
                }
            }
            BaseType::Void => write!(f, "void")?,
            BaseType::TypeVariable(id) => write!(f, "tv{id}")?,
            BaseType::Error => write!(f, "error")?,
        }
        write!(f, "]")?;
        Ok(())
    }
}
