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

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Type {
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
        match type_ {
            Type::I64 => 8,
            Type::I8 => 1,
            Type::Bool => 1,
            Type::Pointer(_) => 8,
            Type::Struct(struct_id, _) => self.structs[*struct_id]
                .fields
                .iter()
                .map(|type_| self.byte_size(type_))
                .sum(),
            Type::Void => 0,
            Type::TypeVariable(_) => unreachable!(),
            Type::Error => unreachable!(),
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
        Type::Pointer(Box::new(self.clone()))
    }

    pub fn list(&self) -> Type {
        Type::Struct(1, vec![self.clone()])
    }

    pub fn substitute_types(&self, substitutions: &[Type]) -> Type {
        use Type::*;
        match self {
            I64 | I8 | Bool => self.clone(),
            Pointer(inner) => Pointer(Box::new(inner.substitute_types(substitutions))),
            Struct(id, args) => Struct(
                *id,
                args.iter()
                    .map(|arg| arg.substitute_types(substitutions))
                    .collect(),
            ),
            Void => self.clone(),
            TypeVariable(i) => substitutions[*i].clone(),
            Error => self.clone(),
        }
    }

    pub fn is_fully_substituted(&self) -> bool {
        use Type::*;
        match self {
            Pointer(inner) => inner.is_fully_substituted(),
            I64 | I8 | Bool | Void => true,
            Struct(_, args) => args.iter().all(Type::is_fully_substituted),
            TypeVariable(_) => false,
            Error => true,
        }
    }

    pub fn is_fully_instantiated(&self) -> bool {
        use Type::*;
        match self {
            Pointer(inner) => inner.is_fully_instantiated(),
            I64 | I8 | Bool | Void => true,
            Struct(_, args) => args.is_empty(),
            TypeVariable(_) => true,
            Error => true,
        }
    }

    pub fn is_i64(&self) -> bool {
        matches!(self, Type::I64)
    }

    pub fn is_i8(&self) -> bool {
        matches!(self, Type::I8)
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct(_, _))
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Type::Error)
    }

    pub fn unwrap_struct(&self) -> usize {
        match self {
            Type::Struct(i, _) => *i,
            _ => panic!("expected struct, got {self:?}"),
        }
    }

    pub fn dereference(&self) -> Type {
        match self {
            Type::Pointer(inner) => inner.as_ref().clone(),
            _ => panic!("expected pointer, got {self:?}"),
        }
    }
}

impl From<Binding> for Value {
    fn from(binding: Binding) -> Value {
        Value::Binding(binding)
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
        match self {
            Type::I64 => write!(f, "i64")?,
            Type::I8 => write!(f, "i8")?,
            Type::Bool => write!(f, "bool")?,
            Type::Pointer(inner) => write!(f, "ptr {inner:?}")?,
            Type::Struct(struct_id, args) => {
                write!(f, "struct{struct_id}")?;
                for arg in args {
                    write!(f, " {arg:?}")?;
                }
            }
            Type::Void => write!(f, "void")?,
            Type::TypeVariable(id) => write!(f, "tv{id}")?,
            Type::Error => write!(f, "error")?,
        }
        Ok(())
    }
}
