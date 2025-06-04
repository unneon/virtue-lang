use crate::error::Spanned;

#[derive(Debug)]
pub struct Module<'a> {
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Assignment {
        left: Spanned<Expression<'a>>,
        type_: Option<Type<'a>>,
        right: Spanned<Expression<'a>>,
    },
    AssignmentBinary {
        op: BinaryOperator,
        left: Spanned<Expression<'a>>,
        right: Spanned<Expression<'a>>,
    },
    Expression(Spanned<Expression<'a>>),
    ForRange {
        index: &'a str,
        lower: Spanned<Expression<'a>>,
        upper: Spanned<Expression<'a>>,
        step: Option<Spanned<Expression<'a>>>,
        body: Vec<Statement<'a>>,
    },
    Function(Function<'a>),
    If {
        condition: Spanned<Expression<'a>>,
        true_: Vec<Statement<'a>>,
        false_: Vec<Statement<'a>>,
    },
    IncrementDecrement {
        value: Spanned<Expression<'a>>,
        op: IncrementDecrementOperator,
    },
    Print {
        fmt: Format<'a>,
    },
    Return {
        value: Spanned<Expression<'a>>,
    },
    Struct {
        name: &'a str,
        fields: Vec<(&'a str, Type<'a>)>,
    },
    While {
        condition: Spanned<Expression<'a>>,
        body: Vec<Statement<'a>>,
    },
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub args: Vec<(&'a str, Type<'a>)>,
    pub return_type: Option<Type<'a>>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Clone, Debug)]
pub struct Type<'a> {
    pub segments: Vec<&'a str>,
}

#[derive(Clone, Debug)]
pub enum Expression<'a> {
    ArrayLiteral(Vec<Spanned<Expression<'a>>>),
    ArrayRepeat(Box<(Spanned<Expression<'a>>, Spanned<Expression<'a>>)>),
    BinaryOperation(
        BinaryOperator,
        Box<(Spanned<Expression<'a>>, Spanned<Expression<'a>>)>,
    ),
    BoolLiteral(bool),
    Call(&'a str, Vec<Spanned<Expression<'a>>>),
    CallMethod(Box<Expression<'a>>, &'a str, Vec<Spanned<Expression<'a>>>),
    Index(Box<(Expression<'a>, Spanned<Expression<'a>>)>),
    Literal(i64),
    New(Type<'a>),
    Field(Box<Expression<'a>>, &'a str),
    StringLiteral(Vec<&'a str>),
    UnaryOperation(UnaryOperator, Box<Spanned<Expression<'a>>>),
    Variable(&'a str),
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitAnd,
    BitOr,
    Xor,
    ShiftLeft,
    ShiftRight,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual,
    LogicAnd,
    LogicOr,
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOperator {
    Negate,
    BitNot,
    LogicNot,
}

#[derive(Debug, Copy, Clone)]
pub enum IncrementDecrementOperator {
    Increment,
    Decrement,
}

#[derive(Debug)]
pub struct Format<'a> {
    pub segments: Vec<FormatSegment<'a>>,
}

#[derive(Debug)]
pub enum FormatSegment<'a> {
    Text(Vec<&'a str>),
    Variable(&'a str),
}
