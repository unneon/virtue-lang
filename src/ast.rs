#[derive(Debug)]
pub struct Module<'a> {
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Assignment {
        left: Expression<'a>,
        right: Expression<'a>,
    },
    AssignmentBinary {
        op: BinaryOperator,
        left: Expression<'a>,
        right: Expression<'a>,
    },
    Expression(Expression<'a>),
    ForRange {
        index: &'a str,
        lower: Expression<'a>,
        upper: Expression<'a>,
        step: Option<Expression<'a>>,
        body: Vec<Statement<'a>>,
    },
    Function(Function<'a>),
    If {
        condition: Expression<'a>,
        true_: Vec<Statement<'a>>,
        false_: Vec<Statement<'a>>,
    },
    IncrementDecrement {
        value: Expression<'a>,
        op: IncrementDecrementOperator,
    },
    Print {
        fmt: Format<'a>,
    },
    Return {
        value: Expression<'a>,
    },
    Struct {
        name: &'a str,
        fields: Vec<(&'a str, Type<'a>)>,
    },
    While {
        condition: Expression<'a>,
        body: Vec<Statement<'a>>,
    },
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub args: Vec<(&'a str, Type<'a>)>,
    pub return_type: Type<'a>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Clone, Debug)]
pub struct Type<'a> {
    pub segments: Vec<&'a str>,
}

#[derive(Clone, Debug)]
pub enum Expression<'a> {
    ArrayLiteral(Vec<Expression<'a>>),
    ArrayRepeat(Box<(Expression<'a>, Expression<'a>)>),
    BinaryOperation(BinaryOperator, Box<(Expression<'a>, Expression<'a>)>),
    Call(&'a str, Vec<Expression<'a>>),
    CallMethod(Box<Expression<'a>>, &'a str, Vec<Expression<'a>>),
    Index(Box<(Expression<'a>, Expression<'a>)>),
    Literal(i64),
    New(Type<'a>),
    Field(Box<Expression<'a>>, &'a str),
    StringLiteral(Vec<&'a str>),
    UnaryOperation(UnaryOperator, Box<Expression<'a>>),
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
    BitXor,
    BitShiftLeft,
    BitShiftRight,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOperator {
    Negate,
    BitNot,
    Not,
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
