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
    Function(Function<'a>),
    If {
        condition: Expression<'a>,
        true_: Vec<Statement<'a>>,
        false_: Vec<Statement<'a>>,
    },
    Print {
        fmt: Format<'a>,
    },
    Return {
        value: Expression<'a>,
    },
    Struct {
        name: &'a str,
        fields: Vec<(&'a str, &'a str)>,
    },
    While {
        condition: Expression<'a>,
        body: Vec<Statement<'a>>,
    },
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub args: Vec<(&'a str, &'a str)>,
    pub return_type: &'a str,
    pub body: Vec<Statement<'a>>,
}

#[derive(Clone, Debug)]
pub enum Expression<'a> {
    BinaryOperation(BinaryOperator, Box<(Expression<'a>, Expression<'a>)>),
    Call(&'a str, Vec<Expression<'a>>),
    Literal(i64),
    New(&'a str),
    Field(Box<Expression<'a>>, &'a str),
    StringLiteral(&'a str),
    Variable(&'a str),
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual,
}

#[derive(Debug)]
pub struct Format<'a> {
    pub segments: Vec<FormatSegment<'a>>,
}

#[derive(Debug)]
pub enum FormatSegment<'a> {
    Text(&'a str),
    Variable(&'a str),
}
