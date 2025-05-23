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
    Function {
        name: &'a str,
        args: Vec<&'a str>,
        body: Vec<Statement<'a>>,
    },
    If {
        condition: Expression<'a>,
        true_block: Vec<Statement<'a>>,
        false_block: Vec<Statement<'a>>,
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

impl Expression<'_> {
    pub fn add<'a>(left: Expression<'a>, right: Expression<'a>) -> Expression<'a> {
        Expression::BinaryOperation(BinaryOperator::Add, Box::new((left, right)))
    }
}
