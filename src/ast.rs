#[derive(Debug)]
pub struct Module<'a> {
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Assignment {
        variable: &'a str,
        expression: Expression<'a>,
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
