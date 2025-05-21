#[derive(Debug)]
pub struct Module<'a> {
    pub items: Vec<Item<'a>>,
}

#[derive(Debug)]
pub enum Item<'a> {
    Function {
        name: &'a str,
        body: Vec<Statement<'a>>,
    },
}

#[derive(Debug)]
pub enum Statement<'a> {
    Assignment {
        variable: &'a str,
        expression: Expression<'a>,
    },
    Print {
        fmt: Format<'a>,
    },
    While {
        condition: Expression<'a>,
        body: Vec<Statement<'a>>,
    },
}

#[derive(Debug)]
pub enum Expression<'a> {
    BinaryOperation(BinaryOperator, Box<(Expression<'a>, Expression<'a>)>),
    Literal(i64),
    Variable(&'a str),
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessOrEqual,
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
