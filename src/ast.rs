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
    Assignment { variable: &'a str, value: i64 },
    Print { fmt: Format<'a> },
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
