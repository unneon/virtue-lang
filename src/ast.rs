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
    Print { fmt: &'a str },
}
