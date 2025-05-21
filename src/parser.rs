use crate::ast::{Item, Module, Statement};
use nom::bytes::complete::{tag, take_while};
use nom::character::anychar;
use nom::character::complete::char;
use nom::combinator::{recognize, verify};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, pair, preceded};
use nom::{IResult, Parser};

pub fn module(input: &str) -> Module {
    separated_list0(sp, item)
        .map(|items| Module { items })
        .parse(input)
        .unwrap()
        .1
}

fn item(input: &str) -> IResult<&str, Item> {
    let (input, _) = tag("func")(input)?;
    let (input, name) = preceded(sp, identifier).parse(input)?;
    let (input, _) = preceded(sp, char('(')).parse(input)?;
    let (input, _) = preceded(sp, char(')')).parse(input)?;
    let (input, _) = newline(input)?;
    let (input, body) = many0(statement).parse(input)?;
    Ok((input, Item::Function { name, body }))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("    print")(input)?;
    let (input, fmt) = preceded(
        sp,
        delimited(char('"'), take_while(|c| c != '"'), char('"')),
    )
    .parse(input)?;
    Ok((input, Statement::Print { fmt }))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        verify(anychar, |c| c.is_ascii_alphabetic()),
        take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
    ))
    .parse(input)
}

fn newline(input: &str) -> IResult<&str, ()> {
    preceded(sp, char('\n')).map(|_| ()).parse(input)
}

fn sp(input: &str) -> IResult<&str, ()> {
    take_while(|c| c == ' ').map(|_| ()).parse(input)
}
