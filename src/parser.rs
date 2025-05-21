use crate::ast::{Format, FormatSegment, Item, Module, Statement};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::anychar;
use nom::character::complete::{char, digit1};
use nom::combinator::{all_consuming, opt, recognize, verify};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, pair, preceded};
use nom::{IResult, Parser};

pub fn module(input: &str) -> Module {
    all_consuming(delimited(
        line_whitespace,
        separated_list0(line_whitespace, item),
        line_whitespace,
    ))
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
    alt((print_statement, assingment_statement)).parse(input)
}

fn assingment_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = indent(input)?;
    let (input, variable) = identifier(input)?;
    let (input, _) = preceded(sp, char('=')).parse(input)?;
    let (input, value) = preceded(sp, integer_literal).parse(input)?;
    let (input, _) = newline(input)?;
    Ok((input, Statement::Assignment { variable, value }))
}

fn print_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = indent(input)?;
    let (input, _) = tag("print")(input)?;
    let (input, fmt) = preceded(sp, format_string).parse(input)?;
    let (input, _) = newline(input)?;
    Ok((input, Statement::Print { fmt }))
}

fn format_string(input: &str) -> IResult<&str, Format> {
    delimited(char('"'), many0(format_segment), char('"'))
        .map(|segments| Format { segments })
        .parse(input)
}

fn format_segment(input: &str) -> IResult<&str, FormatSegment> {
    alt((format_segment_text, format_segment_variable)).parse(input)
}

fn format_segment_text(input: &str) -> IResult<&str, FormatSegment> {
    take_while1(|c| c != '"' && c != '{')
        .map(FormatSegment::Text)
        .parse(input)
}

fn format_segment_variable(input: &str) -> IResult<&str, FormatSegment> {
    delimited(char('{'), identifier, char('}'))
        .map(FormatSegment::Variable)
        .parse(input)
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        verify(anychar, |c| c.is_ascii_alphabetic()),
        take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
    ))
    .parse(input)
}

fn integer_literal(input: &str) -> IResult<&str, i64> {
    let (input, literal) = recognize(pair(opt(char('-')), digit1)).parse(input)?;
    Ok((input, literal.parse().unwrap()))
}

fn indent(input: &str) -> IResult<&str, ()> {
    tag("    ").map(|_| ()).parse(input)
}

fn newline(input: &str) -> IResult<&str, ()> {
    preceded(sp, char('\n')).map(|_| ()).parse(input)
}

fn line_whitespace(input: &str) -> IResult<&str, ()> {
    take_while(|c| c == ' ' || c == '\n')
        .map(|_| ())
        .parse(input)
}

fn sp(input: &str) -> IResult<&str, ()> {
    take_while(|c| c == ' ').map(|_| ()).parse(input)
}
