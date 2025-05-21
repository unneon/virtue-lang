use crate::ast::{BinaryOperator, Expression, Format, FormatSegment, Item, Module, Statement};
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
    let (input, expression) = preceded(sp, expression).parse(input)?;
    let (input, _) = newline(input)?;
    Ok((
        input,
        Statement::Assignment {
            variable,
            expression,
        },
    ))
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

fn expression(input: &str) -> IResult<&str, Expression> {
    expression2(input)
}

fn expression2(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut expression) = expression1(input)?;
    while let Ok((subinput, (op, right))) =
        pair(preceded(sp, binary_op2), preceded(sp, expression1)).parse(input)
    {
        input = subinput;
        expression = Expression::BinaryOperation(op, Box::new((expression, right)));
    }
    Ok((input, expression))
}

fn expression1(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut expression) = expression0(input)?;
    while let Ok((subinput, (op, right))) =
        pair(preceded(sp, binary_op1), preceded(sp, expression0)).parse(input)
    {
        input = subinput;
        expression = Expression::BinaryOperation(op, Box::new((expression, right)));
    }
    Ok((input, expression))
}

fn expression0(input: &str) -> IResult<&str, Expression> {
    alt((integer_literal, variable_reference)).parse(input)
}

fn binary_op2(input: &str) -> IResult<&str, BinaryOperator> {
    alt((add_op, subtraction_op)).parse(input)
}

fn binary_op1(input: &str) -> IResult<&str, BinaryOperator> {
    alt((multiply_op, divide_op)).parse(input)
}

fn add_op(input: &str) -> IResult<&str, BinaryOperator> {
    char('+').map(|_| BinaryOperator::Add).parse(input)
}

fn subtraction_op(input: &str) -> IResult<&str, BinaryOperator> {
    char('-').map(|_| BinaryOperator::Subtract).parse(input)
}

fn multiply_op(input: &str) -> IResult<&str, BinaryOperator> {
    char('*').map(|_| BinaryOperator::Multiply).parse(input)
}

fn divide_op(input: &str) -> IResult<&str, BinaryOperator> {
    char('/').map(|_| BinaryOperator::Divide).parse(input)
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        verify(anychar, |c| c.is_ascii_alphabetic()),
        take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
    ))
    .parse(input)
}

fn integer_literal(input: &str) -> IResult<&str, Expression> {
    let (input, literal) = recognize(pair(opt(char('-')), digit1)).parse(input)?;
    let literal = literal.parse().unwrap();
    let expression = Expression::Literal(literal);
    Ok((input, expression))
}

fn variable_reference(input: &str) -> IResult<&str, Expression> {
    identifier.map(Expression::Variable).parse(input)
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
