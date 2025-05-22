use crate::ast::{BinaryOperator, Expression, Format, FormatSegment, Module, Statement};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::anychar;
use nom::character::complete::{char, digit1};
use nom::combinator::{all_consuming, opt, recognize, verify};
use nom::multi::{count, many0, separated_list0};
use nom::sequence::{delimited, pair, preceded};
use nom::{IResult, Parser};

pub fn module(input: &str) -> Module {
    all_consuming(delimited(
        line_whitespace,
        separated_list0(line_whitespace, |input| statement(0, input)),
        line_whitespace,
    ))
    .map(|statements| Module { statements })
    .parse(input)
    .unwrap()
    .1
}

fn block(nesting: usize, mut input: &str) -> IResult<&str, Vec<Statement>> {
    let mut statements = Vec::new();
    while let Ok((subinput, statement)) = statement(nesting, input) {
        input = subinput;
        statements.push(statement);
    }
    Ok((input, statements))
}

fn statement(nesting: usize, input: &str) -> IResult<&str, Statement> {
    if let Ok(r) = if_statement(nesting, input) {
        Ok(r)
    } else if let Ok(r) = print_statement(nesting, input) {
        Ok(r)
    } else if let Ok(r) = while_statement(nesting, input) {
        Ok(r)
    } else if let Ok(r) = func_statement(nesting, input) {
        Ok(r)
    } else if let Ok(r) = return_statement(nesting, input) {
        Ok(r)
    } else {
        assingment_statement(nesting, input)
    }
}

fn assingment_statement(nesting: usize, input: &str) -> IResult<&str, Statement> {
    let (input, _) = indentiation(nesting, input)?;
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

fn if_statement(nesting: usize, input: &str) -> IResult<&str, Statement> {
    let (input, _) = indentiation(nesting, input)?;
    let (input, _) = tag("if")(input)?;
    let (input, condition) = preceded(sp, expression).parse(input)?;
    let (input, _) = newline(input)?;
    let (input, true_block) = block(nesting + 1, input)?;
    let (input, false_block) = else_block(nesting, input).or_else(|_| Ok((input, vec![])))?;
    Ok((
        input,
        Statement::If {
            condition,
            true_block,
            false_block,
        },
    ))
}

fn else_block(nesting: usize, input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, _) = indentiation(nesting, input)?;
    let (input, _) = tag("else")(input)?;
    let (input, _) = newline(input)?;
    let (input, false_block) = block(nesting + 1, input)?;
    Ok((input, false_block))
}

fn print_statement(nesting: usize, input: &str) -> IResult<&str, Statement> {
    let (input, _) = indentiation(nesting, input)?;
    let (input, _) = tag("print")(input)?;
    let (input, fmt) = preceded(sp, format_string).parse(input)?;
    let (input, _) = newline(input)?;
    Ok((input, Statement::Print { fmt }))
}

fn while_statement(nesting: usize, input: &str) -> IResult<&str, Statement> {
    let (input, _) = indentiation(nesting, input)?;
    let (input, _) = tag("while")(input)?;
    let (input, condition) = preceded(sp, expression).parse(input)?;
    let (input, _) = newline(input)?;
    let (input, body) = block(nesting + 1, input)?;
    Ok((input, Statement::While { condition, body }))
}

fn func_statement(nesting: usize, input: &str) -> IResult<&str, Statement> {
    let (input, _) = indentiation(nesting, input)?;
    let (input, _) = tag("func")(input)?;
    let (input, name) = preceded(sp, identifier).parse(input)?;
    let (input, _) = preceded(sp, char('(')).parse(input)?;
    let (input, args) =
        separated_list0(preceded(sp, char(',')), preceded(sp, identifier)).parse(input)?;
    let (input, _) = preceded(sp, char(')')).parse(input)?;
    let (input, _) = newline(input)?;
    let (input, body) = block(nesting + 1, input)?;
    Ok((input, Statement::Function { name, args, body }))
}

fn return_statement(nesting: usize, input: &str) -> IResult<&str, Statement> {
    let (input, _) = indentiation(nesting, input)?;
    let (input, _) = tag("return")(input)?;
    let (input, value) = preceded(sp, expression).parse(input)?;
    let (input, _) = newline(input)?;
    Ok((input, Statement::Return { value }))
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
    expression4(input)
}

fn expression4(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut expression) = expression3(input)?;
    while let Ok((subinput, (op, right))) =
        pair(preceded(sp, binary_op4), preceded(sp, expression3)).parse(input)
    {
        input = subinput;
        expression = Expression::BinaryOperation(op, Box::new((expression, right)));
    }
    Ok((input, expression))
}

fn expression3(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut expression) = expression2(input)?;
    while let Ok((subinput, (op, right))) =
        pair(preceded(sp, binary_op3), preceded(sp, expression2)).parse(input)
    {
        input = subinput;
        expression = Expression::BinaryOperation(op, Box::new((expression, right)));
    }
    Ok((input, expression))
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
    alt((integer_literal, function_call, variable_reference)).parse(input)
}

fn binary_op4(input: &str) -> IResult<&str, BinaryOperator> {
    alt((
        less_or_equal_op,
        less,
        greater_or_equal_op,
        greater,
        equal,
        not_equal,
    ))
    .parse(input)
}

fn binary_op3(input: &str) -> IResult<&str, BinaryOperator> {
    modulo_op(input)
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

fn modulo_op(input: &str) -> IResult<&str, BinaryOperator> {
    char('%').map(|_| BinaryOperator::Modulo).parse(input)
}

fn less(input: &str) -> IResult<&str, BinaryOperator> {
    tag("<").map(|_| BinaryOperator::Less).parse(input)
}

fn less_or_equal_op(input: &str) -> IResult<&str, BinaryOperator> {
    tag("<=").map(|_| BinaryOperator::LessOrEqual).parse(input)
}

fn greater(input: &str) -> IResult<&str, BinaryOperator> {
    tag(">").map(|_| BinaryOperator::Greater).parse(input)
}

fn greater_or_equal_op(input: &str) -> IResult<&str, BinaryOperator> {
    tag(">=")
        .map(|_| BinaryOperator::GreaterOrEqual)
        .parse(input)
}

fn equal(input: &str) -> IResult<&str, BinaryOperator> {
    tag("==").map(|_| BinaryOperator::Equal).parse(input)
}

fn not_equal(input: &str) -> IResult<&str, BinaryOperator> {
    tag("!=").map(|_| BinaryOperator::NotEqual).parse(input)
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

fn function_call(input: &str) -> IResult<&str, Expression> {
    let (input, func) = identifier(input)?;
    let (input, _) = preceded(sp, char('(')).parse(input)?;
    let (input, args) =
        separated_list0(preceded(sp, char(',')), preceded(sp, expression)).parse(input)?;
    let (input, _) = preceded(sp, char(')')).parse(input)?;
    Ok((input, Expression::Call(func, args)))
}

fn variable_reference(input: &str) -> IResult<&str, Expression> {
    identifier.map(Expression::Variable).parse(input)
}

fn indentiation(nesting: usize, input: &str) -> IResult<&str, ()> {
    count(indent, nesting).map(|_| ()).parse(input)
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
