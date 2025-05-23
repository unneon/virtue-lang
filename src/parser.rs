use crate::ast::{BinaryOperator, Expression, Format, FormatSegment, Module, Statement};
use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::anychar;
use nom::character::complete::{char, digit1};
use nom::combinator::{all_consuming, opt, recognize, verify};
use nom::error::Error;
use nom::multi::{count, many0, separated_list0};
use nom::sequence::{delimited, pair, preceded};

trait Parser<'a, T> = nom::Parser<&'a str, Output = T, Error = Error<&'a str>>;

pub fn module(input: &str) -> Module {
    let statements = all_consuming(block(0)).parse(input).unwrap().1;
    Module { statements }
}

fn block<'a>(nesting: usize) -> impl Parser<'a, Vec<Statement<'a>>> {
    move |input| block_(nesting, input)
}

// Helper function seems to be necessary to break cyclic dependency of impl trait.
fn block_(nesting: usize, input: &str) -> IResult<&str, Vec<Statement>> {
    many0(preceded(empty_lines, statement(nesting))).parse(input)
}

fn statement<'a>(nesting: usize) -> impl Parser<'a, Statement<'a>> {
    preceded(
        indentiation(nesting),
        alt((
            if_statement(nesting),
            print_statement(),
            while_statement(nesting),
            func_statement(nesting),
            return_statement(),
            assingment_statement(),
        )),
    )
}

fn assingment_statement<'a>() -> impl Parser<'a, Statement<'a>> {
    (
        identifier,
        delimited((sp, char('='), sp), expression(), newline),
    )
        .map(|(variable, expression)| Statement::Assignment {
            variable,
            expression,
        })
}

fn if_statement<'a>(nesting: usize) -> impl Parser<'a, Statement<'a>> {
    (
        preceded((tag("if"), sp), expression()),
        preceded(newline, block(nesting + 1)),
        opt(else_block(nesting)),
    )
        .map(|(condition, true_block, false_block)| {
            let false_block = false_block.unwrap_or_else(Vec::new);
            Statement::If {
                condition,
                true_block,
                false_block,
            }
        })
}

fn else_block<'a>(nesting: usize) -> impl Parser<'a, Vec<Statement<'a>>> {
    preceded(
        (indentiation(nesting), tag("else"), newline),
        block(nesting + 1),
    )
}

fn print_statement<'a>() -> impl Parser<'a, Statement<'a>> {
    delimited((tag("print"), sp), format_string(), newline).map(|fmt| Statement::Print { fmt })
}

fn while_statement<'a>(nesting: usize) -> impl Parser<'a, Statement<'a>> {
    (
        preceded((tag("while"), sp), expression()),
        preceded(newline, block(nesting + 1)),
    )
        .map(|(condition, body)| Statement::While { condition, body })
}

fn func_statement<'a>(nesting: usize) -> impl Parser<'a, Statement<'a>> {
    (
        preceded((tag("func"), sp), identifier),
        preceded(
            (sp, char('(')),
            separated_list0(preceded(sp, char(',')), preceded(sp, identifier)),
        ),
        preceded((sp, char(')'), newline), block(nesting + 1)),
    )
        .map(|(name, args, body)| Statement::Function { name, args, body })
}

fn return_statement<'a>() -> impl Parser<'a, Statement<'a>> {
    delimited((tag("return"), sp), expression(), newline).map(|value| Statement::Return { value })
}

fn format_string<'a>() -> impl Parser<'a, Format<'a>> {
    delimited(char('"'), many0(format_segment()), char('"')).map(|segments| Format { segments })
}

fn format_segment<'a>() -> impl Parser<'a, FormatSegment<'a>> {
    alt((format_segment_text(), format_segment_variable()))
}

fn format_segment_text<'a>() -> impl Parser<'a, FormatSegment<'a>> {
    take_while1(|c| c != '"' && c != '{').map(FormatSegment::Text)
}

fn format_segment_variable<'a>() -> impl Parser<'a, FormatSegment<'a>> {
    delimited(char('{'), identifier, char('}')).map(FormatSegment::Variable)
}

fn expression<'a>() -> impl Parser<'a, Expression<'a>> {
    |input| expression4(input)
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
        separated_list0(preceded(sp, char(',')), preceded(sp, expression())).parse(input)?;
    let (input, _) = preceded(sp, char(')')).parse(input)?;
    Ok((input, Expression::Call(func, args)))
}

fn variable_reference(input: &str) -> IResult<&str, Expression> {
    identifier.map(Expression::Variable).parse(input)
}

fn indentiation<'a>(nesting: usize) -> impl Parser<'a, ()> {
    count(indent, nesting).map(|_| ())
}

fn indent(input: &str) -> IResult<&str, ()> {
    tag("    ").map(|_| ()).parse(input)
}

fn empty_lines(input: &str) -> IResult<&str, ()> {
    many0(preceded(sp, alt((newline, comment))))
        .map(|_| ())
        .parse(input)
}

fn comment(input: &str) -> IResult<&str, ()> {
    (tag("#"), take_while(|c| c != '\n'), char('\n'))
        .map(|_| ())
        .parse(input)
}

fn newline(input: &str) -> IResult<&str, ()> {
    preceded(sp, char('\n')).map(|_| ()).parse(input)
}

fn sp(input: &str) -> IResult<&str, ()> {
    take_while(|c| c == ' ').map(|_| ()).parse(input)
}
