use crate::ast::{BinaryOperator, Expression, Format, FormatSegment, Module, Statement};
use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::anychar;
use nom::character::complete::{char, digit1};
use nom::combinator::{all_consuming, opt, recognize, verify};
use nom::error::Error;
use nom::multi::{count, fold_many0, many0, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated};

trait Parser<'a, T> = nom::Parser<&'a str, Output = T, Error = Error<&'a str>>;

pub fn module(input: &str) -> Module {
    let statements = all_consuming(terminated(block(0), empty_lines))
        .parse(input)
        .unwrap()
        .1;
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
            struct_statement(nesting),
            assingment_statement(),
        )),
    )
}

fn assingment_statement<'a>() -> impl Parser<'a, Statement<'a>> {
    (
        expression(),
        delimited((sp, char('='), sp), expression(), newline),
    )
        .map(|(left, right)| Statement::Assignment { left, right })
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

fn struct_statement<'a>(nesting: usize) -> impl Parser<'a, Statement<'a>> {
    (
        delimited((tag("struct"), sp), identifier, newline),
        many0(delimited(
            (empty_lines, indentiation(nesting + 1)),
            (identifier, preceded(sp, identifier)),
            newline,
        )),
    )
        .map(|(name, fields)| Statement::Struct { name, fields })
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
    let less_or_equal = tag("<=").map(|_| BinaryOperator::LessOrEqual);
    let less = tag("<").map(|_| BinaryOperator::Less);
    let greater_or_equal = tag(">=").map(|_| BinaryOperator::GreaterOrEqual);
    let greater = tag(">").map(|_| BinaryOperator::Greater);
    let equal = tag("==").map(|_| BinaryOperator::Equal);
    let not_equal = tag("!=").map(|_| BinaryOperator::NotEqual);
    let op = alt((
        less_or_equal,
        less,
        greater_or_equal,
        greater,
        equal,
        not_equal,
    ));
    expression_binary(expression3, op, input)
}

fn expression3(input: &str) -> IResult<&str, Expression> {
    let modulo = char('%').map(|_| BinaryOperator::Modulo);
    expression_binary(expression2, modulo, input)
}

fn expression2(input: &str) -> IResult<&str, Expression> {
    let add = char('+').map(|_| BinaryOperator::Add);
    let subtract = char('-').map(|_| BinaryOperator::Subtract);
    let op = alt((add, subtract));
    expression_binary(expression1, op, input)
}

fn expression1(input: &str) -> IResult<&str, Expression> {
    let multiply = char('*').map(|_| BinaryOperator::Multiply);
    let divide = char('/').map(|_| BinaryOperator::Divide);
    let op = alt((multiply, divide));
    expression_binary(expression0, op, input)
}

fn expression_binary<'a>(
    mut sub_expr: impl Parser<'a, Expression<'a>>,
    op: impl Parser<'a, BinaryOperator>,
    input: &'a str,
) -> IResult<&'a str, Expression<'a>> {
    let (input, expression) = sub_expr.parse(input)?;
    fold_many0(
        (preceded(sp, op), preceded(sp, sub_expr)),
        || expression.clone(),
        |left, (op, right)| Expression::BinaryOperation(op, Box::new((left, right))),
    )
    .parse(input)
}

fn expression0(input: &str) -> IResult<&str, Expression> {
    alt((
        integer_literal,
        parentheses,
        field_expression,
        function_call,
        new_expression,
        string_literal,
        variable_reference,
    ))
    .parse(input)
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

fn parentheses(input: &str) -> IResult<&str, Expression> {
    delimited((sp, char('(')), expression(), (sp, char(')'))).parse(input)
}

fn field_expression(input: &str) -> IResult<&str, Expression> {
    (variable_reference, preceded(char('.'), identifier))
        .map(|(object, field)| Expression::Field(Box::new(object), field))
        .parse(input)
}

fn function_call(input: &str) -> IResult<&str, Expression> {
    (
        preceded(sp, identifier),
        delimited(
            (sp, char('(')),
            separated_list0(preceded(sp, char(',')), preceded(sp, expression())),
            (sp, char(')')),
        ),
    )
        .map(|(func, args)| Expression::Call(func, args))
        .parse(input)
}

fn new_expression(input: &str) -> IResult<&str, Expression> {
    preceded((sp, tag("new"), sp), identifier)
        .map(Expression::New)
        .parse(input)
}

fn string_literal(input: &str) -> IResult<&str, Expression> {
    delimited(preceded(sp, char('"')), take_while(|c| c != '"'), char('"'))
        .map(Expression::StringLiteral)
        .parse(input)
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
