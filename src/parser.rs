use crate::ast::{
    BinaryOperator, Expression, Format, FormatSegment, Function, Module, Statement, Type,
    UnaryOperator,
};
use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::anychar;
use nom::character::complete::{char, digit1};
use nom::combinator::{all_consuming, opt, recognize, verify};
use nom::error::Error;
use nom::multi::{count, fold_many0, many0, many1, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated};

trait Parser<'a, T> = nom::Parser<&'a str, Output = T, Error = Error<&'a str>>;

pub fn parse(input: &str) -> Result<Module, String> {
    Ok(module(input).map_err(|e| e.to_string())?.1)
}

pub fn module(input: &str) -> IResult<&str, Module> {
    all_consuming(terminated(block(0), empty_lines))
        .map(|statements| Module { statements })
        .parse(input)
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
            for_range_statement(nesting),
            func_statement(nesting),
            return_statement(),
            struct_statement(nesting),
            assingment_statement(),
            expression_statement(),
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

fn expression_statement<'a>() -> impl Parser<'a, Statement<'a>> {
    terminated(expression(), newline).map(Statement::Expression)
}

fn if_statement<'a>(nesting: usize) -> impl Parser<'a, Statement<'a>> {
    (
        preceded((tag("if"), sp), expression()),
        preceded(newline, block(nesting + 1)),
        opt(else_block(nesting)),
    )
        .map(|(condition, true_, false_block)| {
            let false_ = false_block.unwrap_or_else(Vec::new);
            Statement::If {
                condition,
                true_,
                false_,
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

fn for_range_statement<'a>(nesting: usize) -> impl Parser<'a, Statement<'a>> {
    (
        preceded((tag("for"), sp), identifier),
        preceded((sp, tag("from"), sp), expression()),
        preceded((sp, tag("to"), sp), expression()),
        opt(preceded((sp, tag("by"), sp), expression())),
        preceded(newline, block(nesting + 1)),
    )
        .map(|(index, lower, upper, step, body)| Statement::ForRange {
            index,
            lower,
            upper,
            step,
            body,
        })
}

fn func_statement<'a>(nesting: usize) -> impl Parser<'a, Statement<'a>> {
    (
        preceded((tag("func"), sp), identifier),
        preceded(
            (sp, char('(')),
            separated_list0(
                preceded(sp, char(',')),
                (preceded(sp, identifier), preceded(sp, type_)),
            ),
        ),
        preceded((sp, char(')')), type_),
        preceded(newline, block(nesting + 1)),
    )
        .map(|(name, args, return_type, body)| {
            Statement::Function(Function {
                name,
                args,
                return_type,
                body,
            })
        })
}

fn return_statement<'a>() -> impl Parser<'a, Statement<'a>> {
    delimited((tag("return"), sp), expression(), newline).map(|value| Statement::Return { value })
}

fn struct_statement<'a>(nesting: usize) -> impl Parser<'a, Statement<'a>> {
    (
        delimited((tag("struct"), sp), identifier, newline),
        many0(delimited(
            (empty_lines, indentiation(nesting + 1)),
            (identifier, type_),
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
    let normal = verify(
        take_while(|c| c != '"' && c != '\\' && c != '{'),
        |s: &str| !s.is_empty(),
    );
    let segment = alt((string_escape, normal));
    many1(segment).map(FormatSegment::Text)
}

fn format_segment_variable<'a>() -> impl Parser<'a, FormatSegment<'a>> {
    delimited(char('{'), identifier, char('}')).map(FormatSegment::Variable)
}

fn expression<'a>() -> impl Parser<'a, Expression<'a>> {
    |input| expression5(input)
}

fn expression5(input: &str) -> IResult<&str, Expression> {
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
    expression_binary_single(expression4, op, input)
}

fn expression4(input: &str) -> IResult<&str, Expression> {
    let and = char('&').map(|_| BinaryOperator::BitAnd);
    let or = char('|').map(|_| BinaryOperator::BitOr);
    let xor = char('^').map(|_| BinaryOperator::BitXor);
    let shift_left = tag("<<").map(|_| BinaryOperator::BitShiftLeft);
    let shift_right = tag(">>").map(|_| BinaryOperator::BitShiftRight);
    let op = alt((and, or, xor, shift_left, shift_right));
    expression_binary_single(expression3, op, input)
}

fn expression3(input: &str) -> IResult<&str, Expression> {
    let add = char('+').map(|_| BinaryOperator::Add);
    let subtract = char('-').map(|_| BinaryOperator::Subtract);
    let op = alt((add, subtract));
    expression_binary(expression2, op, input)
}

fn expression2(input: &str) -> IResult<&str, Expression> {
    let multiply = char('*').map(|_| BinaryOperator::Multiply);
    let divide = char('/').map(|_| BinaryOperator::Divide);
    let modulo = char('%').map(|_| BinaryOperator::Modulo);
    let op = alt((multiply, divide, modulo));
    expression_binary(expression1, op, input)
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

fn expression_binary_single<'a>(
    mut sub_expr: impl Parser<'a, Expression<'a>>,
    op: impl Parser<'a, BinaryOperator>,
    input: &'a str,
) -> IResult<&'a str, Expression<'a>> {
    let (input, left) = sub_expr.parse(input)?;
    if let Ok((input, (op, right))) = (preceded(sp, op), preceded(sp, sub_expr)).parse(input) {
        Ok((
            input,
            Expression::BinaryOperation(op, Box::new((left, right))),
        ))
    } else {
        Ok((input, left))
    }
}

fn expression1(input: &str) -> IResult<&str, Expression> {
    let negate = char('-').map(|_| UnaryOperator::Negate);
    let bitnot = char('~').map(|_| UnaryOperator::BitNot);
    let not = (tag("not"), sp).map(|_| UnaryOperator::Not);
    let op = alt((negate, bitnot, not));
    (opt(op), expression0)
        .map(|(op, expr)| match op {
            Some(op) => Expression::UnaryOperation(op, Box::new(expr)),
            None => expr,
        })
        .parse(input)
}

fn expression0(input: &str) -> IResult<&str, Expression> {
    alt((
        integer_literal,
        parentheses,
        array_literal,
        array_repeat,
        method_call,
        field_expression,
        function_call,
        index_expression,
        new_expression,
        string_literal,
        bool_literal,
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
    let (input, literal) = preceded(sp, digit1).parse(input)?;
    let literal = literal.parse().unwrap();
    let expression = Expression::Literal(literal);
    Ok((input, expression))
}

fn parentheses(input: &str) -> IResult<&str, Expression> {
    delimited((sp, char('(')), expression(), (sp, char(')'))).parse(input)
}

fn array_literal(input: &str) -> IResult<&str, Expression> {
    delimited(
        (sp, char('[')),
        separated_list0((sp, char(',')), expression()),
        (sp, char(']')),
    )
    .map(Expression::ArrayLiteral)
    .parse(input)
}

fn array_repeat(input: &str) -> IResult<&str, Expression> {
    delimited(
        (sp, char('[')),
        (expression(), preceded((sp, char(';')), expression())),
        (sp, char(']')),
    )
    .map(|(initializer, length)| Expression::ArrayRepeat(Box::new((initializer, length))))
    .parse(input)
}

fn method_call(input: &str) -> IResult<&str, Expression> {
    (
        variable_reference,
        preceded(char('.'), identifier),
        delimited(
            (sp, char('(')),
            separated_list0(preceded(sp, char(',')), expression()),
            (sp, char(')')),
        ),
    )
        .map(|(object, method, args)| Expression::CallMethod(Box::new(object), method, args))
        .parse(input)
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

fn index_expression(input: &str) -> IResult<&str, Expression> {
    (
        variable_reference,
        delimited(
            preceded(sp, char('[')),
            expression(),
            preceded(sp, char(']')),
        ),
    )
        .map(|(list, index)| Expression::Index(Box::new((list, index))))
        .parse(input)
}
fn new_expression(input: &str) -> IResult<&str, Expression> {
    preceded((sp, tag("new"), sp), type_)
        .map(Expression::New)
        .parse(input)
}

fn string_literal(input: &str) -> IResult<&str, Expression> {
    let normal = verify(take_while(|c| c != '"' && c != '\\'), |s: &str| {
        !s.is_empty()
    });
    let segment = alt((string_escape, normal));
    delimited((sp, char('"')), many0(segment), char('"'))
        .map(Expression::StringLiteral)
        .parse(input)
}

fn string_escape(input: &str) -> IResult<&str, &str> {
    tag("\\n").map(|_| "\n").parse(input)
}

fn bool_literal(input: &str) -> IResult<&str, Expression> {
    let true_ = tag("true").map(|_| Expression::Literal(1));
    let false_ = tag("false").map(|_| Expression::Literal(0));
    preceded(sp, alt((true_, false_))).parse(input)
}

fn variable_reference(input: &str) -> IResult<&str, Expression> {
    preceded(sp, identifier)
        .map(Expression::Variable)
        .parse(input)
}

fn type_(input: &str) -> IResult<&str, Type> {
    many0(preceded(sp, identifier))
        .map(|segments| Type { segments })
        .parse(input)
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
