use crate::ast::{
    BinaryOperator, Expression, Format, FormatSegment, Function, IncrementDecrementOperator,
    Module, Statement, Struct, Type, UnaryOperator,
};
use crate::error::{Span, SpanExt, Spanned};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::anychar;
use nom::character::complete::{char, digit1};
use nom::combinator::{all_consuming, cut, opt, recognize, verify};
use nom::multi::{count, fold_many0, many0, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, terminated};

trait Parser<'a, T> = nom::Parser<&'a str, Output = T, Error = ParseError<'a>>;

#[derive(Debug)]
enum ParseElement {
    Identifier,
}

#[derive(Debug)]
enum ParseError<'a> {
    Expected {
        element: ParseElement,
        position: usize,
    },
    Nom(nom::error::Error<&'a str>),
}

type Result<'a, T> = std::result::Result<(&'a str, T), nom::Err<ParseError<'a>>>;

impl<'a> nom::error::ParseError<&'a str> for ParseError<'a> {
    fn from_error_kind(input: &'a str, kind: nom::error::ErrorKind) -> Self {
        ParseError::Nom(nom::error::Error::from_error_kind(input, kind))
    }

    fn append(_: &'a str, _: nom::error::ErrorKind, other: ParseError<'a>) -> Self {
        other
    }
}

impl std::fmt::Display for ParseElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ParseElement::Identifier => "identifier",
        })
    }
}

pub fn parse(input: &str) -> std::result::Result<Module, Vec<crate::error::Error>> {
    match module(input) {
        Ok((_, module)) => Ok(module),
        Err(e) => {
            let e = match e {
                nom::Err::Incomplete(_) => unreachable!(),
                nom::Err::Error(e) => e,
                nom::Err::Failure(e) => e,
            };
            let (note, position) = match &e {
                ParseError::Expected { element, position } => {
                    (format!("expected {element}"), *position)
                }
                ParseError::Nom(e) => ("expected ???".to_owned(), e.input.len()),
            };
            let note_span = Span {
                start: position,
                end: position - 1,
            };
            Err(vec![crate::error::Error {
                message: "parse error",
                note,
                note_span,
            }])
        }
    }
}

fn module(input: &str) -> Result<Module> {
    all_consuming(terminated(block(0), empty_lines))
        .map(|body| {
            let main = Function {
                name: "_start",
                args: Vec::new(),
                return_type: Some(Type {
                    segments: vec![Spanned::fake("int")],
                }),
                body,
            };
            let statements = vec![Statement::Function(main)];
            Module { statements }
        })
        .parse(input)
}

fn block<'a>(nesting: usize) -> impl Parser<'a, Vec<Statement<'a>>> {
    move |input| block_(nesting, input)
}

// Helper function seems to be necessary to break cyclic dependency of impl trait.
fn block_(nesting: usize, input: &str) -> Result<Vec<Statement>> {
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
            variable_declaration_statement(),
            increment_decrement_statement(),
            assignment_binary_statement(),
            assingment_statement(),
            expression_statement(),
        )),
    )
}

fn variable_declaration_statement<'a>() -> impl Parser<'a, Statement<'a>> {
    (
        spanned(spanned(identifier).map(Expression::Variable)),
        preceded(sp, opt(type_)),
        delimited((sp, char('='), sp), expression(), newline),
    )
        .map(|(left, type_, right)| Statement::Assignment { left, type_, right })
}

fn increment_decrement_statement<'a>() -> impl Parser<'a, Statement<'a>> {
    let increment = tag("++").map(|_| IncrementDecrementOperator::Increment);
    let decrement = tag("--").map(|_| IncrementDecrementOperator::Decrement);
    (expression(), preceded(sp, alt((increment, decrement))))
        .map(|(value, op)| Statement::IncrementDecrement { value, op })
}

fn assignment_binary_statement<'a>() -> impl Parser<'a, Statement<'a>> {
    let add = tag("+=").map(|_| BinaryOperator::Add);
    let subtract = tag("-=").map(|_| BinaryOperator::Subtract);
    let multiply = tag("*=").map(|_| BinaryOperator::Multiply);
    let divide = tag("/=").map(|_| BinaryOperator::Divide);
    let modulo = tag("%=").map(|_| BinaryOperator::Modulo);
    let bit_and = tag("&=").map(|_| BinaryOperator::BitAnd);
    let bit_or = tag("|=").map(|_| BinaryOperator::BitOr);
    let xor = tag("^=").map(|_| BinaryOperator::Xor);
    let shift_left = tag("<<=").map(|_| BinaryOperator::ShiftLeft);
    let shift_right = tag(">>=").map(|_| BinaryOperator::ShiftRight);
    let op = alt((
        add,
        subtract,
        multiply,
        divide,
        modulo,
        bit_and,
        bit_or,
        xor,
        shift_left,
        shift_right,
    ));
    (expression(), preceded(sp, op), expression())
        .map(|(left, op, right)| Statement::AssignmentBinary { op, left, right })
}

fn assingment_statement<'a>() -> impl Parser<'a, Statement<'a>> {
    (
        expression(),
        delimited((sp, char('='), sp), expression(), newline),
    )
        .map(|(left, right)| Statement::Assignment {
            left,
            type_: None,
            right,
        })
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
    preceded(
        tag("func"),
        cut((
            preceded(sp, identifier),
            preceded(
                (sp, char('(')),
                separated_list0(
                    preceded(sp, char(',')),
                    (preceded(sp, spanned(identifier)), preceded(sp, type_)),
                ),
            ),
            preceded((sp, char(')')), opt(type_)),
            preceded(newline, block(nesting + 1)),
        )),
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
        preceded((tag("struct"), sp), identifier),
        delimited(
            sp,
            opt(delimited(
                (char('('), sp),
                separated_list1(
                    (sp, char(','), sp),
                    (spanned(identifier), preceded(sp, type_)),
                ),
                (sp, char(')')),
            )),
            (sp, newline),
        ),
        many0(delimited(
            (empty_lines, indentiation(nesting + 1)),
            (identifier, type_),
            newline,
        )),
    )
        .map(|(name, args, fields)| {
            let args = args.unwrap_or_default();
            Statement::Struct(Struct { name, args, fields })
        })
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

fn expression<'a>() -> impl Parser<'a, Spanned<Expression<'a>>> {
    |input| expression6(input)
}

fn expression6(input: &str) -> Result<Spanned<Expression>> {
    let logic_and = tag("and").map(|_| BinaryOperator::LogicAnd);
    let logic_or = tag("or").map(|_| BinaryOperator::LogicOr);
    let op = alt((logic_and, logic_or));
    expression_binary_single(expression5, op, input)
}

fn expression5(input: &str) -> Result<Spanned<Expression>> {
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

fn expression4(input: &str) -> Result<Spanned<Expression>> {
    let and = char('&').map(|_| BinaryOperator::BitAnd);
    let or = char('|').map(|_| BinaryOperator::BitOr);
    let xor = char('^').map(|_| BinaryOperator::Xor);
    let shift_left = tag("<<").map(|_| BinaryOperator::ShiftLeft);
    let shift_right = tag(">>").map(|_| BinaryOperator::ShiftRight);
    let op = alt((and, or, xor, shift_left, shift_right));
    expression_binary_single(expression3, op, input)
}

fn expression3(input: &str) -> Result<Spanned<Expression>> {
    let add = char('+').map(|_| BinaryOperator::Add);
    let subtract = char('-').map(|_| BinaryOperator::Subtract);
    let op = alt((add, subtract));
    expression_binary(expression2, op, input)
}

fn expression2(input: &str) -> Result<Spanned<Expression>> {
    let multiply = char('*').map(|_| BinaryOperator::Multiply);
    let divide = char('/').map(|_| BinaryOperator::Divide);
    let modulo = char('%').map(|_| BinaryOperator::Modulo);
    let op = alt((multiply, divide, modulo));
    expression_binary(expression1, op, input)
}

fn expression_binary<'a>(
    mut sub_expr: impl Parser<'a, Spanned<Expression<'a>>>,
    op: impl Parser<'a, BinaryOperator>,
    input: &'a str,
) -> Result<'a, Spanned<Expression<'a>>> {
    let start = input.len();
    let (input, expression) = sub_expr.parse(input)?;
    fold_many0(
        (preceded(sp, spanned(op)), preceded(sp, sub_expr), position),
        || expression.clone(),
        |left, (op, right, end)| {
            Expression::BinaryOperation(op, Box::new((left, right))).with_span(Span { start, end })
        },
    )
    .parse(input)
}

fn expression_binary_single<'a>(
    mut sub_expr: impl Parser<'a, Spanned<Expression<'a>>>,
    op: impl Parser<'a, BinaryOperator>,
    input: &'a str,
) -> Result<'a, Spanned<Expression<'a>>> {
    let start = input.len();
    let (input, left) = sub_expr.parse(input)?;
    if let Ok((input, (op, right, end))) =
        (preceded(sp, spanned(op)), preceded(sp, sub_expr), position).parse(input)
    {
        Ok((
            input,
            Expression::BinaryOperation(op, Box::new((left, right))).with_span(Span { start, end }),
        ))
    } else {
        Ok((input, left))
    }
}

fn expression1(input: &str) -> Result<Spanned<Expression>> {
    let negate = char('-').map(|_| UnaryOperator::Negate);
    let bitnot = char('~').map(|_| UnaryOperator::BitNot);
    let not = (tag("not"), sp).map(|_| UnaryOperator::LogicNot);
    let op = alt((negate, bitnot, not));
    (position, opt(op), expression0, position)
        .map(|(start, op, expr, end)| match op {
            Some(op) => {
                Expression::UnaryOperation(op, Box::new(expr)).with_span(Span { start, end })
            }
            None => expr,
        })
        .parse(input)
}

fn expression0(input: &str) -> Result<Spanned<Expression>> {
    (
        position,
        alt((
            integer_literal,
            parentheses,
            list_literal,
            list_repeat,
            method_call,
            field_expression,
            function_call,
            index_expression,
            new_expression,
            string_literal,
            bool_literal,
            variable_reference,
        )),
        position,
    )
        .map(|(start, expr, end)| expr.with_span(Span { start, end }))
        .parse(input)
}

fn identifier(input: &str) -> Result<&str> {
    recognize(pair(
        verify(anychar, |c| c.is_ascii_alphabetic()),
        take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
    ))
    .parse(input)
    .map_err(|_: nom::Err<()>| {
        nom::Err::Error(ParseError::Expected {
            element: ParseElement::Identifier,
            position: input.len(),
        })
    })
}

fn integer_literal(input: &str) -> Result<Expression> {
    let (input, literal) = preceded(sp, digit1).parse(input)?;
    let literal = literal.parse().unwrap();
    let expression = Expression::Literal(literal);
    Ok((input, expression))
}

fn parentheses(input: &str) -> Result<Expression> {
    delimited((sp, char('(')), expression(), (sp, char(')')))
        .map(|expr| expr.value)
        .parse(input)
}

fn list_literal(input: &str) -> Result<Expression> {
    delimited(
        (sp, char('[')),
        separated_list0((sp, char(','), sp), expression()),
        (sp, char(']')),
    )
    .map(Expression::ListLiteral)
    .parse(input)
}

fn list_repeat(input: &str) -> Result<Expression> {
    delimited(
        (sp, char('[')),
        (expression(), preceded((sp, char(';'), sp), expression())),
        (sp, char(']')),
    )
    .map(|(initializer, length)| Expression::ListRepeat(Box::new((initializer, length))))
    .parse(input)
}

fn method_call(input: &str) -> Result<Expression> {
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

fn field_expression(input: &str) -> Result<Expression> {
    (variable_reference, preceded(char('.'), spanned(identifier)))
        .map(|(object, field)| Expression::Field(Box::new(object), field))
        .parse(input)
}

fn function_call(input: &str) -> Result<Expression> {
    (
        preceded(sp, spanned(identifier)),
        delimited(
            (sp, char('(')),
            spanned(separated_list0(
                preceded(sp, char(',')),
                preceded(sp, expression()),
            )),
            (sp, char(')')),
        ),
    )
        .map(|(func, args)| Expression::Call(func, args))
        .parse(input)
}

fn index_expression(input: &str) -> Result<Expression> {
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
fn new_expression(input: &str) -> Result<Expression> {
    preceded((sp, tag("new"), sp), type_)
        .map(Expression::New)
        .parse(input)
}

fn string_literal(input: &str) -> Result<Expression> {
    let normal = verify(take_while(|c| c != '"' && c != '\\'), |s: &str| {
        !s.is_empty()
    });
    let segment = alt((string_escape, normal));
    delimited(char('"'), many0(segment), char('"'))
        .map(Expression::StringLiteral)
        .parse(input)
}

fn string_escape(input: &str) -> Result<&str> {
    tag("\\n").map(|_| "\n").parse(input)
}

fn bool_literal(input: &str) -> Result<Expression> {
    let true_ = tag("true").map(|_| Expression::BoolLiteral(true));
    let false_ = tag("false").map(|_| Expression::BoolLiteral(false));
    preceded(sp, alt((true_, false_))).parse(input)
}

fn variable_reference(input: &str) -> Result<Expression> {
    preceded(sp, spanned(identifier))
        .map(Expression::Variable)
        .parse(input)
}

fn type_(input: &str) -> Result<Type> {
    many1(preceded(sp, spanned(identifier)))
        .map(|segments| Type { segments })
        .parse(input)
}

fn indentiation<'a>(nesting: usize) -> impl Parser<'a, ()> {
    count(indent, nesting).map(|_| ())
}

fn indent(input: &str) -> Result<()> {
    tag("    ").map(|_| ()).parse(input)
}

fn empty_lines(input: &str) -> Result<()> {
    many0(preceded(sp, alt((newline, comment))))
        .map(|_| ())
        .parse(input)
}

fn comment(input: &str) -> Result<()> {
    (tag("#"), take_while(|c| c != '\n'), char('\n'))
        .map(|_| ())
        .parse(input)
}

fn newline(input: &str) -> Result<()> {
    preceded(sp, char('\n')).map(|_| ()).parse(input)
}

fn sp(input: &str) -> Result<()> {
    take_while(|c| c == ' ').map(|_| ()).parse(input)
}

fn spanned<'a, T>(parser: impl Parser<'a, T>) -> impl Parser<'a, Spanned<T>> {
    (position, parser, position).map(|(start, value, end)| value.with_span(Span { start, end }))
}

fn position(input: &str) -> Result<usize> {
    Ok((input, input.len()))
}
