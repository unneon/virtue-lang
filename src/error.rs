use std::ops::Deref;

pub trait SpanExt: Sized {
    fn with_span(self, span: Span) -> Spanned<Self>;
}

pub struct Colors {
    red: &'static str,
    blue: &'static str,
    bold: &'static str,
    reset: &'static str,
}

#[derive(Debug)]
pub struct Error {
    pub message: &'static str,
    pub note: String,
    pub note_span: Span,
}

#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

pub const NO_COLORS: Colors = Colors {
    red: "",
    blue: "",
    bold: "",
    reset: "",
};

pub const ANSI_COLORS: Colors = Colors {
    red: "\x1B[1;31m",
    blue: "\x1B[1;36m",
    bold: "\x1B[1m",
    reset: "\x1B[0m",
};

impl Error {
    fn format(&self, source: &str, filename: &str, colors: &Colors) -> String {
        let Error {
            message,
            note,
            note_span,
        } = self;
        if note_span.end > source.len() {
            return format!("standard library error {self:?}");
        }
        let Colors {
            red,
            blue,
            bold,
            reset,
        } = colors;
        let start = source.len() - note_span.start;
        let end = source.len() - note_span.end;
        let line = source[..start].chars().filter(|c| *c == '\n').count() + 1;
        let line_start = source[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
        let line_end = line_start + source[line_start..].find('\n').unwrap_or(source.len());
        let file_padding_indent = " ".repeat(line.ilog10() as usize);
        let line_padding_indent = " ".repeat(line.ilog10() as usize + 2);
        let column = start - line_start + 1;
        let code = &source[line_start..line_end];
        let note_indent = " ".repeat(column - 1);
        let note_highlight = "^".repeat(end - start);
        let note_space = if note.is_empty() { "" } else { " " };
        format!(
            r#"{red}error:{reset} {bold}{message}{reset}
 {file_padding_indent}{blue}-->{reset} {filename}:{line}:{column}
 {line_padding_indent}{blue}|{reset}
 {blue}{line}{reset} {blue}|{reset} {code}
 {line_padding_indent}{blue}|{reset} {note_indent}{red}{note_highlight}{note_space}{note}{reset}
"#
        )
    }
}

impl<T> Spanned<T> {
    pub fn fake(value: T) -> Spanned<T> {
        Spanned {
            value,
            span: Span { start: 0, end: 0 },
        }
    }
}

impl<T> SpanExt for T {
    fn with_span(self, span: Span) -> Spanned<T> {
        Spanned { value: self, span }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

pub fn format_errors(errors: &[Error], source: &str, filename: &str, colors: &Colors) -> String {
    errors
        .iter()
        .flat_map(|error| {
            error
                .format(source, filename, colors)
                .into_chars()
                .chain(std::iter::once('\n'))
        })
        .collect()
}
