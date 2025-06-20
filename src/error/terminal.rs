use crate::error::Error;

pub struct Colors {
    red: &'static str,
    blue: &'static str,
    bold: &'static str,
    reset: &'static str,
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

pub fn format_errors(errors: &[Error], source: &str, filename: &str, colors: &Colors) -> String {
    errors
        .iter()
        .flat_map(|error| {
            format_error(error, source, filename, colors)
                .into_chars()
                .chain(std::iter::once('\n'))
        })
        .collect()
}

fn format_error(error: &Error, source: &str, filename: &str, colors: &Colors) -> String {
    let Error {
        message,
        note,
        note_span,
    } = error;
    if note_span.end > source.len() {
        return format!("standard library error {error:?}");
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
    let line_end = line_start
        + source[line_start..]
            .find('\n')
            .unwrap_or(source.len() - line_start);
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
