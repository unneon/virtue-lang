use crate::error::Error;

pub fn format_errors(errors: &[Error], source: &str, filename: &str) -> String {
    errors
        .iter()
        .flat_map(|error| format_error(error, source, filename).into_chars())
        .collect()
}

fn format_error(error: &Error, source: &str, filename: &str) -> String {
    let Error {
        message,
        note,
        note_span,
    } = error;
    if note_span.end > source.len() {
        return format!("standard library error {error:?}");
    }
    let start = source.len() - note_span.start;
    let end = source.len() - note_span.end;
    let line = source[..start].chars().filter(|c| *c == '\n').count() + 1;
    let line_start = source[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let column_start = start - line_start + 1;
    let column_end = column_start + end - start - 1;
    format!(
        "::error file={filename},line={line},endLine={line},col={column_start},endColumn={column_end},title={message}::{note}\n"
    )
}
