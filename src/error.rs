pub mod github_actions;
pub mod terminal;

use std::ops::Deref;

pub trait SpanExt: Sized {
    fn with_span(self, span: Span) -> Spanned<Self>;
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

#[derive(Clone, Copy)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

pub enum ErrorFormat {
    GithubActions,
    Terminal,
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

impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.value)
    }
}
