//! Source span helpers used throughout diagnostic reporting.
//!
//! A `Span` tracks byte offsets within a single file and is attached to
//! identifiers and AST nodes so higher layers can surface precise errors.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
