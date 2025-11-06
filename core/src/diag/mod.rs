//! Diagnostic utilities shared across the core crate.
//!
//! This module centralises reusable metadata such as source spans and
//! error payloads so parsers, semantic passes, and the VM can agree on a
//! common representation.

pub mod error;
pub mod span;

pub use error::{ErrorObject, StackFrame};
pub use span::Span;

/// Parser-specific diagnostic codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParseErrorCode {
    UnexpectedToken,
    ExpectedItem,
    ExpectedUsePath,
    ExpectedFunction,
    ExpectedExpression,
    ExpectedArrayElement,
    ExpectedMapEntry,
    InvalidIntegerLiteral,
    InvalidFloatLiteral,
    InvalidArrayLength,
}
