//! Semantic diagnostic placeholders.
//!
//! Diagnostics emitted during type checking and name resolution will augment
//! the parser's error reporting. This module lays down the shared structures
//! that later passes will populate.

use core::diag::Span;

/// Enumerates semantic error categories.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemanticErrorCode {
    UnresolvedSymbol,
    DuplicateSymbol,
    TypeMismatch,
    UnsupportedOperation,
    UnknownType,
    MissingTypeAnnotation,
    ExpectedBoolean,
}

/// Semantic diagnostic payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticDiagnostic {
    /// Human-readable message summarising the issue.
    pub message: String,
    /// Source span associated with the diagnostic.
    pub span: Span,
    /// Categorical identifier for programmatic handling.
    pub code: SemanticErrorCode,
}

impl SemanticDiagnostic {
    /// Create a new diagnostic.
    pub fn new(message: impl Into<String>, span: Span, code: SemanticErrorCode) -> Self {
        Self {
            message: message.into(),
            span,
            code,
        }
    }
}
