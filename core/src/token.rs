//! Compatibility facade for legacy `core::token` imports.
//!
//! AST items now reside in `core::ast` and diagnostic structures live in
//! `core::diag`. Re-exporting them from this module preserves the
//! original module path while the codebase migrates to the new layout.

pub use crate::ast::TypeIdentKind;
pub use crate::ast::nodes::*;
pub use crate::ast::ops::{BinaryOp, UnaryOp};
pub use crate::ast::tokens::TokenKind;
pub use crate::diag::{ErrorObject, Span, StackFrame};
