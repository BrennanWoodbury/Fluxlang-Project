//! Runtime and compile-time error payload types.
//!
//! These structures describe the minimal information needed to
//! propagate user-facing errors, including stack traces captured by the
//! interpreter or compiler diagnostics.

use crate::types::TypeId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorObject {
    pub type_id: TypeId,
    pub message: String,
    pub trace: Vec<StackFrame>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackFrame {
    pub function: String,
    pub file: String,
    pub line: u32,
    pub column: u32,
}
