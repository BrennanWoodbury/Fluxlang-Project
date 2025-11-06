//! Semantic analysis facade.
//!
//! This module ties together literal lowering, the HIR scaffolding, and future
//! subsystems responsible for name resolution and type checking. The exports
//! here keep existing literal functionality available while the remaining
//! pieces are implemented.

pub mod eval;
pub mod hir;
pub mod literals;
pub mod lowering;
pub mod symbols;
pub mod typeck;

pub use literals::{ArrayTypeHints, LiteralAnalyzer, MapTypeHints};
