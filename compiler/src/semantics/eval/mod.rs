//! Constant folding and simple evaluation scaffolding.
//!
//! Later passes will reuse this module to evaluate pure expressions during
//! semantic analysis. The current implementation only defines the API shape
//! used by higher-level orchestration code.

use crate::semantics::hir::nodes::HirModule;

/// Placeholder constant folder.
#[derive(Debug, Default)]
pub struct ConstantFolder;

impl ConstantFolder {
    /// Create a new constant folder.
    pub fn new() -> Self {
        Self
    }

    /// Run the constant folding pass over a module.
    pub fn fold_module(&self, module: &mut HirModule) {
        let _ = module;
    }
}
