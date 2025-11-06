//! Constraint solving placeholders.
//!
//! Constraint sets enable the type checker to defer decisions until sufficient
//! information is available. The stubbed implementation captures the shape of
//! the API without enforcing any particular algorithm yet.

use std::collections::VecDeque;

use crate::semantics::hir::ids::{HirExprId, TypeCtxId};

/// Basic representation of a type constraint awaiting resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstraint {
    /// Expression subject to the constraint.
    pub expr: HirExprId,
    /// Expected type for the expression.
    pub expected: TypeCtxId,
}

/// Collection of outstanding constraints.
#[derive(Debug, Default)]
pub struct ConstraintSet {
    queue: VecDeque<TypeConstraint>,
}

impl ConstraintSet {
    /// Record a new constraint.
    pub fn push(&mut self, constraint: TypeConstraint) {
        self.queue.push_back(constraint);
    }

    /// Fetch the next constraint to process.
    pub fn pop(&mut self) -> Option<TypeConstraint> {
        self.queue.pop_front()
    }

    /// Number of unresolved constraints.
    pub fn len(&self) -> usize {
        self.queue.len()
    }

    /// Whether the set contains no constraints.
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }
}
