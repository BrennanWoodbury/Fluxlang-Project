//! High-level intermediate representation (HIR) support.
//!
//! This module owns arena-backed HIR nodes together with utilities for
//! inspecting and formatting typed semantic structures. The contents remain
//! intentionally lightweight during scaffolding and will grow alongside the
//! semantic analysis pipeline.

pub mod arena;
pub mod display;
pub mod ids;
pub mod nodes;
pub mod types;

pub use arena::{Arena, ArenaIndex};
pub use display::{HirFormatter, format_module};
pub use ids::{
    HirBlockId, HirExprId, HirItemId, HirNodeId, HirStmtId, ScopeId, SymbolId, TraitImplId,
    TypeCtxId,
};
pub use nodes::{
    HirBlock, HirConst, HirExpr, HirExprKind, HirField, HirFnSignature, HirFunction, HirInterface,
    HirItem, HirItemKind, HirLiteral, HirModule, HirObject, HirParam, HirStmt, HirStmtKind,
    HirTraitImpl, HirUse,
};
pub use types::{PrimitiveTypes, TypeContext, TypeInfo};
