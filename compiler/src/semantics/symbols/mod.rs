//! Symbol table scaffolding.
//!
//! Symbol management underpins name resolution, type checking, and later code
//! generation. The files in this module provide the initial structures that
//! subsequent phases will flesh out.

pub mod collector;
pub mod resolver;
pub mod scope;
pub mod symbol;

pub use collector::{SymbolCollection, collect_module_symbols};
pub use resolver::{DeclarationResult, SymbolResolver};
pub use scope::{Scope, ScopeIdStack, ScopeMap};
pub use symbol::{Symbol, SymbolIdMap, SymbolIdSet, SymbolKind, SymbolTable};
