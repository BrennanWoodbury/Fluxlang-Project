//! Symbol table entries and helpers.
//!
//! Symbols describe named program elements (functions, variables, objects) and
//! are stored in a central table so multiple semantic passes can share the
//! same metadata without duplication.

use std::collections::{HashMap, HashSet};

use core::diag::Span;

use crate::semantics::hir::ids::{ScopeId, SymbolId};

/// Classification of a symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Module,
    Function,
    Object,
    Interface,
    Implementation,
    Use,
    Const,
    Let,
    Var,
    Parameter,
    Field,
}

impl SymbolKind {
    /// Human-readable description used in diagnostics.
    pub fn as_str(self) -> &'static str {
        match self {
            SymbolKind::Module => "module",
            SymbolKind::Function => "function",
            SymbolKind::Object => "object",
            SymbolKind::Interface => "interface",
            SymbolKind::Implementation => "implementation",
            SymbolKind::Use => "use",
            SymbolKind::Const => "const",
            SymbolKind::Let => "let",
            SymbolKind::Var => "var",
            SymbolKind::Parameter => "parameter",
            SymbolKind::Field => "field",
        }
    }
}

/// Metadata describing a single symbol.
#[derive(Debug, Clone)]
pub struct Symbol {
    pub id: SymbolId,
    pub name: String,
    pub kind: SymbolKind,
    pub span: Option<Span>,
    pub scope: ScopeId,
}

impl Symbol {
    /// Convenience constructor.
    pub fn new(
        id: SymbolId,
        name: impl Into<String>,
        kind: SymbolKind,
        span: Option<Span>,
        scope: ScopeId,
    ) -> Self {
        Self {
            id,
            name: name.into(),
            kind,
            span,
            scope,
        }
    }
}

/// Central registry of all symbols declared within a compilation unit.
#[derive(Debug, Default)]
pub struct SymbolTable {
    entries: Vec<Symbol>,
}

impl SymbolTable {
    /// Create an empty symbol table.
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Allocate a new symbol entry.
    pub fn alloc(
        &mut self,
        scope: ScopeId,
        name: impl Into<String>,
        kind: SymbolKind,
        span: Option<Span>,
    ) -> SymbolId {
        let id = SymbolId::from_raw(self.entries.len() as u32);
        let symbol = Symbol::new(id, name, kind, span, scope);
        self.entries.push(symbol);
        id
    }

    /// Retrieve a symbol by identifier.
    pub fn get(&self, id: SymbolId) -> Option<&Symbol> {
        self.entries.get(id.to_raw() as usize)
    }

    /// Iterate over stored symbols.
    pub fn iter(&self) -> impl Iterator<Item = &Symbol> {
        self.entries.iter()
    }

    /// Number of registered symbols.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the table currently has no entries.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// Convenience alias for maps keyed by symbol identifiers.
pub type SymbolIdMap<V> = HashMap<SymbolId, V>;

/// Convenience alias for sets of symbol identifiers.
pub type SymbolIdSet = HashSet<SymbolId>;
