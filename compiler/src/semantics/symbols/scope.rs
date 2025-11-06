//! Lexical scope representation.
//!
//! Scopes will eventually track bindings, visibility, and lifetime metadata.
//! The scaffolding here keeps the structure light while we focus on
//! establishing the rest of the semantic pipeline.

use std::collections::HashMap;

use crate::semantics::hir::ids::{ScopeId, SymbolId};

/// Minimal information stored for each scope.
#[derive(Debug, Default)]
pub struct Scope {
    /// Identifier assigned to the scope.
    pub id: ScopeId,
    /// Optional parent scope enabling lookups to walk upwards.
    pub parent: Option<ScopeId>,
    /// Mapping of symbol names to identifiers.
    pub symbols: HashMap<String, SymbolId>,
}

impl Scope {
    /// Construct a new scope with the supplied identifier.
    pub fn new(id: ScopeId, parent: Option<ScopeId>) -> Self {
        Self {
            id,
            parent,
            symbols: HashMap::new(),
        }
    }

    /// Attempt to insert a symbol; returns the previous entry if one existed.
    pub fn insert_symbol(&mut self, name: String, id: SymbolId) -> Option<SymbolId> {
        self.symbols.insert(name, id)
    }

    /// Lookup a symbol by name within this scope only.
    pub fn lookup(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }
}

/// Owner for all scope objects within a module.
#[derive(Debug, Default)]
pub struct ScopeMap {
    map: HashMap<ScopeId, Scope>,
    next_id: u32,
}

impl ScopeMap {
    /// Create an empty scope map.
    pub fn new() -> Self {
        Self::default()
    }

    /// Allocate a new scope and return a mutable reference to it.
    pub fn alloc_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let id = ScopeId::from_raw(self.next_id);
        self.next_id += 1;
        let scope = Scope::new(id, parent);
        self.map.insert(id, scope);
        id
    }

    /// Fetch an immutable reference to a scope.
    pub fn get(&self, id: ScopeId) -> Option<&Scope> {
        self.map.get(&id)
    }

    /// Fetch a mutable reference to a scope.
    pub fn get_mut(&mut self, id: ScopeId) -> Option<&mut Scope> {
        self.map.get_mut(&id)
    }

    /// Retrieve the parent of a scope, if any.
    pub fn parent(&self, id: ScopeId) -> Option<ScopeId> {
        self.map.get(&id).and_then(|scope| scope.parent)
    }

    /// Iterate over all stored scopes.
    pub fn iter(&self) -> impl Iterator<Item = (&ScopeId, &Scope)> {
        self.map.iter()
    }
}

/// Stack helper used while traversing nested scopes.
#[derive(Debug, Default)]
pub struct ScopeIdStack {
    stack: Vec<ScopeId>,
}

impl ScopeIdStack {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn push(&mut self, scope: ScopeId) {
        self.stack.push(scope);
    }

    pub fn pop(&mut self) -> Option<ScopeId> {
        self.stack.pop()
    }

    pub fn current(&self) -> Option<ScopeId> {
        self.stack.last().copied()
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }
}
