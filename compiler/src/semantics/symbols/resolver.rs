//! Name resolution scaffolding.
//!
//! The resolver coordinates interactions between the AST, symbol tables, and
//! diagnostic reporting. The initial version focuses on providing an API
//! surface area that later phases can extend with real logic.

use core::diag::Span;

use crate::semantics::hir::ids::{ScopeId, SymbolId};
use crate::semantics::symbols::scope::{Scope, ScopeIdStack, ScopeMap};
use crate::semantics::symbols::symbol::{Symbol, SymbolKind, SymbolTable};

/// Temporary resolver placeholder.
#[derive(Debug)]
pub struct SymbolResolver {
    scopes: ScopeMap,
    root_scope: ScopeId,
    scope_stack: ScopeIdStack,
    symbols: SymbolTable,
}

impl SymbolResolver {
    /// Create a resolver with a fresh root scope.
    pub fn new() -> Self {
        let mut scopes = ScopeMap::new();
        let root_scope = scopes.alloc_scope(None);
        let mut scope_stack = ScopeIdStack::new();
        scope_stack.push(root_scope);
        Self {
            scopes,
            root_scope,
            scope_stack,
            symbols: SymbolTable::new(),
        }
    }

    /// Access the root scope identifier.
    pub fn root_scope(&self) -> ScopeId {
        self.root_scope
    }

    /// Enter a new child scope and push it onto the stack.
    pub fn push_child_scope(&mut self) -> ScopeId {
        let parent = self.current_scope();
        let child = self.scopes.alloc_scope(Some(parent));
        self.scope_stack.push(child);
        child
    }

    /// Pop the current scope from the stack.
    pub fn pop_scope(&mut self) -> Option<ScopeId> {
        // Prevent removing the root scope.
        if self.scope_stack.is_empty() {
            return None;
        }

        if let Some(current) = self.scope_stack.current() {
            if current == self.root_scope {
                return None;
            }
        }

        self.scope_stack.pop()
    }

    /// Retrieve the active scope identifier.
    pub fn current_scope(&self) -> ScopeId {
        self.scope_stack.current().unwrap_or(self.root_scope)
    }

    /// Declare a symbol in the current scope, returning the outcome.
    pub fn declare(
        &mut self,
        name: impl Into<String>,
        kind: SymbolKind,
        span: Option<Span>,
    ) -> DeclarationResult {
        let name_string = name.into();
        let scope_id = self.current_scope();

        if let Some(existing) = self.lookup_in_scope(scope_id, &name_string) {
            return DeclarationResult::Duplicate { existing };
        }

        let symbol_id = self
            .symbols
            .alloc(scope_id, name_string.clone(), kind, span);
        if let Some(scope) = self.scopes.get_mut(scope_id) {
            scope.insert_symbol(name_string, symbol_id);
        }

        DeclarationResult::Ok(symbol_id)
    }

    /// Lookup a symbol with the given name within the current scope.
    pub fn lookup_current(&self, name: &str) -> Option<SymbolId> {
        self.lookup_in_scope(self.current_scope(), name)
    }

    /// Retrieve a scope by identifier.
    pub fn scope(&self, id: ScopeId) -> Option<&Scope> {
        self.scopes.get(id)
    }

    /// Retrieve a symbol by identifier.
    pub fn symbol(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(id)
    }

    /// Number of registered symbols.
    pub fn symbol_table_len(&self) -> usize {
        self.symbols.len()
    }

    fn lookup_in_scope(&self, scope: ScopeId, name: &str) -> Option<SymbolId> {
        let mut current = Some(scope);
        while let Some(scope_id) = current {
            if let Some(scope) = self.scopes.get(scope_id) {
                if let Some(symbol) = scope.lookup(name) {
                    return Some(symbol);
                }
                current = scope.parent;
            } else {
                break;
            }
        }
        None
    }
}

/// Outcome of attempting to declare a symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclarationResult {
    Ok(SymbolId),
    Duplicate { existing: SymbolId },
}
