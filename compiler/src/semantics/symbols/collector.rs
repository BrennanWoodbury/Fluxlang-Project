//! Symbol collection for module-level declarations.
//!
//! This pass walks parsed AST items, predeclares them in the symbol table, and
//! records diagnostics for duplicate definitions. Downstream phases can reuse
//! the resulting `SymbolResolver` when resolving nested scopes.

use core::ast::nodes::{Ident, Item};
use core::diag::Span;

use crate::semantics::symbols::resolver::{DeclarationResult, SymbolResolver};
use crate::semantics::symbols::symbol::SymbolKind;
use crate::semantics::typeck::diagnostics::{SemanticDiagnostic, SemanticErrorCode};

/// Output produced by the collection pass.
#[derive(Debug)]
pub struct SymbolCollection {
    pub resolver: SymbolResolver,
    pub diagnostics: Vec<SemanticDiagnostic>,
}

impl SymbolCollection {
    pub fn new(resolver: SymbolResolver, diagnostics: Vec<SemanticDiagnostic>) -> Self {
        Self {
            resolver,
            diagnostics,
        }
    }
}

/// Collect symbols from a module and return the resulting resolver plus
/// diagnostics.
pub fn collect_module_symbols(items: &[Item]) -> SymbolCollection {
    let mut resolver = SymbolResolver::new();
    let mut diagnostics = Vec::new();

    for item in items {
        if let Some((name, span, kind)) = item_signature(item) {
            match resolver.declare(name.name.clone(), kind, span.clone()) {
                DeclarationResult::Ok(_) => {}
                DeclarationResult::Duplicate { existing } => {
                    let message =
                        format!("duplicate definition of {} `{}`", kind.as_str(), name.name);

                    diagnostics.push(SemanticDiagnostic::new(
                        message,
                        span.clone().unwrap_or(Span { start: 0, end: 0 }),
                        SemanticErrorCode::DuplicateSymbol,
                    ));

                    if let Some(existing_symbol) = resolver.symbol(existing) {
                        if let Some(existing_span) = existing_symbol.span.clone() {
                            diagnostics.push(SemanticDiagnostic::new(
                                format!(
                                    "previous `{}` definition here",
                                    existing_symbol.kind.as_str()
                                ),
                                existing_span,
                                SemanticErrorCode::DuplicateSymbol,
                            ));
                        }
                    }
                }
            }
        }
    }

    SymbolCollection::new(resolver, diagnostics)
}

fn item_signature(item: &Item) -> Option<(Ident, Option<Span>, SymbolKind)> {
    match item {
        Item::Fn(func) => Some((
            func.name.clone(),
            func.name.span.clone(),
            SymbolKind::Function,
        )),
        Item::Obj(obj) => Some((obj.name.clone(), obj.name.span.clone(), SymbolKind::Object)),
        Item::Interface(interface) => Some((
            interface.name.clone(),
            interface.name.span.clone(),
            SymbolKind::Interface,
        )),
        Item::Use(use_decl) => {
            let name = use_decl.alias.clone().unwrap_or_else(|| {
                use_decl.path.last().cloned().unwrap_or_else(|| Ident {
                    name: String::from("_"),
                    span: None,
                })
            });
            Some((name.clone(), name.span.clone(), SymbolKind::Use))
        }
        Item::Impl(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::ast::nodes::{Block, FnDecl, GenericParams};

    fn ident(name: &str) -> Ident {
        Ident {
            name: name.to_string(),
            span: Some(Span {
                start: 0,
                end: name.len(),
            }),
        }
    }

    fn empty_fn(name: &str) -> Item {
        Item::Fn(FnDecl {
            vis: core::ast::nodes::Visibility::Private,
            name: ident(name),
            generics: GenericParams { params: Vec::new() },
            params: Vec::new(),
            ret_type: None,
            body: Block { stmts: Vec::new() },
        })
    }

    #[test]
    fn collects_unique_symbols() {
        let items = vec![empty_fn("alpha"), empty_fn("beta")];
        let collection = collect_module_symbols(&items);
        assert!(collection.diagnostics.is_empty());
        assert_eq!(collection.resolver.symbol_table_len(), 2);
    }

    #[test]
    fn reports_duplicates() {
        let items = vec![empty_fn("alpha"), empty_fn("alpha")];
        let collection = collect_module_symbols(&items);
        assert_eq!(collection.diagnostics.len(), 2);
        assert_eq!(collection.resolver.symbol_table_len(), 1);
    }
}
