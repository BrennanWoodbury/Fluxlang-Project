//! Type context scaffolding for the semantic layer.
//!
//! The type context will eventually host interning, unification helpers, and
//! lookup tables that power FluxLang's gradual type system. The initial
//! version tracks a list of known types so callers can obtain stable
//! identifiers while avoiding duplicate allocations.

use std::cell::RefCell;
use std::collections::HashMap;

use core::types::TypeId;

use super::ids::TypeCtxId;

/// High-level categories of FluxLang types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeInfo {
    /// The dynamic `any` type used for `let` bindings and fallback cases.
    Any,
    /// Nominal types imported from the `core::types` module.
    Concrete(TypeId),
}

impl TypeInfo {
    /// Convenience constructor for nominal types.
    pub fn nominal(name: impl Into<String>) -> Self {
        TypeInfo::Concrete(TypeId::new(name))
    }
}

/// Owned table of type information.
#[derive(Debug, Default, Clone)]
pub struct TypeContext {
    entries: RefCell<Vec<TypeInfo>>,
    index: RefCell<HashMap<TypeInfo, TypeCtxId>>,
}

impl TypeContext {
    /// Create an empty type table.
    pub fn new() -> Self {
        Self {
            entries: RefCell::new(Vec::new()),
            index: RefCell::new(HashMap::new()),
        }
    }

    /// Insert a new type description and return its identifier.
    pub fn intern(&self, info: TypeInfo) -> TypeCtxId {
        if let Some(existing) = self.index.borrow().get(&info) {
            return *existing;
        }

        let mut entries = self.entries.borrow_mut();
        let mut index = self.index.borrow_mut();
        let id = TypeCtxId::from_raw(entries.len() as u32);
        entries.push(info.clone());
        index.insert(info, id);
        id
    }

    /// Convenience for interning concrete `TypeId` values.
    pub fn intern_concrete(&self, ty: TypeId) -> TypeCtxId {
        self.intern(TypeInfo::Concrete(ty))
    }

    /// Intern the dynamic `any` type.
    pub fn intern_any(&self) -> TypeCtxId {
        self.intern(TypeInfo::Any)
    }

    /// Fetch a type description by identifier.
    pub fn get(&self, id: TypeCtxId) -> Option<TypeInfo> {
        self.entries.borrow().get(id.to_raw() as usize).cloned()
    }

    /// Number of registered types.
    pub fn len(&self) -> usize {
        self.entries.borrow().len()
    }

    /// Whether the context currently has no entries.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Create a context pre-populated with core primitive types.
    pub fn with_primitives() -> (Self, PrimitiveTypes) {
        let ctx = TypeContext::new();
        let any = ctx.intern(TypeInfo::Any);
        let int = ctx.intern_concrete(TypeId::int());
        let float = ctx.intern_concrete(TypeId::float());
        let string = ctx.intern_concrete(TypeId::string());
        let bool = ctx.intern_concrete(TypeId::bool());
        let unit = ctx.intern_concrete(TypeId::unit());
        (
            ctx,
            PrimitiveTypes {
                any,
                int,
                float,
                string,
                bool,
                unit,
            },
        )
    }

    /// Retrieve the primitive type handles used throughout the type context.
    pub fn primitives(&self) -> PrimitiveTypes {
        PrimitiveTypes {
            any: self.intern(TypeInfo::Any),
            int: self.intern_concrete(TypeId::int()),
            float: self.intern_concrete(TypeId::float()),
            string: self.intern_concrete(TypeId::string()),
            bool: self.intern_concrete(TypeId::bool()),
            unit: self.intern_concrete(TypeId::unit()),
        }
    }
}

/// Convenient handles to commonly used primitive type identifiers.
#[derive(Debug, Clone, Copy)]
pub struct PrimitiveTypes {
    pub any: TypeCtxId,
    pub int: TypeCtxId,
    pub float: TypeCtxId,
    pub string: TypeCtxId,
    pub bool: TypeCtxId,
    pub unit: TypeCtxId,
}
