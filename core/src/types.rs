//! Shared nominal type identifiers.
//!
//! These lightweight handles let the runtime and diagnostics refer to
//! concrete FluxLang types without depending on heavier metadata.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeId {
    pub name: String,
}

impl TypeId {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
}
