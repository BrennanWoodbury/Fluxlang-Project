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

    pub fn int() -> Self {
        Self::new("int")
    }

    pub fn float() -> Self {
        Self::new("float")
    }

    pub fn string() -> Self {
        Self::new("string")
    }

    pub fn bool() -> Self {
        Self::new("bool")
    }

    pub fn any() -> Self {
        Self::new("any")
    }

    pub fn range() -> Self {
        Self::new("range")
    }

    pub fn array_of(element: &TypeId) -> Self {
        Self::new(format!("Array<{}>", element.name))
    }

    pub fn map_of(key: &TypeId, value: &TypeId) -> Self {
        Self::new(format!("Map<{}, {}>", key.name, value.name))
    }
}
