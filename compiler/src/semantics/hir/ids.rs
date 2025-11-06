//! Strongly typed identifier wrappers used throughout the semantic layer.
//!
//! Using dedicated newtypes instead of raw integers makes the compiler safer
//! and clarifies which identifiers belong to which tables.

use std::fmt;

macro_rules! define_id {
    ($name:ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Default)]
        pub struct $name(pub u32);

        impl $name {
            /// Construct an identifier from a raw value.
            pub const fn from_raw(raw: u32) -> Self {
                Self(raw)
            }

            /// Retrieve the underlying integer value.
            pub const fn to_raw(self) -> u32 {
                self.0
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!(stringify!($name), "({})"), self.0)
            }
        }
    };
}

define_id!(SymbolId);
define_id!(ScopeId);
define_id!(TraitImplId);
define_id!(HirNodeId);
define_id!(HirItemId);
define_id!(HirStmtId);
define_id!(HirExprId);
define_id!(HirBlockId);
define_id!(TypeCtxId);
