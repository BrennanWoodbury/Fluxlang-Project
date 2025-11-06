//! FluxLang abstract syntax tree and token types.
//!
//! The submodules organise lexical tokens, operator enums, and high
//! level AST nodes produced by the parser. Re-exports at this level give
//! downstream crates a convenient single import path.

pub mod nodes;
pub mod ops;
pub mod tokens;

pub use nodes::*;
pub use ops::{BinaryOp, UnaryOp};
pub use tokens::TokenKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeIdentKind {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Bool,
    Char,
    Int,
    UInt,
    Float,
    String,
    #[cfg(feature = "bigint")]
    I256,
    #[cfg(feature = "bigint")]
    U256,
}
