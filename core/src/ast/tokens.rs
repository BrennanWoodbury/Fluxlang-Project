//! Token definitions for FluxLang's lexer.
//!
//! This module enumerates every lexical token emitted by the scanner,
//! keeping literal data and keyword categories in one place.

use super::TypeIdentKind;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // identifiers
    Ident(String),
    TypeIdent(TypeIdentKind),

    // keywords
    KwObj,
    KwInterface,
    KwImpl,
    KwPub,
    KwUse,
    KwAs,
    KwReturn,
    KwThrow,
    KwPanic,
    KwLoop,
    KwFn,
    KwConst,
    KwLet,
    KwVar,

    // punctuation
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Lt,
    Gt,
    Comma,
    Dot,
    Semicolon,
    Colon,
    ColonColon,
    Arrow,

    // operators
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Bang,
    EqEq,
    EqEqEq,
    NotEq,
    LtEq,
    GtEq,

    // literals
    IntLit {
        value: String,
        ty: Option<TypeIdentKind>,
    },
    FloatLit {
        value: String,
        ty: Option<TypeIdentKind>,
    },
    StringLit {
        value: String,
        size_suffix: Option<u32>,
    },

    // misc
    Eof,
}
