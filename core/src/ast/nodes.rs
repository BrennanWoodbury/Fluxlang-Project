//! Abstract syntax tree node definitions.
//!
//! These types represent parsed FluxLang source code after lexing.
//! The structures are intentionally plain so they can be shared across
//! semantic analysis, lowering, and diagnostic reporting passes.

use super::{BinaryOp, TypeIdentKind, UnaryOp};
use crate::diag::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: String,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Obj(ObjDecl),
    Interface(InterfaceDecl),
    Impl(ImplBlock),
    Fn(FnDecl),
    Use(UseDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjDecl {
    pub vis: Visibility,
    pub name: Ident,
    pub generics: GenericParams,
    pub fields: Vec<FieldDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldDecl {
    pub name: Ident,
    pub ty: TypeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDecl {
    pub vis: Visibility,
    pub name: Ident,
    pub generics: GenericParams,
    pub methods: Vec<FnSig>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplBlock {
    pub vis: Visibility,
    pub generics: GenericParams,
    pub target: TypeExpr,
    pub interface: Option<TypeExpr>,
    pub methods: Vec<FnDecl>,
    pub span: Option<Span>,
}

impl ImplBlock {
    pub fn new(
        vis: Visibility,
        generics: GenericParams,
        target: TypeExpr,
        interface: Option<TypeExpr>,
        methods: Vec<FnDecl>,
        span: Option<Span>,
    ) -> Self {
        Self {
            vis,
            generics,
            target,
            interface,
            methods,
            span,
        }
    }

    pub fn is_trait_impl(&self) -> bool {
        self.interface.is_some()
    }

    pub fn is_inherent(&self) -> bool {
        self.interface.is_none()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    pub vis: Visibility,
    pub name: Ident,
    pub generics: GenericParams,
    pub params: Vec<Param>,
    pub ret_type: Option<TypeExpr>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnSig {
    pub vis: Visibility,
    pub name: Ident,
    pub generics: GenericParams,
    pub params: Vec<Param>,
    pub ret_type: Option<TypeExpr>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Ident,
    pub ty: Option<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseDecl {
    pub vis: Visibility,
    pub path: Vec<Ident>,
    pub alias: Option<Ident>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl {
    pub name: Ident,
    pub ty: Option<TypeExpr>,
    pub value: Expr,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDecl {
    pub name: Ident,
    pub ty: Option<TypeExpr>,
    pub value: Option<Expr>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub name: Ident,
    pub ty: TypeExpr,
    pub value: Option<Expr>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Const(ConstDecl),
    Let(LetDecl),
    Var(VarDecl),
    Expr(Expr),
    Return(Option<Expr>),
    Throw(Expr),
    Panic(Option<Expr>),
    Assign { target: Expr, value: Expr },
    Block(Block),
    For(Box<ForStmt>),
    Loop(Block),
    While { cond: Expr, body: Block },
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub init: Option<Box<Stmt>>,
    pub cond: Option<Expr>,
    pub step: Option<Expr>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(Ident),
    IntLit {
        value: i128,
        ty: Option<TypeIdentKind>,
    },
    FloatLit {
        value: f64,
        ty: Option<u32>,
    },
    StringLit {
        value: String,
        size: Option<u32>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Member {
        object: Box<Expr>,
        args: Vec<Expr>,
    },
    New {
        ty: TypeExpr,
        fields: Vec<(Ident, Expr)>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeExpr {
    Named(Ident),
    BuiltIn(TypeIdentKind),
    Generic { base: Ident, args: Vec<TypeExpr> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParams {
    pub params: Vec<Ident>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}
