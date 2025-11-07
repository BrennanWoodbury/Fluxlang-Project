//! High-level HIR node definitions.
//!
//! These structures represent the typed high-level intermediate
//! representation produced by the semantic passes. The layout keeps the
//! representation close to the source AST while attaching stable identifiers,
//! resolved types, and spans for diagnostics.

use core::ast::{BinaryOp, UnaryOp};
use core::diag::Span;
use std::cell::Ref;

use super::arena::{Arena, ArenaIndex};
use super::ids::{HirBlockId, HirExprId, HirItemId, HirStmtId, SymbolId, TraitImplId, TypeCtxId};
use super::types::TypeContext;

/// Container for all HIR nodes associated with a single module.
#[derive(Debug)]
pub struct HirModule {
    /// Interned type table used by the module.
    pub types: TypeContext,
    /// Arena storing item nodes.
    pub items: Arena<HirItem>,
    /// Arena storing statement nodes.
    pub stmts: Arena<HirStmt>,
    /// Arena storing expression nodes.
    pub exprs: Arena<HirExpr>,
    /// Arena storing block nodes.
    pub blocks: Arena<HirBlock>,
    /// Arena storing trait implementation descriptors.
    pub trait_impls: Arena<HirTraitImpl>,
    /// Root item identifiers making up the module.
    pub roots: Vec<HirItemId>,
    /// Optional span covering the entire module.
    pub span: Option<Span>,
}

impl Default for HirModule {
    fn default() -> Self {
        Self {
            types: TypeContext::new(),
            items: Arena::new(),
            stmts: Arena::new(),
            exprs: Arena::new(),
            blocks: Arena::new(),
            trait_impls: Arena::new(),
            roots: Vec::new(),
            span: None,
        }
    }
}

/// High-level item node.
#[derive(Debug)]
pub struct HirItem {
    pub name: String,
    pub symbol: Option<SymbolId>,
    pub span: Option<Span>,
    pub kind: HirItemKind,
}

/// Variants describing the different item kinds.
#[derive(Debug)]
pub enum HirItemKind {
    Function(HirFunction),
    Const(HirConst),
    Object(HirObject),
    Interface(HirInterface),
    Use(HirUse),
    TraitImpl { impl_id: TraitImplId },
    Unsupported { reason: String },
}

/// Function declaration lowered into HIR.
#[derive(Debug)]
pub struct HirFunction {
    pub params: Vec<HirParam>,
    pub ret_type: TypeCtxId,
    pub body: HirBlockId,
    pub span: Option<Span>,
}

/// Function parameter metadata.
#[derive(Debug)]
pub struct HirParam {
    pub name: String,
    pub symbol: Option<SymbolId>,
    pub ty: TypeCtxId,
    pub span: Option<Span>,
}

/// Constant item metadata.
#[derive(Debug)]
pub struct HirConst {
    pub value: HirExprId,
    pub ty: TypeCtxId,
    pub span: Option<Span>,
}

/// Object declaration metadata.
#[derive(Debug)]
pub struct HirObject {
    pub fields: Vec<HirField>,
    pub span: Option<Span>,
}

/// Object field description.
#[derive(Debug)]
pub struct HirField {
    pub name: String,
    pub ty: Option<TypeCtxId>,
    pub span: Option<Span>,
}

/// Interface declaration metadata.
#[derive(Debug)]
pub struct HirInterface {
    pub methods: Vec<HirFnSignature>,
    pub span: Option<Span>,
}

/// Function signature captured for interfaces.
#[derive(Debug)]
pub struct HirFnSignature {
    pub name: String,
    pub params: Vec<HirParam>,
    pub ret_type: Option<TypeCtxId>,
    pub span: Option<Span>,
}

/// `use` declaration metadata.
#[derive(Debug)]
pub struct HirUse {
    pub path: Vec<String>,
    pub alias: Option<String>,
    pub span: Option<Span>,
}

/// Trait implementation record stored in the module table.
#[derive(Debug)]
pub struct HirTraitImpl {
    pub target: String,
    pub interface: Option<String>,
    pub methods: Vec<HirItemId>,
    pub span: Option<Span>,
}

/// Block expression containing statements and a resulting type.
#[derive(Debug)]
pub struct HirBlock {
    pub stmts: Vec<HirStmtId>,
    pub ty: TypeCtxId,
    pub span: Option<Span>,
}

/// Statement node with optional resulting type.
#[derive(Debug)]
pub struct HirStmt {
    pub kind: HirStmtKind,
    pub ty: Option<TypeCtxId>,
    pub span: Option<Span>,
}

/// Statement variants used throughout lowering.
#[derive(Debug)]
pub enum HirStmtKind {
    Let {
        name: String,
        value: Option<HirExprId>,
    },
    Const {
        name: String,
        value: HirExprId,
    },
    Var {
        name: String,
        value: Option<HirExprId>,
    },
    Expr(HirExprId),
    Return {
        value: Option<HirExprId>,
    },
    Assign {
        target: HirExprId,
        value: HirExprId,
    },
    Block(HirBlockId),
    If {
        cond: HirExprId,
        then_block: HirBlockId,
        else_block: Option<HirBlockId>,
    },
    While {
        cond: HirExprId,
        body: HirBlockId,
    },
    Loop {
        body: HirBlockId,
    },
    Break,
    Continue,
    Unsupported {
        reason: String,
    },
}

/// Expression node with its inferred type.
#[derive(Debug)]
pub struct HirExpr {
    pub ty: TypeCtxId,
    pub span: Option<Span>,
    pub kind: HirExprKind,
    pub const_value: Option<HirLiteral>,
}

/// Expression variants supported by the lowering MVP.
#[derive(Debug)]
pub enum HirExprKind {
    Literal(HirLiteral),
    Identifier {
        name: String,
        symbol: Option<SymbolId>,
    },
    Unary {
        op: UnaryOp,
        operand: HirExprId,
    },
    Binary {
        op: BinaryOp,
        left: HirExprId,
        right: HirExprId,
    },
    Tuple(Vec<HirExprId>),
    Array(Vec<HirExprId>),
    Map(Vec<(HirExprId, HirExprId)>),
    Call {
        callee: HirExprId,
        args: Vec<HirExprId>,
    },
    Block(HirBlockId),
    Unsupported {
        reason: String,
    },
}

/// Literal variants lowered into HIR.
#[derive(Debug, Clone, PartialEq)]
pub enum HirLiteral {
    Int(i128),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,
}

impl HirModule {
    /// Return the slice of root item identifiers.
    pub fn root_items(&self) -> &[HirItemId] {
        &self.roots
    }

    /// Fetch an item by identifier.
    pub fn item(&self, id: HirItemId) -> Option<Ref<'_, HirItem>> {
        self.items.get(ArenaIndex::from_raw(id.to_raw() as usize))
    }

    /// Fetch a function item by identifier.
    pub fn function(&self, id: HirItemId) -> Option<Ref<'_, HirFunction>> {
        let item_ref = self.item(id)?;
        if matches!(item_ref.kind, HirItemKind::Function(_)) {
            Some(Ref::map(item_ref, |item| match &item.kind {
                HirItemKind::Function(func) => func,
                _ => unreachable!(),
            }))
        } else {
            None
        }
    }

    /// Fetch a block by identifier.
    pub fn block(&self, id: HirBlockId) -> Option<Ref<'_, HirBlock>> {
        self.blocks.get(ArenaIndex::from_raw(id.to_raw() as usize))
    }

    /// Fetch a statement by identifier.
    pub fn stmt(&self, id: HirStmtId) -> Option<Ref<'_, HirStmt>> {
        self.stmts.get(ArenaIndex::from_raw(id.to_raw() as usize))
    }

    /// Fetch an expression by identifier.
    pub fn expr(&self, id: HirExprId) -> Option<Ref<'_, HirExpr>> {
        self.exprs.get(ArenaIndex::from_raw(id.to_raw() as usize))
    }
}
