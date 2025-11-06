//! AST to HIR lowering implementation.
//!
//! This module translates parsed syntax trees into the typed high-level
//! intermediate representation. The MVP focuses on functions, basic
//! statements, and a trait implementation table so subsequent passes can
//! traverse a structured, typed view of the program.

use core::ast::TypeIdentKind;
use core::ast::nodes::{
    ArrayLiteral, Block, Expr, FieldDecl, FnDecl, FnSig, ImplBlock, InterfaceDecl, Item,
    MapLiteral, ObjDecl, Param, Stmt, TypeExpr, UseDecl,
};
use core::diag::Span;

use crate::semantics::hir::ids::{HirBlockId, HirExprId, HirItemId, HirStmtId, TraitImplId};
use crate::semantics::hir::{
    ArenaIndex, HirBlock, HirExpr, HirExprKind, HirField, HirFnSignature, HirFunction,
    HirInterface, HirItem, HirItemKind, HirLiteral, HirModule, HirObject, HirParam, HirStmt,
    HirStmtKind, HirTraitImpl, HirUse,
};
use crate::semantics::typeck::{SemanticDiagnostic, TypeChecker};

/// Result of lowering a module.
#[derive(Debug)]
pub struct LoweringResult {
    pub module: HirModule,
    pub diagnostics: Vec<SemanticDiagnostic>,
}

/// Context object orchestrating AST to HIR lowering.
#[derive(Debug)]
pub struct LoweringContext {
    module: HirModule,
    typeck: TypeChecker,
}

impl LoweringContext {
    /// Create a fresh lowering context.
    pub fn new() -> Self {
        Self {
            module: HirModule::default(),
            typeck: TypeChecker::new(),
        }
    }

    /// Lower the provided items into HIR.
    pub fn lower(items: &[Item], span: Option<Span>) -> LoweringResult {
        Self::new().lower_module(items, span)
    }

    fn lower_module(mut self, items: &[Item], span: Option<Span>) -> LoweringResult {
        for item in items {
            if let Some(id) = self.lower_item(item) {
                self.module.roots.push(id);
            }
        }

        self.module.span = span;

        let diagnostics = self.typeck.diagnostics().to_vec();
        let mut module = self.module;
        module.types = self.typeck.types.clone();

        LoweringResult {
            module,
            diagnostics,
        }
    }

    fn lower_item(&mut self, item: &Item) -> Option<HirItemId> {
        match item {
            Item::Fn(decl) => Some(self.lower_function(decl)),
            Item::Impl(block) => Some(self.lower_impl(block)),
            Item::Obj(decl) => Some(self.lower_object(decl)),
            Item::Interface(decl) => Some(self.lower_interface(decl)),
            Item::Use(decl) => Some(self.lower_use(decl)),
        }
    }

    fn lower_function(&mut self, decl: &FnDecl) -> HirItemId {
        let params = decl
            .params
            .iter()
            .map(|param| self.lower_param(param))
            .collect();
        let ret_type = decl
            .ret_type
            .as_ref()
            .and_then(|ty| self.typeck.resolve_annotation(ty))
            .unwrap_or(self.typeck.primitives.unit);
        let body = self.lower_block(&decl.body);

        self.alloc_item(HirItem {
            name: decl.name.name.clone(),
            symbol: None,
            span: decl.name.span.clone(),
            kind: HirItemKind::Function(HirFunction {
                params,
                ret_type,
                body,
                span: decl.name.span.clone(),
            }),
        })
    }

    fn lower_impl(&mut self, block: &ImplBlock) -> HirItemId {
        let target = type_expr_to_label(&block.target);
        let interface = block.interface.as_ref().map(type_expr_to_label);

        let mut method_ids = Vec::new();
        for method in &block.methods {
            let id = self.lower_function(method);
            method_ids.push(id);
        }

        let impl_id = self.alloc_trait_impl(HirTraitImpl {
            target: target.clone(),
            interface: interface.clone(),
            methods: method_ids,
            span: block.span.clone(),
        });

        self.alloc_item(HirItem {
            name: if let Some(interface) = &interface {
                format!("impl {} for {}", target, interface)
            } else {
                format!("impl {}", target)
            },
            symbol: None,
            span: block.span.clone(),
            kind: HirItemKind::TraitImpl { impl_id },
        })
    }

    fn lower_object(&mut self, decl: &ObjDecl) -> HirItemId {
        let fields = decl
            .fields
            .iter()
            .map(|field| self.lower_field(field))
            .collect();

        self.alloc_item(HirItem {
            name: decl.name.name.clone(),
            symbol: None,
            span: decl.name.span.clone(),
            kind: HirItemKind::Object(HirObject {
                fields,
                span: decl.name.span.clone(),
            }),
        })
    }

    fn lower_interface(&mut self, decl: &InterfaceDecl) -> HirItemId {
        let methods = decl
            .methods
            .iter()
            .map(|sig| self.lower_signature(sig))
            .collect();

        self.alloc_item(HirItem {
            name: decl.name.name.clone(),
            symbol: None,
            span: decl.name.span.clone(),
            kind: HirItemKind::Interface(HirInterface {
                methods,
                span: decl.name.span.clone(),
            }),
        })
    }

    fn lower_use(&mut self, decl: &UseDecl) -> HirItemId {
        let path: Vec<String> = decl.path.iter().map(|ident| ident.name.clone()).collect();
        let alias = decl.alias.as_ref().map(|ident| ident.name.clone());

        self.alloc_item(HirItem {
            name: path.join("::"),
            symbol: None,
            span: decl.alias.as_ref().and_then(|ident| ident.span.clone()),
            kind: HirItemKind::Use(HirUse {
                path,
                alias,
                span: decl.alias.as_ref().and_then(|ident| ident.span.clone()),
            }),
        })
    }

    fn lower_param(&mut self, param: &Param) -> HirParam {
        let ty = param
            .ty
            .as_ref()
            .and_then(|ty| self.typeck.resolve_annotation(ty))
            .unwrap_or(self.typeck.primitives.any);

        HirParam {
            name: param.name.name.clone(),
            symbol: None,
            ty,
            span: param.name.span.clone(),
        }
    }

    fn lower_field(&mut self, field: &FieldDecl) -> HirField {
        let ty = self.typeck.resolve_annotation(&field.ty);

        HirField {
            name: field.name.name.clone(),
            ty,
            span: field.name.span.clone(),
        }
    }

    fn lower_signature(&mut self, sig: &FnSig) -> HirFnSignature {
        let params = sig
            .params
            .iter()
            .map(|param| self.lower_param(param))
            .collect();
        let ret_type = sig
            .ret_type
            .as_ref()
            .and_then(|ty| self.typeck.resolve_annotation(ty));

        HirFnSignature {
            name: sig.name.name.clone(),
            params,
            ret_type,
            span: sig.span.clone(),
        }
    }

    fn lower_block(&mut self, block: &Block) -> HirBlockId {
        let mut stmt_ids = Vec::new();
        for stmt in &block.stmts {
            if let Some(stmt_id) = self.lower_statement(stmt) {
                stmt_ids.push(stmt_id);
            }
        }
        let ty = self.typeck.check_block(block);

        self.alloc_block(HirBlock {
            stmts: stmt_ids,
            ty,
            span: None,
        })
    }

    fn lower_statement(&mut self, stmt: &Stmt) -> Option<HirStmtId> {
        let stmt_ty = self.typeck.infer_statement(stmt);

        let kind = match stmt {
            Stmt::Let(decl) => {
                let value = decl.value.as_ref().map(|expr| self.lower_expr(expr));
                HirStmtKind::Let {
                    name: decl.name.name.clone(),
                    value,
                }
            }
            Stmt::Const(decl) => {
                let value = self.lower_expr(&decl.value);
                HirStmtKind::Const {
                    name: decl.name.name.clone(),
                    value,
                }
            }
            Stmt::Var(decl) => {
                let value = decl.value.as_ref().map(|expr| self.lower_expr(expr));
                HirStmtKind::Var {
                    name: decl.name.name.clone(),
                    value,
                }
            }
            Stmt::Expr(expr) => HirStmtKind::Expr(self.lower_expr(expr)),
            Stmt::Return(expr) => HirStmtKind::Return {
                value: expr.as_ref().map(|value| self.lower_expr(value)),
            },
            Stmt::Assign { target, value } => HirStmtKind::Assign {
                target: self.lower_expr(target),
                value: self.lower_expr(value),
            },
            Stmt::Block(block) => HirStmtKind::Block(self.lower_block(block)),
            other => HirStmtKind::Unsupported {
                reason: format!("unsupported statement: {:?}", other),
            },
        };

        Some(self.alloc_stmt(HirStmt {
            kind,
            ty: stmt_ty,
            span: statement_span(stmt),
        }))
    }

    fn lower_expr(&mut self, expr: &Expr) -> HirExprId {
        let ty = self
            .typeck
            .infer_expr(expr)
            .unwrap_or(self.typeck.primitives.any);
        let span = expression_span(expr);

        let kind = match expr {
            Expr::IntLit { value, .. } => HirExprKind::Literal(HirLiteral::Int(*value)),
            Expr::FloatLit { value, .. } => HirExprKind::Literal(HirLiteral::Float(*value)),
            Expr::StringLit { value, .. } => {
                HirExprKind::Literal(HirLiteral::String(value.clone()))
            }
            Expr::Unary { op, expr } => HirExprKind::Unary {
                op: *op,
                operand: self.lower_expr(expr),
            },
            Expr::Binary { op, left, right } => HirExprKind::Binary {
                op: *op,
                left: self.lower_expr(left),
                right: self.lower_expr(right),
            },
            Expr::Tuple(elements) => {
                HirExprKind::Tuple(elements.iter().map(|expr| self.lower_expr(expr)).collect())
            }
            Expr::Array(ArrayLiteral { elements, .. }) => {
                HirExprKind::Array(elements.iter().map(|expr| self.lower_expr(expr)).collect())
            }
            Expr::Map(MapLiteral { entries, .. }) => HirExprKind::Map(
                entries
                    .iter()
                    .map(|entry| (self.lower_expr(&entry.key), self.lower_expr(&entry.value)))
                    .collect(),
            ),
            Expr::Ident(ident) => HirExprKind::Identifier {
                name: ident.name.clone(),
                symbol: None,
            },
            Expr::Call { callee, args } => HirExprKind::Call {
                callee: self.lower_expr(callee),
                args: args.iter().map(|arg| self.lower_expr(arg)).collect(),
            },
            other => HirExprKind::Unsupported {
                reason: format!("unsupported expression: {:?}", other),
            },
        };

        self.alloc_expr(HirExpr { ty, span, kind })
    }

    fn alloc_item(&mut self, item: HirItem) -> HirItemId {
        to_item_id(self.module.items.alloc(item))
    }

    fn alloc_stmt(&mut self, stmt: HirStmt) -> HirStmtId {
        to_stmt_id(self.module.stmts.alloc(stmt))
    }

    fn alloc_expr(&mut self, expr: HirExpr) -> HirExprId {
        to_expr_id(self.module.exprs.alloc(expr))
    }

    fn alloc_block(&mut self, block: HirBlock) -> HirBlockId {
        to_block_id(self.module.blocks.alloc(block))
    }

    fn alloc_trait_impl(&mut self, entry: HirTraitImpl) -> TraitImplId {
        to_trait_impl_id(self.module.trait_impls.alloc(entry))
    }
}

fn to_item_id(index: ArenaIndex<HirItem>) -> HirItemId {
    HirItemId::from_raw(index.to_raw() as u32)
}

fn to_stmt_id(index: ArenaIndex<HirStmt>) -> HirStmtId {
    HirStmtId::from_raw(index.to_raw() as u32)
}

fn to_expr_id(index: ArenaIndex<HirExpr>) -> HirExprId {
    HirExprId::from_raw(index.to_raw() as u32)
}

fn to_block_id(index: ArenaIndex<HirBlock>) -> HirBlockId {
    HirBlockId::from_raw(index.to_raw() as u32)
}

fn to_trait_impl_id(index: ArenaIndex<HirTraitImpl>) -> TraitImplId {
    TraitImplId::from_raw(index.to_raw() as u32)
}

fn statement_span(stmt: &Stmt) -> Option<Span> {
    match stmt {
        Stmt::Let(decl) => decl.span.clone(),
        Stmt::Const(decl) => decl.span.clone(),
        Stmt::Var(decl) => decl.span.clone(),
        _ => None,
    }
}

fn expression_span(expr: &Expr) -> Option<Span> {
    match expr {
        Expr::Ident(ident) => ident.span.clone(),
        Expr::Array(array) => array.span.clone(),
        Expr::Map(map) => map.span.clone(),
        _ => None,
    }
}

fn type_expr_to_label(expr: &TypeExpr) -> String {
    match expr {
        TypeExpr::Named(ident) => ident.name.clone(),
        TypeExpr::BuiltIn(kind) => type_ident_to_string(*kind),
        TypeExpr::Generic { base, args } => {
            let args = args
                .iter()
                .map(type_expr_to_label)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{}>", base.name, args)
        }
        TypeExpr::Pointer(inner) => format!("*{}", type_expr_to_label(inner)),
        TypeExpr::Tuple(elements) => {
            let parts = elements
                .iter()
                .map(type_expr_to_label)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", parts)
        }
        TypeExpr::Array(array) => {
            if let Some(element) = &array.element {
                format!("[{}]", type_expr_to_label(element))
            } else {
                "[]".to_string()
            }
        }
        TypeExpr::Map(map) => {
            let key = map
                .key
                .as_ref()
                .map(|k| type_expr_to_label(k))
                .unwrap_or_else(|| "?".to_string());
            let value = map
                .value
                .as_ref()
                .map(|v| type_expr_to_label(v))
                .unwrap_or_else(|| "?".to_string());
            format!("{{{} -> {}}}", key, value)
        }
        TypeExpr::Inferred => "_".to_string(),
    }
}

#[allow(unreachable_patterns)]
fn type_ident_to_string(kind: TypeIdentKind) -> String {
    match kind {
        TypeIdentKind::Int => "int".to_string(),
        TypeIdentKind::UInt => "uint".to_string(),
        TypeIdentKind::Float => "float".to_string(),
        TypeIdentKind::String => "string".to_string(),
        TypeIdentKind::Bool => "bool".to_string(),
        TypeIdentKind::I8 => "i8".to_string(),
        TypeIdentKind::I16 => "i16".to_string(),
        TypeIdentKind::I32 => "i32".to_string(),
        TypeIdentKind::I64 => "i64".to_string(),
        TypeIdentKind::I128 => "i128".to_string(),
        TypeIdentKind::U8 => "u8".to_string(),
        TypeIdentKind::U16 => "u16".to_string(),
        TypeIdentKind::U32 => "u32".to_string(),
        TypeIdentKind::U64 => "u64".to_string(),
        TypeIdentKind::U128 => "u128".to_string(),
        TypeIdentKind::F32 => "f32".to_string(),
        TypeIdentKind::F64 => "f64".to_string(),
        TypeIdentKind::Char => "char".to_string(),
        other => format!("{:?}", other),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantics::hir::{ArenaIndex, HirItemKind, HirStmtKind};
    use core::ast::nodes::{
        Block, FnDecl, GenericParams, Ident, ImplBlock, Item, Param, Stmt, TypeExpr,
    };
    use core::ast::{BinaryOp, Expr, TypeIdentKind};
    use core::diag::Span;

    fn ident(name: &str) -> Ident {
        Ident {
            name: name.to_string(),
            span: Some(Span { start: 0, end: 0 }),
        }
    }

    #[test]
    fn lowers_simple_function() {
        let fn_decl = FnDecl {
            vis: core::ast::nodes::Visibility::Public,
            name: ident("add"),
            generics: GenericParams { params: Vec::new() },
            params: vec![
                Param {
                    name: ident("lhs"),
                    ty: Some(TypeExpr::BuiltIn(TypeIdentKind::Int)),
                },
                Param {
                    name: ident("rhs"),
                    ty: Some(TypeExpr::BuiltIn(TypeIdentKind::Int)),
                },
            ],
            ret_type: Some(TypeExpr::BuiltIn(TypeIdentKind::Int)),
            body: Block {
                stmts: vec![Stmt::Return(Some(Expr::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Ident(ident("lhs"))),
                    right: Box::new(Expr::Ident(ident("rhs"))),
                }))],
            },
        };

        let LoweringResult {
            module,
            diagnostics,
        } = LoweringContext::lower(&[Item::Fn(fn_decl)], None);

        assert!(diagnostics.is_empty());
        assert_eq!(module.roots.len(), 1);

        let func_id = module.roots[0];
        let func = module
            .items
            .get(ArenaIndex::from_raw(func_id.to_raw() as usize))
            .expect("function present");

        match &func.kind {
            HirItemKind::Function(func_data) => {
                assert_eq!(func.name, "add");
                assert_eq!(func_data.params.len(), 2);

                let block = module
                    .blocks
                    .get(ArenaIndex::from_raw(func_data.body.to_raw() as usize))
                    .expect("block present");
                assert_eq!(block.stmts.len(), 1);

                let stmt = module
                    .stmts
                    .get(ArenaIndex::from_raw(block.stmts[0].to_raw() as usize))
                    .expect("stmt present");
                assert!(matches!(stmt.kind, HirStmtKind::Return { .. }));
            }
            other => panic!("expected function item, found {other:?}"),
        }
    }

    #[test]
    fn lowers_trait_impl_with_method() {
        let method = FnDecl {
            vis: core::ast::nodes::Visibility::Public,
            name: ident("next"),
            generics: GenericParams { params: Vec::new() },
            params: Vec::new(),
            ret_type: Some(TypeExpr::BuiltIn(TypeIdentKind::Int)),
            body: Block {
                stmts: vec![Stmt::Return(Some(Expr::IntLit { value: 1, ty: None }))],
            },
        };

        let impl_block = ImplBlock::new(
            core::ast::nodes::Visibility::Public,
            GenericParams { params: Vec::new() },
            TypeExpr::Named(ident("Vector")),
            Some(TypeExpr::Named(ident("Iterable"))),
            vec![method],
            None,
        );

        let LoweringResult {
            module,
            diagnostics,
        } = LoweringContext::lower(&[Item::Impl(impl_block)], None);

        assert!(diagnostics.is_empty());
        assert_eq!(module.roots.len(), 1);

        let impl_item = module
            .items
            .get(ArenaIndex::from_raw(module.roots[0].to_raw() as usize))
            .expect("impl item present");

        match &impl_item.kind {
            HirItemKind::TraitImpl { impl_id } => {
                let entry = module
                    .trait_impls
                    .get(ArenaIndex::from_raw(impl_id.to_raw() as usize))
                    .expect("trait impl entry");
                assert_eq!(entry.methods.len(), 1);
            }
            other => panic!("expected trait impl, found {other:?}"),
        }
    }
}
