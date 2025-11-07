//! Type checking scaffolding.
//!
//! The type checking subsystem will eventually handle inference, constraint
//! solving, and diagnostic reporting. This initial module establishes the
//! interfaces consumed by other phases.

pub mod constraints;
pub mod diagnostics;

pub use constraints::{ConstraintSet, TypeConstraint};
pub use diagnostics::{SemanticDiagnostic, SemanticErrorCode};

use core::ast::nodes::{Block, ConstDecl, Expr, LetDecl, Stmt, TypeExpr, VarDecl};
use core::ast::{BinaryOp, TypeIdentKind, UnaryOp};
use core::diag::Span;

use crate::semantics::hir::{PrimitiveTypes, TypeContext, TypeCtxId, TypeInfo};
use std::collections::HashSet;

const EMPTY_SPAN: Span = Span { start: 0, end: 0 };

/// Entry point for FluxLang type checking.
#[derive(Debug)]
pub struct TypeChecker {
    pub types: TypeContext,
    pub primitives: PrimitiveTypes,
    diagnostics: Vec<SemanticDiagnostic>,
    seen_diagnostics: HashSet<DiagnosticKey>,
}

impl TypeChecker {
    /// Create a type checker with a fresh type context populated with primitives.
    pub fn new() -> Self {
        let (types, primitives) = TypeContext::with_primitives();
        Self {
            types,
            primitives,
            diagnostics: Vec::new(),
            seen_diagnostics: HashSet::new(),
        }
    }

    /// Type-check a block and return the type of the final expression.
    pub fn check_block(&mut self, block: &Block) -> TypeCtxId {
        let mut last = self.primitives.unit;
        for stmt in &block.stmts {
            if let Some(ty) = self.check_statement(stmt) {
                last = ty;
            }
        }
        last
    }

    /// Retrieve diagnostics produced during the last analysis run.
    pub fn diagnostics(&self) -> &[SemanticDiagnostic] {
        &self.diagnostics
    }

    /// Public helper for other phases to reuse expression inference.
    pub(crate) fn infer_expr(&mut self, expr: &Expr) -> Option<TypeCtxId> {
        self.check_expr(expr)
    }

    /// Public helper for other phases to reuse statement checking.
    pub(crate) fn infer_statement(&mut self, stmt: &Stmt) -> Option<TypeCtxId> {
        self.check_statement(stmt)
    }

    /// Public helper for other phases to reuse type annotation resolution.
    pub(crate) fn resolve_annotation(&mut self, ty: &TypeExpr) -> Option<TypeCtxId> {
        self.type_from_annotation(ty)
    }

    fn check_statement(&mut self, stmt: &Stmt) -> Option<TypeCtxId> {
        match stmt {
            Stmt::Const(decl) => self.check_const_decl(decl),
            Stmt::Let(decl) => self.check_let_decl(decl),
            Stmt::Var(decl) => self.check_var_decl(decl),
            Stmt::Expr(expr) => self.check_expr(expr),
            Stmt::Assign { target, value } => {
                let target_ty = self.check_expr(target);
                let value_ty = self.check_expr(value);
                if let (Some(target), Some(value)) = (target_ty, value_ty) {
                    self.ensure_assignable(target, value, None);
                    Some(target)
                } else {
                    None
                }
            }
            Stmt::If(if_stmt) => {
                if let Some(cond_ty) = self.check_expr(&if_stmt.cond) {
                    self.ensure_boolean(cond_ty, None);
                }
                let _ = self.check_block(&if_stmt.then_branch);
                if let Some(else_branch) = &if_stmt.else_branch {
                    let _ = self.check_statement(else_branch);
                }
                Some(self.primitives.unit)
            }
            Stmt::Loop(block) => {
                let _ = self.check_block(block);
                Some(self.primitives.unit)
            }
            Stmt::While { cond, body } => {
                if let Some(cond_ty) = self.check_expr(cond) {
                    self.ensure_boolean(cond_ty, None);
                }
                let _ = self.check_block(body);
                Some(self.primitives.unit)
            }
            Stmt::Break | Stmt::Continue => Some(self.primitives.unit),
            Stmt::Return(expr) => expr
                .as_ref()
                .and_then(|value| self.check_expr(value))
                .or(Some(self.primitives.unit)),
            Stmt::Block(inner) => Some(self.check_block(inner)),
            _ => Some(self.primitives.unit),
        }
    }

    fn check_const_decl(&mut self, decl: &ConstDecl) -> Option<TypeCtxId> {
        let value_ty = self.check_expr(&decl.value);
        match (decl.ty.as_ref(), value_ty) {
            (Some(annotation), Some(actual)) => {
                let annotated = self.type_from_annotation(annotation);
                if let Some(annotated) = annotated {
                    self.ensure_assignable(annotated, actual, decl.name.span.clone());
                    Some(annotated)
                } else {
                    Some(actual)
                }
            }
            (Some(annotation), None) => self.type_from_annotation(annotation),
            (None, Some(actual)) => Some(actual),
            (None, None) => {
                self.emit_missing_type(&decl.name.span);
                None
            }
        }
    }

    fn check_let_decl(&mut self, decl: &LetDecl) -> Option<TypeCtxId> {
        let init_ty = decl.value.as_ref().and_then(|expr| self.check_expr(expr));
        match (&decl.ty, init_ty) {
            (Some(annotation), Some(actual)) => {
                let annotated = self.type_from_annotation(annotation);
                if let Some(annotated) = annotated {
                    self.ensure_assignable(annotated, actual, decl.name.span.clone());
                    Some(annotated)
                } else {
                    Some(actual)
                }
            }
            (Some(annotation), None) => self.type_from_annotation(annotation),
            (None, Some(actual)) => Some(actual),
            (None, None) => {
                self.emit_missing_type(&decl.name.span);
                None
            }
        }
    }

    fn check_var_decl(&mut self, decl: &VarDecl) -> Option<TypeCtxId> {
        let annotation_ty = self.type_from_annotation(&decl.ty);
        if annotation_ty.is_none() {
            self.emit_unknown_type(&decl.name.span);
        }
        let annotation_ty = annotation_ty;
        let value_ty = decl.value.as_ref().and_then(|expr| self.check_expr(expr));
        match (annotation_ty, value_ty) {
            (Some(annotation), Some(actual)) => {
                self.ensure_assignable(annotation, actual, decl.name.span.clone());
                Some(annotation)
            }
            (Some(annotation), None) => Some(annotation),
            (None, Some(actual)) => Some(actual),
            (None, None) => None,
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Option<TypeCtxId> {
        match expr {
            Expr::IntLit { .. } => Some(self.primitives.int),
            Expr::FloatLit { .. } => Some(self.primitives.float),
            Expr::StringLit { .. } => Some(self.primitives.string),
            Expr::Unary { op, expr: inner } => {
                let inner_ty = self.check_expr(inner)?;
                Some(self.check_unary(*op, inner_ty, expr))
            }
            Expr::Binary { op, left, right } => {
                let left_ty = self.check_expr(left)?;
                let right_ty = self.check_expr(right)?;
                Some(self.unify_binary(*op, left_ty, right_ty))
            }
            Expr::Tuple(elements) => {
                let element_types: Vec<_> = elements
                    .iter()
                    .map(|element| self.check_expr(element).unwrap_or(self.primitives.any))
                    .collect();
                let label = element_types
                    .iter()
                    .map(|ty| self.type_label(*ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                Some(self.types.intern(TypeInfo::nominal(format!("({})", label))))
            }
            Expr::Array(array) => {
                let mut element_ty = None;
                for element in &array.elements {
                    let ty = self.check_expr(element).unwrap_or(self.primitives.any);
                    element_ty = Some(match element_ty {
                        None => ty,
                        Some(existing) => self.unify_numeric(existing, ty),
                    });
                }
                Some(element_ty.unwrap_or(self.primitives.any))
            }
            Expr::Map(map) => {
                let mut key_ty = None;
                let mut value_ty = None;
                for entry in &map.entries {
                    let k = self.check_expr(&entry.key).unwrap_or(self.primitives.any);
                    let v = self.check_expr(&entry.value).unwrap_or(self.primitives.any);
                    key_ty = Some(match key_ty {
                        None => k,
                        Some(existing) => self.unify_numeric(existing, k),
                    });
                    value_ty = Some(match value_ty {
                        None => v,
                        Some(existing) => self.unify_numeric(existing, v),
                    });
                }
                let label = format!(
                    "Map<{}, {}>",
                    self.type_label(key_ty.unwrap_or(self.primitives.any)),
                    self.type_label(value_ty.unwrap_or(self.primitives.any))
                );
                Some(self.types.intern(TypeInfo::nominal(label)))
            }
            _ => Some(self.primitives.any),
        }
    }

    fn check_unary(&mut self, _op: UnaryOp, operand: TypeCtxId, _expr: &Expr) -> TypeCtxId {
        operand
    }

    fn unify_binary(&mut self, op: BinaryOp, left: TypeCtxId, right: TypeCtxId) -> TypeCtxId {
        match op {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Mod
            | BinaryOp::Shl
            | BinaryOp::Shr => self.unify_numeric(left, right),
            BinaryOp::Eq
            | BinaryOp::NotEq
            | BinaryOp::Lt
            | BinaryOp::Lte
            | BinaryOp::Gt
            | BinaryOp::Gte
            | BinaryOp::And
            | BinaryOp::Or => self.primitives.bool,
            BinaryOp::Assign
            | BinaryOp::AddAssign
            | BinaryOp::SubAssign
            | BinaryOp::MulAssign
            | BinaryOp::DivAssign
            | BinaryOp::ModAssign => {
                self.ensure_assignable(left, right, None);
                left
            }
            _ => self.primitives.any,
        }
    }

    fn unify_numeric(&mut self, left: TypeCtxId, right: TypeCtxId) -> TypeCtxId {
        if left == self.primitives.float || right == self.primitives.float {
            self.primitives.float
        } else if left == self.primitives.int && right == self.primitives.int {
            self.primitives.int
        } else {
            self.primitives.any
        }
    }

    fn ensure_assignable(&mut self, target: TypeCtxId, value: TypeCtxId, span: Option<Span>) {
        if target != value && target != self.primitives.any {
            self.push_diagnostic(
                format!(
                    "cannot assign `{}` to `{}`",
                    self.type_label(value),
                    self.type_label(target)
                ),
                span.unwrap_or(EMPTY_SPAN),
                SemanticErrorCode::TypeMismatch,
            );
        }
    }

    fn ensure_boolean(&mut self, ty: TypeCtxId, span: Option<Span>) {
        if ty != self.primitives.bool && ty != self.primitives.any {
            self.emit_expected_boolean(span);
        }
    }

    fn type_from_annotation(&mut self, ty: &TypeExpr) -> Option<TypeCtxId> {
        match ty {
            TypeExpr::BuiltIn(kind) => Some(match kind {
                TypeIdentKind::Int => self.primitives.int,
                TypeIdentKind::UInt => self.primitives.int,
                TypeIdentKind::Float => self.primitives.float,
                TypeIdentKind::String => self.primitives.string,
                TypeIdentKind::Bool => self.primitives.bool,
                _ => self.primitives.any,
            }),
            TypeExpr::Named(ident) => Some(self.types.intern(TypeInfo::nominal(&ident.name))),
            TypeExpr::Inferred => Some(self.primitives.any),
            _ => None,
        }
    }

    fn type_label(&self, id: TypeCtxId) -> String {
        match self.types.get(id) {
            Some(TypeInfo::Any) => "any".to_string(),
            Some(TypeInfo::Concrete(ty)) => ty.name.clone(),
            None => "unknown".to_string(),
        }
    }

    fn emit_missing_type(&mut self, span: &Option<Span>) {
        self.push_diagnostic(
            "missing type annotation and no initializer".to_string(),
            span.clone().unwrap_or(EMPTY_SPAN),
            SemanticErrorCode::MissingTypeAnnotation,
        );
    }

    fn emit_unknown_type(&mut self, span: &Option<Span>) {
        self.push_diagnostic(
            "unknown type annotation".to_string(),
            span.clone().unwrap_or(EMPTY_SPAN),
            SemanticErrorCode::UnknownType,
        );
    }

    fn emit_expected_boolean(&mut self, span: Option<Span>) {
        self.push_diagnostic(
            "condition must evaluate to a boolean".to_string(),
            span.unwrap_or(EMPTY_SPAN),
            SemanticErrorCode::ExpectedBoolean,
        );
    }

    fn push_diagnostic(&mut self, message: String, span: Span, code: SemanticErrorCode) {
        let key = DiagnosticKey {
            code,
            span: span.clone(),
            message: message.clone(),
        };
        if self.seen_diagnostics.insert(key) {
            self.diagnostics
                .push(SemanticDiagnostic::new(message, span, code));
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct DiagnosticKey {
    code: SemanticErrorCode,
    span: Span,
    message: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::ast::nodes::TypeExpr;
    use core::ast::nodes::{Block, Expr, Ident, IfStmt, LetDecl, Stmt, VarDecl};
    use core::ast::{BinaryOp, TypeIdentKind};

    fn empty_span() -> Option<Span> {
        Some(Span { start: 0, end: 0 })
    }

    #[test]
    fn let_without_initializer_requires_type() {
        let mut checker = TypeChecker::new();
        let stmt = Stmt::Let(LetDecl {
            name: Ident {
                name: "value".into(),
                span: empty_span(),
            },
            ty: None,
            value: None,
            span: None,
        });
        checker.check_statement(&stmt);
        assert_eq!(checker.diagnostics().len(), 1);
        assert_eq!(
            checker.diagnostics()[0].code,
            SemanticErrorCode::MissingTypeAnnotation
        );
    }

    #[test]
    fn numeric_promotion_prefers_float() {
        let mut checker = TypeChecker::new();
        let expr = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::IntLit { value: 1, ty: None }),
            right: Box::new(Expr::FloatLit {
                value: 2.0,
                ty: None,
            }),
        };
        let ty = checker.check_expr(&expr).unwrap();
        assert_eq!(ty, checker.primitives.float);
        assert!(checker.diagnostics().is_empty());
    }

    #[test]
    fn mismatched_assignment_reports_diagnostic() {
        let mut checker = TypeChecker::new();
        let stmt = Stmt::Var(VarDecl {
            name: Ident {
                name: "value".into(),
                span: empty_span(),
            },
            ty: TypeExpr::BuiltIn(TypeIdentKind::Int),
            value: Some(Expr::FloatLit {
                value: 3.14,
                ty: None,
            }),
            span: None,
        });
        checker.check_statement(&stmt);
        assert_eq!(checker.diagnostics().len(), 1);
        assert_eq!(
            checker.diagnostics()[0].code,
            SemanticErrorCode::TypeMismatch
        );
    }

    #[test]
    fn if_condition_requires_boolean() {
        let mut checker = TypeChecker::new();
        let stmt = Stmt::If(Box::new(IfStmt {
            cond: Expr::IntLit { value: 1, ty: None },
            then_branch: Block { stmts: Vec::new() },
            else_branch: None,
        }));

        checker.check_statement(&stmt);

        assert!(
            checker
                .diagnostics()
                .iter()
                .any(|diag| diag.code == SemanticErrorCode::ExpectedBoolean)
        );
    }

    #[test]
    fn while_condition_accepts_boolean() {
        let mut checker = TypeChecker::new();
        let stmt = Stmt::While {
            cond: Expr::Binary {
                op: BinaryOp::Eq,
                left: Box::new(Expr::IntLit { value: 1, ty: None }),
                right: Box::new(Expr::IntLit { value: 1, ty: None }),
            },
            body: Block { stmts: Vec::new() },
        };

        checker.check_statement(&stmt);

        assert!(
            checker
                .diagnostics()
                .iter()
                .all(|diag| diag.code != SemanticErrorCode::ExpectedBoolean)
        );
    }
}
