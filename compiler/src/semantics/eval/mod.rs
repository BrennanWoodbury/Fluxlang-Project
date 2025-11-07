//! Constant folding and simple evaluation.
//!
//! This pass walks the HIR and records constant values for expressions that
//! can be evaluated at compile time. The resulting annotations allow later
//! phases to reuse folded literals without re-evaluating the original
//! expression structure.

use std::collections::HashMap;

use core::ast::{BinaryOp, UnaryOp};

use crate::semantics::hir::{
    ArenaIndex, HirExprId, HirExprKind, HirLiteral, HirModule, PrimitiveTypes,
};

/// Constant folding driver.
#[derive(Debug, Default)]
pub struct ConstantFolder;

impl ConstantFolder {
    /// Create a new constant folder.
    pub fn new() -> Self {
        Self
    }

    /// Run the constant folding pass over a module. Returns the number of
    /// expressions that gained a constant annotation during this run.
    pub fn fold_module(&self, module: &mut HirModule) -> usize {
        let primitives = module.types.primitives();
        let mut cache: HashMap<HirExprId, Option<HirLiteral>> = HashMap::new();
        let mut annotated = 0usize;

        let expr_count = module.exprs.len();
        for raw in 0..expr_count {
            let expr_id = HirExprId::from_raw(raw as u32);

            let had_annotation = module
                .exprs
                .get(ArenaIndex::from_raw(raw))
                .map(|expr| expr.const_value.is_some())
                .unwrap_or(false);

            self.fold_expr(module, expr_id, &primitives, &mut cache);

            let has_annotation = module
                .exprs
                .get(ArenaIndex::from_raw(raw))
                .map(|expr| expr.const_value.is_some())
                .unwrap_or(false);

            if has_annotation && !had_annotation {
                annotated += 1;
            }
        }

        annotated
    }

    fn fold_expr(
        &self,
        module: &mut HirModule,
        id: HirExprId,
        primitives: &PrimitiveTypes,
        cache: &mut HashMap<HirExprId, Option<HirLiteral>>,
    ) -> Option<HirLiteral> {
        if let Some(result) = cache.get(&id) {
            return result.clone();
        }

        enum ExprCase {
            Literal(HirLiteral),
            Unary(UnaryOp, HirExprId, crate::semantics::hir::TypeCtxId),
            Binary(
                BinaryOp,
                HirExprId,
                HirExprId,
                crate::semantics::hir::TypeCtxId,
            ),
            Other,
        }

        let case = {
            let expr_ref = module
                .exprs
                .get(ArenaIndex::from_raw(id.to_raw() as usize))
                .expect("expression id must exist");

            match &expr_ref.kind {
                HirExprKind::Literal(lit) => ExprCase::Literal(lit.clone()),
                HirExprKind::Unary { op, operand } => ExprCase::Unary(*op, *operand, expr_ref.ty),
                HirExprKind::Binary { op, left, right } => {
                    ExprCase::Binary(*op, *left, *right, expr_ref.ty)
                }
                _ => ExprCase::Other,
            }
        };

        let result = match case {
            ExprCase::Literal(lit) => Some(lit),
            ExprCase::Unary(op, operand, ty) => {
                let operand_val = self.fold_expr(module, operand, primitives, cache)?;
                self.eval_unary(op, &operand_val, ty, primitives)
            }
            ExprCase::Binary(op, left, right, ty) => {
                let left_val = self.fold_expr(module, left, primitives, cache)?;
                let right_val = self.fold_expr(module, right, primitives, cache)?;
                self.eval_binary(op, ty, &left_val, &right_val, primitives)
            }
            ExprCase::Other => None,
        };

        if let Some(lit) = &result {
            if let Some(mut expr_mut) = module
                .exprs
                .get_mut(ArenaIndex::from_raw(id.to_raw() as usize))
            {
                expr_mut.const_value = Some(lit.clone());
            }
        }

        cache.insert(id, result.clone());
        result
    }

    fn eval_unary(
        &self,
        op: UnaryOp,
        value: &HirLiteral,
        result_ty: crate::semantics::hir::TypeCtxId,
        primitives: &PrimitiveTypes,
    ) -> Option<HirLiteral> {
        match op {
            UnaryOp::Neg => {
                if result_ty == primitives.float {
                    Some(HirLiteral::Float(-self.as_f64(value)?))
                } else {
                    Some(HirLiteral::Int(-self.as_int(value)?))
                }
            }
            UnaryOp::Not => Some(HirLiteral::Bool(!self.as_bool(value)?)),
            UnaryOp::BitNot => Some(HirLiteral::Int(!self.as_int(value)?)),
            UnaryOp::Ref | UnaryOp::Deref => None,
        }
    }

    fn eval_binary(
        &self,
        op: BinaryOp,
        expr_ty: crate::semantics::hir::TypeCtxId,
        left: &HirLiteral,
        right: &HirLiteral,
        primitives: &PrimitiveTypes,
    ) -> Option<HirLiteral> {
        use BinaryOp::*;

        match op {
            Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor | Shl | Shr => {
                self.eval_numeric(op, expr_ty, left, right, primitives)
            }
            Eq => Some(HirLiteral::Bool(self.eq_literals(left, right)?)),
            NotEq => Some(HirLiteral::Bool(!self.eq_literals(left, right)?)),
            Lt | Lte | Gt | Gte => Some(HirLiteral::Bool(self.compare(op, left, right)?)),
            And => Some(HirLiteral::Bool(
                self.as_bool(left)? && self.as_bool(right)?,
            )),
            Or => Some(HirLiteral::Bool(
                self.as_bool(left)? || self.as_bool(right)?,
            )),
            Assign | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign => None,
        }
    }

    fn eval_numeric(
        &self,
        op: BinaryOp,
        expr_ty: crate::semantics::hir::TypeCtxId,
        left: &HirLiteral,
        right: &HirLiteral,
        primitives: &PrimitiveTypes,
    ) -> Option<HirLiteral> {
        use BinaryOp::*;

        if expr_ty == primitives.float {
            let lhs = self.as_f64(left)?;
            let rhs = self.as_f64(right)?;
            let result = match op {
                Add => lhs + rhs,
                Sub => lhs - rhs,
                Mul => lhs * rhs,
                Div => lhs / rhs,
                Mod => lhs % rhs,
                _ => return None,
            };
            return Some(HirLiteral::Float(result));
        }

        let lhs = self.as_int(left)?;
        let rhs = self.as_int(right)?;

        let result = match op {
            Add => HirLiteral::Int(lhs + rhs),
            Sub => HirLiteral::Int(lhs - rhs),
            Mul => HirLiteral::Int(lhs * rhs),
            Div => HirLiteral::Int(lhs / rhs),
            Mod => HirLiteral::Int(lhs % rhs),
            BitAnd => HirLiteral::Int(lhs & rhs),
            BitOr => HirLiteral::Int(lhs | rhs),
            BitXor => HirLiteral::Int(lhs ^ rhs),
            Shl => HirLiteral::Int(lhs << rhs),
            Shr => HirLiteral::Int(lhs >> rhs),
            _ => return None,
        };

        Some(result)
    }

    fn eq_literals(&self, left: &HirLiteral, right: &HirLiteral) -> Option<bool> {
        use HirLiteral::*;
        let result = match (left, right) {
            (Int(a), Int(b)) => *a == *b,
            (Float(a), Float(b)) => *a == *b,
            (Int(a), Float(b)) => (*a as f64) == *b,
            (Float(a), Int(b)) => *a == (*b as f64),
            (String(a), String(b)) => a == b,
            (Bool(a), Bool(b)) => a == b,
            (Unit, Unit) => true,
            _ => return None,
        };
        Some(result)
    }

    fn compare(&self, op: BinaryOp, left: &HirLiteral, right: &HirLiteral) -> Option<bool> {
        use BinaryOp::*;

        if matches!(left, HirLiteral::String(_)) || matches!(right, HirLiteral::String(_)) {
            let lhs = self.as_string(left)?;
            let rhs = self.as_string(right)?;
            return Some(match op {
                Lt => lhs < rhs,
                Lte => lhs <= rhs,
                Gt => lhs > rhs,
                Gte => lhs >= rhs,
                _ => unreachable!(),
            });
        }

        if matches!(left, HirLiteral::Float(_)) || matches!(right, HirLiteral::Float(_)) {
            let lhs = self.as_f64(left)?;
            let rhs = self.as_f64(right)?;
            return Some(match op {
                Lt => lhs < rhs,
                Lte => lhs <= rhs,
                Gt => lhs > rhs,
                Gte => lhs >= rhs,
                _ => unreachable!(),
            });
        }

        let lhs = self.as_int(left)?;
        let rhs = self.as_int(right)?;
        Some(match op {
            Lt => lhs < rhs,
            Lte => lhs <= rhs,
            Gt => lhs > rhs,
            Gte => lhs >= rhs,
            _ => unreachable!(),
        })
    }

    fn as_int(&self, literal: &HirLiteral) -> Option<i128> {
        match literal {
            HirLiteral::Int(value) => Some(*value),
            HirLiteral::Float(value) => Some(*value as i128),
            HirLiteral::Bool(value) => Some(if *value { 1 } else { 0 }),
            _ => None,
        }
    }

    fn as_f64(&self, literal: &HirLiteral) -> Option<f64> {
        match literal {
            HirLiteral::Int(value) => Some(*value as f64),
            HirLiteral::Float(value) => Some(*value),
            _ => None,
        }
    }

    fn as_bool(&self, literal: &HirLiteral) -> Option<bool> {
        match literal {
            HirLiteral::Bool(value) => Some(*value),
            _ => None,
        }
    }

    fn as_string(&self, literal: &HirLiteral) -> Option<String> {
        match literal {
            HirLiteral::String(value) => Some(value.clone()),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::semantics::hir::{ArenaIndex, HirItemKind};
    use crate::semantics::lowering::LoweringContext;
    use core::ast::nodes::{Block, Expr, FnDecl, GenericParams, Ident, Item, Stmt, TypeExpr};
    use core::ast::{BinaryOp, UnaryOp};
    use core::diag::Span;

    fn ident(name: &str) -> Ident {
        Ident {
            name: name.to_string(),
            span: Some(Span { start: 0, end: 0 }),
        }
    }

    fn fold_module_from_expr(expr: Expr, ret_type: TypeExpr) -> HirModule {
        let fn_decl = FnDecl {
            vis: core::ast::nodes::Visibility::Public,
            name: ident("main"),
            generics: GenericParams { params: Vec::new() },
            params: Vec::new(),
            ret_type: Some(ret_type),
            body: Block {
                stmts: vec![Stmt::Return(Some(expr))],
            },
        };

        let mut module = LoweringContext::lower(&[Item::Fn(fn_decl)], None).module;
        ConstantFolder::new().fold_module(&mut module);
        module
    }

    #[test]
    fn folds_integer_addition() {
        let expr = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::IntLit { value: 1, ty: None }),
            right: Box::new(Expr::IntLit { value: 2, ty: None }),
        };

        let module = fold_module_from_expr(expr, TypeExpr::BuiltIn(core::ast::TypeIdentKind::Int));

        let func_id = module.roots[0];
        let func = module
            .items
            .get(ArenaIndex::from_raw(func_id.to_raw() as usize))
            .unwrap();

        let func_data = match &func.kind {
            HirItemKind::Function(func) => func,
            other => panic!("unexpected item kind: {other:?}"),
        };

        let ret_stmt_id = module
            .blocks
            .get(ArenaIndex::from_raw(func_data.body.to_raw() as usize))
            .unwrap()
            .stmts[0];

        let ret_stmt = module
            .stmts
            .get(ArenaIndex::from_raw(ret_stmt_id.to_raw() as usize))
            .unwrap();

        let expr_id = match &ret_stmt.kind {
            crate::semantics::hir::HirStmtKind::Return { value: Some(expr) } => *expr,
            other => panic!("unexpected stmt kind: {other:?}"),
        };

        let expr = module
            .exprs
            .get(ArenaIndex::from_raw(expr_id.to_raw() as usize))
            .unwrap();

        assert_eq!(expr.const_value, Some(HirLiteral::Int(3)));
    }

    #[test]
    fn folds_nested_float_expression() {
        let inner = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::IntLit { value: 1, ty: None }),
            right: Box::new(Expr::IntLit { value: 3, ty: None }),
        };
        let expr = Expr::Binary {
            op: BinaryOp::Mul,
            left: Box::new(Expr::FloatLit {
                value: 2.0,
                ty: None,
            }),
            right: Box::new(inner),
        };

        let module =
            fold_module_from_expr(expr, TypeExpr::BuiltIn(core::ast::TypeIdentKind::Float));

        let func_id = module.roots[0];
        let func = module
            .items
            .get(ArenaIndex::from_raw(func_id.to_raw() as usize))
            .unwrap();
        let func_data = match &func.kind {
            HirItemKind::Function(func) => func,
            other => panic!("unexpected item kind: {other:?}"),
        };
        let block = module
            .blocks
            .get(ArenaIndex::from_raw(func_data.body.to_raw() as usize))
            .unwrap();
        let ret_stmt_id = block.stmts[0];
        let ret_stmt = module
            .stmts
            .get(ArenaIndex::from_raw(ret_stmt_id.to_raw() as usize))
            .unwrap();
        let expr_id = match &ret_stmt.kind {
            crate::semantics::hir::HirStmtKind::Return { value: Some(expr) } => *expr,
            _ => panic!("expected return"),
        };

        let expr = module
            .exprs
            .get(ArenaIndex::from_raw(expr_id.to_raw() as usize))
            .unwrap();

        assert_eq!(expr.const_value, Some(HirLiteral::Float(8.0)));
    }

    #[test]
    fn annotates_unary_negation() {
        let expr = Expr::Unary {
            op: UnaryOp::Neg,
            expr: Box::new(Expr::IntLit { value: 5, ty: None }),
        };

        let module = fold_module_from_expr(expr, TypeExpr::BuiltIn(core::ast::TypeIdentKind::Int));

        let func_id = module.roots[0];
        let func = module
            .items
            .get(ArenaIndex::from_raw(func_id.to_raw() as usize))
            .unwrap();
        let func_data = match &func.kind {
            HirItemKind::Function(func) => func,
            other => panic!("unexpected item kind: {other:?}"),
        };
        let block = module
            .blocks
            .get(ArenaIndex::from_raw(func_data.body.to_raw() as usize))
            .unwrap();
        let stmt_id = block.stmts[0];
        let stmt = module
            .stmts
            .get(ArenaIndex::from_raw(stmt_id.to_raw() as usize))
            .unwrap();
        let expr_id = match &stmt.kind {
            crate::semantics::hir::HirStmtKind::Return { value: Some(expr) } => *expr,
            _ => panic!("expected return"),
        };
        let expr = module
            .exprs
            .get(ArenaIndex::from_raw(expr_id.to_raw() as usize))
            .unwrap();

        assert_eq!(expr.const_value, Some(HirLiteral::Int(-5)));
    }
}
