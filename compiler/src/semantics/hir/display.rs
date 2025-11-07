//! Pretty-printing helpers for HIR structures.
//!
//! The formatter produces deterministic, human-friendly output to assist with
//! debugging and snapshot-style regression tests.

use std::fmt::{self, Write};

use super::arena::ArenaIndex;
use super::ids::{HirBlockId, HirExprId, HirItemId, HirStmtId, TraitImplId, TypeCtxId};
use super::nodes::{HirExprKind, HirItemKind, HirLiteral, HirModule, HirStmtKind};
use super::types::TypeInfo;

/// Format a module into a human-readable string.
pub fn format_module(module: &HirModule) -> String {
    let mut formatter = HirFormatter::new();
    formatter.write_module(module).expect("string writer");
    formatter.finish()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantics::lowering::LoweringContext;
    use core::ast::nodes::{
        Block, FnDecl, GenericParams, Ident, IfStmt, ImplBlock, Item, Param, Stmt, TypeExpr,
    };
    use core::ast::{BinaryOp, Expr, TypeIdentKind};

    fn ident(name: &str) -> Ident {
        Ident {
            name: name.to_string(),
            span: None,
        }
    }

    fn lower_and_format(items: &[Item]) -> String {
        let lowering = LoweringContext::lower(items, None);
        assert!(lowering.diagnostics.is_empty());
        format_module(&lowering.module)
    }

    #[test]
    fn formats_simple_function_module() {
        let fn_decl = FnDecl {
            vis: core::ast::nodes::Visibility::Public,
            name: ident("main"),
            generics: GenericParams { params: Vec::new() },
            params: Vec::new(),
            ret_type: Some(TypeExpr::BuiltIn(TypeIdentKind::Int)),
            body: Block {
                stmts: vec![Stmt::Return(Some(Expr::IntLit { value: 1, ty: None }))],
            },
        };

        let rendered = lower_and_format(&[Item::Fn(fn_decl)]);
        let expected = "HIRModule {\n  items: 1, trait_impls: 0\n    fn main() -> int\n      block -> int {\n        return 1 : int;\n      }\n}\n";

        assert_eq!(rendered, expected);
    }

    #[test]
    fn formats_control_flow_statements() {
        let fn_decl = FnDecl {
            vis: core::ast::nodes::Visibility::Public,
            name: ident("control"),
            generics: GenericParams { params: Vec::new() },
            params: Vec::new(),
            ret_type: None,
            body: Block {
                stmts: vec![
                    Stmt::If(Box::new(IfStmt {
                        cond: Expr::Binary {
                            op: BinaryOp::Eq,
                            left: Box::new(Expr::IntLit { value: 1, ty: None }),
                            right: Box::new(Expr::IntLit { value: 2, ty: None }),
                        },
                        then_branch: Block {
                            stmts: vec![Stmt::Expr(Expr::IntLit { value: 0, ty: None })],
                        },
                        else_branch: Some(Box::new(Stmt::Expr(Expr::IntLit {
                            value: 3,
                            ty: None,
                        }))),
                    })),
                    Stmt::While {
                        cond: Expr::Binary {
                            op: BinaryOp::Eq,
                            left: Box::new(Expr::IntLit { value: 1, ty: None }),
                            right: Box::new(Expr::IntLit { value: 1, ty: None }),
                        },
                        body: Block {
                            stmts: vec![Stmt::Break],
                        },
                    },
                    Stmt::Loop(Block {
                        stmts: vec![Stmt::Break],
                    }),
                ],
            },
        };

        let rendered = lower_and_format(&[Item::Fn(fn_decl)]);
        let expected = "HIRModule {\n  items: 1, trait_impls: 0\n    fn control() -> unit\n      block -> unit {\n        if (1 Eq 2) : unit\n          block -> int {\n            0;\n          }\n        else\n          block -> int {\n            3;\n          }\n        while (1 Eq 1) : unit\n          block -> unit {\n            break : unit;\n          }\n        loop : unit\n          block -> unit {\n            break : unit;\n          }\n      }\n}\n";

        assert_eq!(rendered, expected);
    }

    #[test]
    fn formats_trait_impl_with_method() {
        let method = FnDecl {
            vis: core::ast::nodes::Visibility::Public,
            name: ident("next"),
            generics: GenericParams { params: Vec::new() },
            params: vec![Param {
                name: ident("self"),
                ty: Some(TypeExpr::BuiltIn(TypeIdentKind::Int)),
            }],
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

        let rendered = lower_and_format(&[Item::Impl(impl_block)]);
        let expected = "HIRModule {\n  items: 1, trait_impls: 1\n    trait_impl #0\n      impl Vector for Iterable\n        fn next(self: int) -> int\n          block -> int {\n            return 1 : int;\n          }\n  trait_impl_table {\n        Vector for Iterable -> 1 methods\n  }\n}\n";

        assert_eq!(rendered, expected);
    }
}

/// Stateful formatter used to render HIR nodes.
#[derive(Debug, Default)]
pub struct HirFormatter {
    buffer: String,
}

impl HirFormatter {
    /// Create a fresh formatter.
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
        }
    }

    /// Emit a textual representation of the provided module.
    pub fn write_module(&mut self, module: &HirModule) -> fmt::Result {
        writeln!(self.buffer, "HIRModule {{")?;
        writeln!(
            self.buffer,
            "  items: {}, trait_impls: {}",
            module.roots.len(),
            module.trait_impls.len()
        )?;

        for item_id in &module.roots {
            self.write_item(module, *item_id, 2)?;
        }

        if module.trait_impls.len() > 0 {
            writeln!(self.buffer, "  trait_impl_table {{")?;
            for idx in 0..module.trait_impls.len() {
                let impl_id = TraitImplId::from_raw(idx as u32);
                self.write_trait_impl_entry(module, impl_id, 4)?;
            }
            writeln!(self.buffer, "  }}")?;
        }

        writeln!(self.buffer, "}}")
    }

    /// Consume the formatter and return the accumulated string.
    pub fn finish(self) -> String {
        self.buffer
    }

    fn write_trait_impl_entry(
        &mut self,
        module: &HirModule,
        id: TraitImplId,
        indent: usize,
    ) -> fmt::Result {
        if let Some(entry) = module
            .trait_impls
            .get(ArenaIndex::from_raw(id.to_raw() as usize))
        {
            self.indent_line(
                indent,
                &format!(
                    "{} -> {} methods",
                    match &entry.interface {
                        Some(iface) => format!("{} for {}", entry.target, iface),
                        None => entry.target.clone(),
                    },
                    entry.methods.len()
                ),
            )?;
        }
        Ok(())
    }

    fn write_item(&mut self, module: &HirModule, id: HirItemId, indent: usize) -> fmt::Result {
        let Some(item) = module.items.get(ArenaIndex::from_raw(id.to_raw() as usize)) else {
            self.indent_line(indent, &format!("<missing item {:?}>", id))?;
            return Ok(());
        };

        match &item.kind {
            HirItemKind::Function(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|param| format!("{}: {}", param.name, self.type_label(module, param.ty)))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.indent_line(
                    indent,
                    &format!(
                        "fn {}({}) -> {}",
                        item.name,
                        params,
                        self.type_label(module, func.ret_type)
                    ),
                )?;
                self.write_block(module, func.body, indent + 1)?;
            }
            HirItemKind::Const(constant) => {
                self.indent_line(
                    indent,
                    &format!(
                        "const {}: {} = {}",
                        item.name,
                        self.type_label(module, constant.ty),
                        self.format_expr(module, constant.value)
                    ),
                )?;
            }
            HirItemKind::Object(object) => {
                self.indent_line(indent, &format!("object {} {{", item.name))?;
                for field in &object.fields {
                    let ty = field
                        .ty
                        .map(|ty| self.type_label(module, ty))
                        .unwrap_or_else(|| "any".to_string());
                    self.indent_line(indent + 1, &format!("{}: {}", field.name, ty))?;
                }
                self.indent_line(indent, "}")?;
            }
            HirItemKind::Interface(interface) => {
                self.indent_line(indent, &format!("interface {} {{", item.name))?;
                for method in &interface.methods {
                    let params = method
                        .params
                        .iter()
                        .map(|param| {
                            format!("{}: {}", param.name, self.type_label(module, param.ty))
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    let ret = method
                        .ret_type
                        .map(|ty| self.type_label(module, ty))
                        .unwrap_or_else(|| "unit".to_string());
                    self.indent_line(
                        indent + 1,
                        &format!("fn {}({}) -> {}", method.name, params, ret),
                    )?;
                }
                self.indent_line(indent, "}")?;
            }
            HirItemKind::Use(use_decl) => {
                let mut line = format!("use {}", use_decl.path.join("::"));
                if let Some(alias) = &use_decl.alias {
                    line.push_str(&format!(" as {}", alias));
                }
                self.indent_line(indent, &line)?;
            }
            HirItemKind::TraitImpl { impl_id } => {
                self.indent_line(indent, &format!("trait_impl #{}", impl_id.to_raw()))?;
                if let Some(entry) = module
                    .trait_impls
                    .get(ArenaIndex::from_raw(impl_id.to_raw() as usize))
                {
                    let header = match &entry.interface {
                        Some(iface) => format!("impl {} for {}", entry.target, iface),
                        None => format!("impl {}", entry.target),
                    };
                    self.indent_line(indent + 1, &header)?;
                    for method in &entry.methods {
                        self.write_item(module, *method, indent + 2)?;
                    }
                }
            }
            HirItemKind::Unsupported { reason } => {
                self.indent_line(indent, &format!("<unsupported item {}>", reason))?;
            }
        }

        Ok(())
    }

    fn write_block(&mut self, module: &HirModule, id: HirBlockId, indent: usize) -> fmt::Result {
        let Some(block) = module
            .blocks
            .get(ArenaIndex::from_raw(id.to_raw() as usize))
        else {
            self.indent_line(indent, &format!("<missing block {:?}>", id))?;
            return Ok(());
        };

        self.indent_line(
            indent,
            &format!("block -> {} {{", self.type_label(module, block.ty)),
        )?;
        for stmt in &block.stmts {
            self.write_stmt(module, *stmt, indent + 1)?;
        }
        self.indent_line(indent, "}")
    }

    fn write_stmt(&mut self, module: &HirModule, id: HirStmtId, indent: usize) -> fmt::Result {
        let Some(stmt) = module.stmts.get(ArenaIndex::from_raw(id.to_raw() as usize)) else {
            self.indent_line(indent, &format!("<missing stmt {:?}>", id))?;
            return Ok(());
        };

        let ty_suffix = stmt
            .ty
            .map(|ty| format!(" : {}", self.type_label(module, ty)))
            .unwrap_or_default();

        match &stmt.kind {
            HirStmtKind::Let { name, value } => {
                let mut line = format!("let {}{}", name, ty_suffix);
                if let Some(expr) = value {
                    line.push_str(" = ");
                    line.push_str(&self.format_expr(module, *expr));
                }
                line.push(';');
                self.indent_line(indent, &line)?;
            }
            HirStmtKind::Const { name, value } => {
                self.indent_line(
                    indent,
                    &format!(
                        "const {}{} = {};",
                        name,
                        ty_suffix,
                        self.format_expr(module, *value)
                    ),
                )?;
            }
            HirStmtKind::Var { name, value } => {
                let mut line = format!("var {}{}", name, ty_suffix);
                if let Some(expr) = value {
                    line.push_str(" = ");
                    line.push_str(&self.format_expr(module, *expr));
                }
                line.push(';');
                self.indent_line(indent, &line)?;
            }
            HirStmtKind::Expr(expr) => {
                self.indent_line(indent, &format!("{};", self.format_expr(module, *expr)))?;
            }
            HirStmtKind::Return { value } => {
                if let Some(expr) = value {
                    self.indent_line(
                        indent,
                        &format!("return {}{};", self.format_expr(module, *expr), ty_suffix),
                    )?;
                } else {
                    self.indent_line(indent, &format!("return{};", ty_suffix))?;
                }
            }
            HirStmtKind::Assign { target, value } => {
                self.indent_line(
                    indent,
                    &format!(
                        "{} = {};{}",
                        self.format_expr(module, *target),
                        self.format_expr(module, *value),
                        ty_suffix
                    ),
                )?;
            }
            HirStmtKind::Block(block_id) => {
                self.write_block(module, *block_id, indent + 1)?;
            }
            HirStmtKind::If {
                cond,
                then_block,
                else_block,
            } => {
                self.indent_line(
                    indent,
                    &format!("if {}{}", self.format_expr(module, *cond), ty_suffix),
                )?;
                self.write_block(module, *then_block, indent + 1)?;
                if let Some(else_block) = else_block {
                    self.indent_line(indent, "else")?;
                    self.write_block(module, *else_block, indent + 1)?;
                }
            }
            HirStmtKind::While { cond, body } => {
                self.indent_line(
                    indent,
                    &format!("while {}{}", self.format_expr(module, *cond), ty_suffix),
                )?;
                self.write_block(module, *body, indent + 1)?;
            }
            HirStmtKind::Loop { body } => {
                self.indent_line(indent, &format!("loop{}", ty_suffix))?;
                self.write_block(module, *body, indent + 1)?;
            }
            HirStmtKind::Break => {
                self.indent_line(indent, &format!("break{};", ty_suffix))?;
            }
            HirStmtKind::Continue => {
                self.indent_line(indent, &format!("continue{};", ty_suffix))?;
            }
            HirStmtKind::Unsupported { reason } => {
                self.indent_line(indent, &format!("<unsupported stmt {}>", reason))?;
            }
        }

        Ok(())
    }

    fn format_expr(&self, module: &HirModule, id: HirExprId) -> String {
        let Some(expr) = module.exprs.get(ArenaIndex::from_raw(id.to_raw() as usize)) else {
            return format!("<missing expr {:?}>", id);
        };

        let mut rendered = match &expr.kind {
            HirExprKind::Literal(lit) => self.literal_to_string(lit),
            HirExprKind::Identifier { name, .. } => name.clone(),
            HirExprKind::Unary { op, operand } => {
                format!("({:?} {})", op, self.format_expr(module, *operand))
            }
            HirExprKind::Binary { op, left, right } => format!(
                "({} {:?} {})",
                self.format_expr(module, *left),
                op,
                self.format_expr(module, *right)
            ),
            HirExprKind::Tuple(elements) => {
                let inner = elements
                    .iter()
                    .map(|expr| self.format_expr(module, *expr))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", inner)
            }
            HirExprKind::Array(elements) => {
                let inner = elements
                    .iter()
                    .map(|expr| self.format_expr(module, *expr))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", inner)
            }
            HirExprKind::Map(entries) => {
                let inner = entries
                    .iter()
                    .map(|(key, value)| {
                        format!(
                            "{}: {}",
                            self.format_expr(module, *key),
                            self.format_expr(module, *value)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", inner)
            }
            HirExprKind::Call { callee, args } => {
                let args = args
                    .iter()
                    .map(|arg| self.format_expr(module, *arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", self.format_expr(module, *callee), args)
            }
            HirExprKind::Block(block) => format!("block#{}", block.to_raw()),
            HirExprKind::Unsupported { reason } => format!("<unsupported expr {}>", reason),
        };

        if let Some(value) = &expr.const_value {
            if !matches!(expr.kind, HirExprKind::Literal(_)) {
                rendered = format!("{}/*={}*/", rendered, self.literal_to_string(value));
            }
        }

        rendered
    }

    fn type_label(&self, module: &HirModule, ty: TypeCtxId) -> String {
        match module.types.get(ty) {
            Some(TypeInfo::Any) => "any".to_string(),
            Some(TypeInfo::Concrete(id)) => id.name.clone(),
            None => "unknown".to_string(),
        }
    }

    fn indent_line(&mut self, indent: usize, line: &str) -> fmt::Result {
        for _ in 0..indent {
            write!(self.buffer, "  ")?;
        }
        writeln!(self.buffer, "{}", line)
    }

    fn literal_to_string(&self, lit: &HirLiteral) -> String {
        match lit {
            HirLiteral::Int(value) => value.to_string(),
            HirLiteral::Float(value) => value.to_string(),
            HirLiteral::String(value) => format!("\"{}\"", value),
            HirLiteral::Bool(value) => value.to_string(),
            HirLiteral::Unit => "unit".to_string(),
        }
    }
}
