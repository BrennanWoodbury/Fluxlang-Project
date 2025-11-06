//! FluxLang front-end utilities.
//!
//! This module exposes the lexer together with a hand-written
//! recursive-descent parser that lowers source text into the AST under
//! `core::ast::nodes`. The initial implementation focuses on the minimal
//! language surface required by the project roadmap: module level items,
//! statement blocks, and expression parsing with a small but extensible
//! operator set. Diagnostics accumulate during the walk so callers can
//! surface multiple syntax issues in a single pass.

pub mod lexer;

use crate::ast::nodes::{
    Block, ConstDecl, Expr, FieldDecl, FnDecl, FnSig, ForStmt, GenericParams, Ident, ImplBlock,
    InterfaceDecl, Item, LetDecl, ObjDecl, Param, Stmt, TypeExpr, UseDecl, VarDecl, Visibility,
};
use crate::ast::{BinaryOp, TokenKind, TypeIdentKind, UnaryOp};
use crate::diag::Span;
use lexer::{LexError, Lexer, Token};
use std::str::FromStr;

/// Resulting syntax tree plus the list of diagnostics gathered while parsing.
#[derive(Debug, Default)]
pub struct ParseOutput {
    pub items: Vec<Item>,
    pub errors: Vec<ParseError>,
}

/// Syntax error emitted by the parser along with recovery hints.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
    pub expected: Vec<String>,
    pub found: Option<TokenKind>,
}

impl ParseError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            expected: Vec::new(),
            found: None,
        }
    }

    fn with_expected(mut self, expected: impl Into<String>) -> Self {
        self.expected.push(expected.into());
        self
    }

    fn with_found(mut self, found: TokenKind) -> Self {
        self.found = Some(found);
        self
    }
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        Self {
            message: err.message,
            span: err.span,
            expected: Vec::new(),
            found: None,
        }
    }
}

/// Parse a FluxLang source string into AST items plus collected errors.
pub fn parse_source(source: &str) -> ParseOutput {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    loop {
        match lexer.next_token() {
            Ok(token) => {
                let is_eof = matches!(token.kind, TokenKind::Eof);
                tokens.push(token);
                if is_eof {
                    break;
                }
            }
            Err(err) => {
                errors.push(ParseError::from(err));
                // Attempt a graceful exit by pushing a synthetic EOF so the
                // parser can still try to recover any already-read tokens.
                tokens.push(Token {
                    kind: TokenKind::Eof,
                    span: Span {
                        start: source.len(),
                        end: source.len(),
                    },
                });
                break;
            }
        }
    }

    if tokens.is_empty() {
        tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span { start: 0, end: 0 },
        });
    }

    let mut parser = Parser::new(tokens, errors);
    let items = parser.parse_items();
    ParseOutput {
        items,
        errors: parser.finish(),
    }
}

struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    fn new(tokens: Vec<Token>, errors: Vec<ParseError>) -> Self {
        Self {
            tokens,
            cursor: 0,
            errors,
        }
    }

    fn finish(self) -> Vec<ParseError> {
        self.errors
    }

    fn parse_items(&mut self) -> Vec<Item> {
        let mut items = Vec::new();
        while !self.is_eof() {
            self.skip_trivia_tokens();
            if self.is_eof() {
                break;
            }

            let visibility = self.parse_visibility();
            match self.current_kind() {
                TokenKind::KwUse => match self.parse_use(visibility) {
                    Some(item) => items.push(item),
                    None => self.synchronize_item(),
                },
                TokenKind::KwObj => match self.parse_obj(visibility) {
                    Some(item) => items.push(Item::Obj(item)),
                    None => self.synchronize_item(),
                },
                TokenKind::KwInterface => match self.parse_interface(visibility) {
                    Some(item) => items.push(Item::Interface(item)),
                    None => self.synchronize_item(),
                },
                TokenKind::KwImpl => match self.parse_impl(visibility) {
                    Some(item) => items.push(Item::Impl(item)),
                    None => self.synchronize_item(),
                },
                TokenKind::KwFn => match self.parse_fn(visibility) {
                    Some(item) => items.push(Item::Fn(item)),
                    None => self.synchronize_item(),
                },
                _ => {
                    self.push_error(
                        ParseError::new("expected an item", self.current_span())
                            .with_expected("item keyword (fn, obj, interface, impl, use)"),
                    );
                    self.synchronize_item();
                }
            }
        }
        items
    }

    fn parse_visibility(&mut self) -> Visibility {
        if matches!(self.current_kind(), TokenKind::KwPub) {
            self.bump();
            Visibility::Public
        } else {
            Visibility::Private
        }
    }

    fn parse_use(&mut self, vis: Visibility) -> Option<Item> {
        let kw_span = self.bump().span;
        let mut path = Vec::new();
        loop {
            match self.parse_ident("use path segment") {
                Some(segment) => path.push(segment),
                None => break,
            }
            if !matches!(self.current_kind(), TokenKind::ColonColon) {
                break;
            }
            self.bump();
        }

        if path.is_empty() {
            self.push_error(
                ParseError::new("`use` requires at least one path segment", kw_span)
                    .with_expected("identifier"),
            );
        }

        let alias = if matches!(self.current_kind(), TokenKind::KwAs) {
            self.bump();
            self.parse_ident("use alias")
        } else {
            None
        };

        self.expect_token(TokenKind::Semicolon, "`;` to terminate use statement");

        Some(Item::Use(UseDecl { vis, path, alias }))
    }

    fn parse_obj(&mut self, vis: Visibility) -> Option<ObjDecl> {
        self.bump(); // consume `obj`
        let name = self.parse_ident("object name")?;
        let generics = self.parse_generic_params();
        self.expect_token(TokenKind::LBrace, "`{` to start object body")?;

        let mut fields = Vec::new();
        while !matches!(self.current_kind(), TokenKind::RBrace | TokenKind::Eof) {
            let field_name = match self.parse_ident("field name") {
                Some(ident) => ident,
                None => {
                    self.synchronize_field();
                    continue;
                }
            };
            self.expect_token(TokenKind::Colon, "`:` after field name")?;
            let ty = match self.parse_type_expr() {
                Some(ty) => ty,
                None => {
                    self.synchronize_field();
                    continue;
                }
            };
            self.expect_token(TokenKind::Semicolon, "`;` after field declaration");
            fields.push(FieldDecl {
                name: field_name,
                ty,
            });
        }
        self.expect_token(TokenKind::RBrace, "`}` to close object body");

        Some(ObjDecl {
            vis,
            name,
            generics,
            fields,
        })
    }

    fn parse_interface(&mut self, vis: Visibility) -> Option<InterfaceDecl> {
        self.bump();
        let name = self.parse_ident("interface name")?;
        let generics = self.parse_generic_params();
        self.expect_token(TokenKind::LBrace, "`{` to start interface body")?;

        let mut methods = Vec::new();
        while !matches!(self.current_kind(), TokenKind::RBrace | TokenKind::Eof) {
            match self.parse_fn_signature(vis) {
                Some(sig) => methods.push(sig),
                None => {
                    self.synchronize_item();
                }
            }
        }
        self.expect_token(TokenKind::RBrace, "`}` to close interface body");

        Some(InterfaceDecl {
            vis,
            name,
            generics,
            methods,
        })
    }

    fn parse_impl(&mut self, vis: Visibility) -> Option<ImplBlock> {
        let impl_token = self.bump();
        let generics = self.parse_generic_params();
        let target = self.parse_type_expr()?;
        let interface = None; // `impl Trait for Type` support is future work.

        self.expect_token(TokenKind::LBrace, "`{` to start impl body")?;
        let mut methods = Vec::new();
        while !matches!(self.current_kind(), TokenKind::RBrace | TokenKind::Eof) {
            let method_vis = self.parse_visibility();
            match self.parse_fn(method_vis) {
                Some(fn_decl) => methods.push(fn_decl),
                None => {
                    self.synchronize_item();
                }
            }
        }
        self.expect_token(TokenKind::RBrace, "`}` to close impl body");

        Some(ImplBlock {
            vis,
            generics,
            target,
            interface,
            methods,
            span: Some(impl_token.span),
        })
    }

    fn parse_fn(&mut self, vis: Visibility) -> Option<FnDecl> {
        self.bump();
        let name = self.parse_ident("function name")?;
        let generics = self.parse_generic_params();
        let params = self.parse_param_list()?;
        let ret_type = if matches!(self.current_kind(), TokenKind::Arrow) {
            self.bump();
            self.parse_type_expr()
        } else {
            None
        };
        let body = match self.parse_block() {
            Some(block) => block,
            None => Block { stmts: Vec::new() },
        };
        Some(FnDecl {
            vis,
            name,
            generics,
            params,
            ret_type,
            body,
        })
    }

    fn parse_fn_signature(&mut self, vis: Visibility) -> Option<FnSig> {
        match self.current_kind() {
            TokenKind::KwFn => {
                self.bump();
                let name = self.parse_ident("function name")?;
                let generics = self.parse_generic_params();
                let params = self.parse_param_list()?;
                let ret_type = if matches!(self.current_kind(), TokenKind::Arrow) {
                    self.bump();
                    self.parse_type_expr()
                } else {
                    None
                };
                let span = Some(self.previous_span());
                self.expect_token(TokenKind::Semicolon, "`;` after function signature");
                Some(FnSig {
                    vis,
                    name,
                    generics,
                    params,
                    ret_type,
                    span,
                })
            }
            _ => {
                self.push_error(
                    ParseError::new("expected `fn` inside interface", self.current_span())
                        .with_expected("fn"),
                );
                None
            }
        }
    }

    fn parse_param_list(&mut self) -> Option<Vec<Param>> {
        self.expect_token(TokenKind::LParen, "`(` to start parameter list")?;
        let mut params = Vec::new();
        while !matches!(self.current_kind(), TokenKind::RParen | TokenKind::Eof) {
            let name = match self.parse_ident("parameter name") {
                Some(name) => name,
                None => {
                    self.synchronize_params();
                    continue;
                }
            };
            let ty = if matches!(self.current_kind(), TokenKind::Colon) {
                self.bump();
                self.parse_type_expr()
            } else {
                None
            };
            params.push(Param { name, ty });

            if matches!(self.current_kind(), TokenKind::Comma) {
                self.bump();
            } else {
                break;
            }
        }
        self.expect_token(TokenKind::RParen, "`)` to end parameter list");
        Some(params)
    }

    fn parse_block(&mut self) -> Option<Block> {
        self.expect_token(TokenKind::LBrace, "`{` to start block")?;
        let mut stmts = Vec::new();
        while !matches!(self.current_kind(), TokenKind::RBrace | TokenKind::Eof) {
            match self.parse_stmt() {
                Some(stmt) => stmts.push(stmt),
                None => self.synchronize_statement(),
            }
        }
        self.expect_token(TokenKind::RBrace, "`}` to close block");
        Some(Block { stmts })
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.current_kind() {
            TokenKind::KwConst => self.parse_const_stmt(),
            TokenKind::KwLet => self.parse_let_stmt(),
            TokenKind::KwVar => self.parse_var_stmt(),
            TokenKind::KwReturn => self.parse_return_stmt(),
            TokenKind::KwThrow => self.parse_throw_stmt(),
            TokenKind::KwPanic => self.parse_panic_stmt(),
            TokenKind::KwFor => self.parse_for_stmt(),
            TokenKind::KwWhile => self.parse_while_stmt(),
            TokenKind::KwBreak => self.parse_break_stmt(),
            TokenKind::KwContinue => self.parse_continue_stmt(),
            TokenKind::KwLoop => self.parse_loop_stmt(),
            TokenKind::LBrace => self.parse_block().map(Stmt::Block),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_const_stmt(&mut self) -> Option<Stmt> {
        let kw_span = self.bump().span;
        let name = self.parse_ident("const name")?;
        let ty = if matches!(self.current_kind(), TokenKind::Colon) {
            self.bump();
            self.parse_type_expr()
        } else {
            None
        };
        self.expect_token(TokenKind::Assign, "`=` in const declaration")?;
        let value = self.parse_expression()?;
        self.expect_token(TokenKind::Semicolon, "`;` after const declaration");
        Some(Stmt::Const(ConstDecl {
            name,
            ty,
            value,
            span: Some(kw_span),
        }))
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        let kw_span = self.bump().span;
        let name = self.parse_ident("let binding name")?;
        let ty = if matches!(self.current_kind(), TokenKind::Colon) {
            self.bump();
            self.parse_type_expr()
        } else {
            None
        };
        let value = if matches!(self.current_kind(), TokenKind::Assign) {
            self.bump();
            self.parse_expression()
        } else {
            None
        };
        self.expect_token(TokenKind::Semicolon, "`;` after let statement");
        Some(Stmt::Let(LetDecl {
            name,
            ty,
            value,
            span: Some(kw_span),
        }))
    }

    fn parse_var_stmt(&mut self) -> Option<Stmt> {
        let kw_span = self.bump().span;
        let name = self.parse_ident("var binding name")?;
        self.expect_token(TokenKind::Colon, "`:` after var name")?;
        let ty = self.parse_type_expr()?;
        let value = if matches!(self.current_kind(), TokenKind::Assign) {
            self.bump();
            self.parse_expression()
        } else {
            None
        };
        self.expect_token(TokenKind::Semicolon, "`;` after var statement");
        Some(Stmt::Var(VarDecl {
            name,
            ty,
            value,
            span: Some(kw_span),
        }))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        let value = if matches!(self.current_kind(), TokenKind::Semicolon) {
            None
        } else {
            self.parse_expression()
        };
        self.expect_token(TokenKind::Semicolon, "`;` after return statement");
        Some(Stmt::Return(value))
    }

    fn parse_throw_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        let value = self.parse_expression()?;
        self.expect_token(TokenKind::Semicolon, "`;` after throw statement");
        Some(Stmt::Throw(value))
    }

    fn parse_panic_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        let value = if matches!(self.current_kind(), TokenKind::Semicolon) {
            None
        } else {
            self.parse_expression()
        };
        self.expect_token(TokenKind::Semicolon, "`;` after panic statement");
        Some(Stmt::Panic(value))
    }

    fn parse_while_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        let cond = self.parse_expression()?;
        let body = self.parse_block()?;
        Some(Stmt::While { cond, body })
    }

    fn parse_break_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        self.expect_token(TokenKind::Semicolon, "`;` after break statement");
        Some(Stmt::Break)
    }

    fn parse_continue_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        self.expect_token(TokenKind::Semicolon, "`;` after continue statement");
        Some(Stmt::Continue)
    }

    fn parse_for_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        self.expect_token(TokenKind::LParen, "`(` to start for header")?;

        let init = if matches!(self.current_kind(), TokenKind::Semicolon) {
            self.bump();
            None
        } else {
            let stmt = self.parse_for_initializer()?;
            self.expect_token(TokenKind::Semicolon, "`;` after for initializer")?;
            Some(Box::new(stmt))
        };

        let cond = if matches!(self.current_kind(), TokenKind::Semicolon) {
            self.bump();
            None
        } else {
            let expr = self.parse_expression()?;
            self.expect_token(TokenKind::Semicolon, "`;` after for condition")?;
            Some(expr)
        };

        let step = if matches!(self.current_kind(), TokenKind::RParen) {
            None
        } else {
            self.parse_expression()
        };

        self.expect_token(TokenKind::RParen, "`)` to close for header")?;
        let body = self.parse_block()?;

        Some(Stmt::For(Box::new(ForStmt {
            init,
            cond,
            step,
            body,
        })))
    }

    fn parse_loop_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        let body = self.parse_block()?;
        Some(Stmt::Loop(body))
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression()?;
        self.expect_token(TokenKind::Semicolon, "`;` after expression statement");
        Some(self.expr_to_stmt(expr))
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        self.parse_expression_bp(0)
    }

    fn parse_expression_bp(&mut self, min_bp: u8) -> Option<Expr> {
        let mut lhs = match self.current_kind() {
            TokenKind::Minus => {
                self.bump();
                let rhs = self.parse_expression_bp(PREC_PREFIX)?;
                Expr::Unary {
                    op: UnaryOp::Neg,
                    expr: Box::new(rhs),
                }
            }
            TokenKind::Bang => {
                self.bump();
                let rhs = self.parse_expression_bp(PREC_PREFIX)?;
                Expr::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(rhs),
                }
            }
            TokenKind::LParen => {
                self.bump();
                let expr = self.parse_expression()?;
                self.expect_token(TokenKind::RParen, "`)` to close grouping");
                expr
            }
            _ => self.parse_postfix_primary()?,
        };

        loop {
            let (left_bp, right_bp, op) = match self.infix_binding_power() {
                Some(info) => info,
                None => break,
            };

            if left_bp < min_bp {
                break;
            }

            self.bump();
            let rhs = match self.parse_expression_bp(right_bp) {
                Some(expr) => expr,
                None => return None,
            };
            lhs = Expr::Binary {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
            };
        }

        Some(lhs)
    }

    fn parse_postfix_primary(&mut self) -> Option<Expr> {
        let mut expr = match self.current_kind() {
            TokenKind::Ident(_) => {
                let token = self.bump();
                if let TokenKind::Ident(name) = token.kind {
                    Expr::Ident(Ident {
                        name,
                        span: Some(token.span),
                    })
                } else {
                    unreachable!()
                }
            }
            TokenKind::IntLit { .. } => self.parse_int_literal()?,
            TokenKind::FloatLit { .. } => self.parse_float_literal()?,
            TokenKind::StringLit { .. } => self.parse_string_literal()?,
            _ => {
                self.push_error(
                    ParseError::new("expected expression", self.current_span())
                        .with_expected("literal or identifier"),
                );
                return None;
            }
        };

        loop {
            match self.current_kind() {
                TokenKind::LParen => {
                    let args = self.parse_call_args()?;
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        args,
                    };
                }
                TokenKind::Dot => {
                    self.bump();
                    let member_ident = self.parse_ident("member name")?;
                    expr = Expr::Member {
                        object: Box::new(expr),
                        args: vec![Expr::Ident(member_ident)],
                    };
                }
                _ => break,
            }
        }

        Some(expr)
    }

    fn parse_call_args(&mut self) -> Option<Vec<Expr>> {
        self.expect_token(TokenKind::LParen, "`(` to start call arguments")?;
        let mut args = Vec::new();
        while !matches!(self.current_kind(), TokenKind::RParen | TokenKind::Eof) {
            let expr = self.parse_expression()?;
            args.push(expr);
            if matches!(self.current_kind(), TokenKind::Comma) {
                self.bump();
            } else {
                break;
            }
        }
        self.expect_token(TokenKind::RParen, "`)` to close call");
        Some(args)
    }

    fn parse_int_literal(&mut self) -> Option<Expr> {
        let token = self.bump();
        if let TokenKind::IntLit { value, ty } = token.kind.clone() {
            match i128::from_str(&value) {
                Ok(parsed) => Some(Expr::IntLit { value: parsed, ty }),
                Err(_) => {
                    self.push_error(
                        ParseError::new("integer literal out of range", token.span)
                            .with_found(TokenKind::IntLit { value, ty }),
                    );
                    None
                }
            }
        } else {
            None
        }
    }

    fn parse_float_literal(&mut self) -> Option<Expr> {
        let token = self.bump();
        if let TokenKind::FloatLit { value, ty } = token.kind.clone() {
            match f64::from_str(&value) {
                Ok(parsed) => Some(Expr::FloatLit {
                    value: parsed,
                    ty: ty.and_then(float_suffix_bits),
                }),
                Err(_) => {
                    self.push_error(
                        ParseError::new("invalid float literal", token.span)
                            .with_found(TokenKind::FloatLit { value, ty }),
                    );
                    None
                }
            }
        } else {
            None
        }
    }

    fn parse_for_initializer(&mut self) -> Option<Stmt> {
        match self.current_kind() {
            TokenKind::KwLet => self.parse_for_let_init(),
            TokenKind::KwConst => self.parse_for_const_init(),
            TokenKind::KwVar => self.parse_for_var_init(),
            _ => self.parse_expression().map(|expr| self.expr_to_stmt(expr)),
        }
    }

    fn parse_for_let_init(&mut self) -> Option<Stmt> {
        let kw_span = self.bump().span;
        let name = self.parse_ident("let binding name")?;
        let ty = if matches!(self.current_kind(), TokenKind::Colon) {
            self.bump();
            self.parse_type_expr()
        } else {
            None
        };
        let value = if matches!(self.current_kind(), TokenKind::Assign) {
            self.bump();
            self.parse_expression()
        } else {
            None
        };
        Some(Stmt::Let(LetDecl {
            name,
            ty,
            value,
            span: Some(kw_span),
        }))
    }

    fn parse_for_const_init(&mut self) -> Option<Stmt> {
        let kw_span = self.bump().span;
        let name = self.parse_ident("const name")?;
        let ty = if matches!(self.current_kind(), TokenKind::Colon) {
            self.bump();
            self.parse_type_expr()
        } else {
            None
        };
        self.expect_token(TokenKind::Assign, "`=` in const declaration")?;
        let value = self.parse_expression()?;
        Some(Stmt::Const(ConstDecl {
            name,
            ty,
            value,
            span: Some(kw_span),
        }))
    }

    fn parse_for_var_init(&mut self) -> Option<Stmt> {
        let kw_span = self.bump().span;
        let name = self.parse_ident("var binding name")?;
        self.expect_token(TokenKind::Colon, "`:` after var name")?;
        let ty = self.parse_type_expr()?;
        let value = if matches!(self.current_kind(), TokenKind::Assign) {
            self.bump();
            self.parse_expression()
        } else {
            None
        };
        Some(Stmt::Var(VarDecl {
            name,
            ty,
            value,
            span: Some(kw_span),
        }))
    }

    fn parse_string_literal(&mut self) -> Option<Expr> {
        let token = self.bump();
        if let TokenKind::StringLit { value, size_suffix } = token.kind {
            Some(Expr::StringLit {
                value,
                size: size_suffix,
            })
        } else {
            None
        }
    }

    fn parse_type_expr(&mut self) -> Option<TypeExpr> {
        match self.current_kind() {
            TokenKind::Ident(_) => {
                let token = self.bump();
                if let TokenKind::Ident(name) = token.kind.clone() {
                    let ident = Ident {
                        name,
                        span: Some(token.span),
                    };
                    if matches!(self.current_kind(), TokenKind::Lt) {
                        let args = self.parse_type_arg_list()?;
                        Some(TypeExpr::Generic { base: ident, args })
                    } else {
                        Some(TypeExpr::Named(ident))
                    }
                } else {
                    None
                }
            }
            TokenKind::TypeIdent(kind) => {
                let kind = *kind;
                self.bump();
                Some(TypeExpr::BuiltIn(kind))
            }
            _ => {
                self.push_error(
                    ParseError::new("expected type", self.current_span())
                        .with_expected("type identifier"),
                );
                None
            }
        }
    }

    fn parse_type_arg_list(&mut self) -> Option<Vec<TypeExpr>> {
        self.expect_token(TokenKind::Lt, "`<` to start generic arguments")?;
        let mut args = Vec::new();
        while !matches!(self.current_kind(), TokenKind::Gt | TokenKind::Eof) {
            let ty = match self.parse_type_expr() {
                Some(ty) => ty,
                None => {
                    self.synchronize_type_arguments();
                    continue;
                }
            };
            args.push(ty);
            if matches!(self.current_kind(), TokenKind::Comma) {
                self.bump();
            } else {
                break;
            }
        }
        self.expect_token(TokenKind::Gt, "`>` to close generic arguments");
        Some(args)
    }

    fn parse_generic_params(&mut self) -> GenericParams {
        if !matches!(self.current_kind(), TokenKind::Lt) {
            return GenericParams { params: Vec::new() };
        }
        self.bump();
        let mut params = Vec::new();
        while !matches!(self.current_kind(), TokenKind::Gt | TokenKind::Eof) {
            match self.parse_ident("generic parameter") {
                Some(ident) => params.push(ident),
                None => self.synchronize_type_arguments(),
            }
            if matches!(self.current_kind(), TokenKind::Comma) {
                self.bump();
            } else {
                break;
            }
        }
        self.expect_token(TokenKind::Gt, "`>` to end generic parameters");
        GenericParams { params }
    }

    fn parse_ident(&mut self, context: &str) -> Option<Ident> {
        match self.current_kind() {
            TokenKind::Ident(_) => {
                let token = self.bump();
                if let TokenKind::Ident(name) = token.kind {
                    Some(Ident {
                        name,
                        span: Some(token.span),
                    })
                } else {
                    unreachable!()
                }
            }
            _ => {
                self.push_error(
                    ParseError::new(
                        format!("expected identifier for {}", context),
                        self.current_span(),
                    )
                    .with_expected("identifier"),
                );
                None
            }
        }
    }

    fn expect_token(&mut self, kind: TokenKind, expected: &str) -> Option<Token> {
        if std::mem::discriminant(self.current_kind()) == std::mem::discriminant(&kind) {
            return Some(self.bump());
        }
        let found = self.current_kind().clone();
        self.push_error(
            ParseError::new(format!("expected {}", expected), self.current_span())
                .with_expected(expected.to_owned())
                .with_found(found),
        );
        None
    }

    fn infix_binding_power(&self) -> Option<(u8, u8, BinaryOp)> {
        match self.current_kind() {
            TokenKind::Assign => Some((PREC_ASSIGN, PREC_ASSIGN - 1, BinaryOp::Assign)),
            TokenKind::PlusEq => Some((PREC_ASSIGN, PREC_ASSIGN - 1, BinaryOp::AddAssign)),
            TokenKind::MinusEq => Some((PREC_ASSIGN, PREC_ASSIGN - 1, BinaryOp::SubAssign)),
            TokenKind::StarEq => Some((PREC_ASSIGN, PREC_ASSIGN - 1, BinaryOp::MulAssign)),
            TokenKind::SlashEq => Some((PREC_ASSIGN, PREC_ASSIGN - 1, BinaryOp::DivAssign)),
            TokenKind::PercentEq => Some((PREC_ASSIGN, PREC_ASSIGN - 1, BinaryOp::ModAssign)),
            TokenKind::Plus => Some((PREC_SUM, PREC_SUM + 1, BinaryOp::Add)),
            TokenKind::Minus => Some((PREC_SUM, PREC_SUM + 1, BinaryOp::Sub)),
            TokenKind::Star => Some((PREC_PRODUCT, PREC_PRODUCT + 1, BinaryOp::Mul)),
            TokenKind::Slash => Some((PREC_PRODUCT, PREC_PRODUCT + 1, BinaryOp::Div)),
            TokenKind::Percent => Some((PREC_PRODUCT, PREC_PRODUCT + 1, BinaryOp::Mod)),
            TokenKind::EqEq => Some((PREC_EQUALITY, PREC_EQUALITY + 1, BinaryOp::Eq)),
            TokenKind::NotEq => Some((PREC_EQUALITY, PREC_EQUALITY + 1, BinaryOp::NotEq)),
            TokenKind::Lt => Some((PREC_COMPARISON, PREC_COMPARISON + 1, BinaryOp::Lt)),
            TokenKind::LtEq => Some((PREC_COMPARISON, PREC_COMPARISON + 1, BinaryOp::Lte)),
            TokenKind::Gt => Some((PREC_COMPARISON, PREC_COMPARISON + 1, BinaryOp::Gt)),
            TokenKind::GtEq => Some((PREC_COMPARISON, PREC_COMPARISON + 1, BinaryOp::Gte)),
            _ => None,
        }
    }

    fn skip_trivia_tokens(&mut self) {
        while matches!(self.current_kind(), TokenKind::Semicolon) {
            self.bump();
        }
    }

    fn expr_to_stmt(&self, expr: Expr) -> Stmt {
        match expr {
            Expr::Binary {
                op: BinaryOp::Assign,
                left,
                right,
            } => Stmt::Assign {
                target: *left,
                value: *right,
            },
            other => Stmt::Expr(other),
        }
    }

    fn synchronize_item(&mut self) {
        while !self.is_eof() {
            match self.current_kind() {
                TokenKind::Semicolon
                | TokenKind::KwFn
                | TokenKind::KwObj
                | TokenKind::KwInterface
                | TokenKind::KwImpl
                | TokenKind::KwUse => {
                    if matches!(self.current_kind(), TokenKind::Semicolon) {
                        self.bump();
                    }
                    break;
                }
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn synchronize_statement(&mut self) {
        while !self.is_eof() {
            match self.current_kind() {
                TokenKind::Semicolon | TokenKind::RBrace => {
                    if matches!(self.current_kind(), TokenKind::Semicolon) {
                        self.bump();
                    }
                    break;
                }
                TokenKind::KwConst
                | TokenKind::KwLet
                | TokenKind::KwVar
                | TokenKind::KwReturn
                | TokenKind::KwThrow
                | TokenKind::KwPanic
                | TokenKind::KwLoop => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn synchronize_field(&mut self) {
        while !self.is_eof() {
            match self.current_kind() {
                TokenKind::Semicolon | TokenKind::RBrace => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn synchronize_params(&mut self) {
        while !self.is_eof() {
            match self.current_kind() {
                TokenKind::Comma | TokenKind::RParen => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn synchronize_type_arguments(&mut self) {
        while !self.is_eof() {
            match self.current_kind() {
                TokenKind::Comma | TokenKind::Gt => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn current_kind(&self) -> &TokenKind {
        &self.current_token().kind
    }

    fn current_token(&self) -> &Token {
        self.tokens
            .get(self.cursor)
            .unwrap_or_else(|| self.tokens.last().unwrap())
    }

    fn current_span(&self) -> Span {
        self.current_token().span.clone()
    }

    fn previous_span(&self) -> Span {
        if self.cursor == 0 {
            return Span { start: 0, end: 0 };
        }
        self.tokens[self.cursor - 1].span.clone()
    }

    fn bump(&mut self) -> Token {
        let token = self.current_token().clone();
        if self.cursor + 1 < self.tokens.len() {
            self.cursor += 1;
        } else {
            self.cursor = self.tokens.len() - 1;
        }
        token
    }

    fn push_error(&mut self, error: ParseError) {
        self.errors.push(error);
        if !self.is_eof() {
            self.cursor = (self.cursor + 1).min(self.tokens.len() - 1);
        }
    }

    fn is_eof(&self) -> bool {
        matches!(self.current_kind(), TokenKind::Eof)
    }
}

const PREC_PREFIX: u8 = 40;
const PREC_PRODUCT: u8 = 35;
const PREC_SUM: u8 = 30;
const PREC_COMPARISON: u8 = 25;
const PREC_EQUALITY: u8 = 20;
const PREC_ASSIGN: u8 = 10;

fn float_suffix_bits(kind: TypeIdentKind) -> Option<u32> {
    match kind {
        TypeIdentKind::F32 => Some(32),
        TypeIdentKind::F64 => Some(64),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ok(src: &str) -> ParseOutput {
        parse_source(src)
    }

    #[test]
    fn parse_empty_module() {
        let output = parse_ok("");
        assert!(output.errors.is_empty());
        assert!(output.items.is_empty());
    }

    #[test]
    fn parse_simple_function() {
        let output = parse_ok(
            "fn main() {\n                let answer = 40 + 2;\n                return;\n            }",
        );
        assert!(output.errors.is_empty());
        assert_eq!(output.items.len(), 1);
    }

    #[test]
    fn parse_let_expression() {
        let output = parse_ok("fn foo() { let x = 1 + 2; }");
        assert!(output.errors.is_empty());
    }

    #[test]
    fn parse_for_loop() {
        let output = parse_ok("fn main() { for (let i = 0; i < 3; i += 1) { return; } }");
        assert!(
            output.errors.is_empty(),
            "parse errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn parse_while_break_continue() {
        let output =
            parse_ok("fn main() { var x: int = 0; while x < 10 { x += 1; continue; break; } }");
        assert!(
            output.errors.is_empty(),
            "parse errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn parse_compound_assignment_expression() {
        let output = parse_ok("fn demo() { var x: int = 1; x *= 3; x -= 2; }");
        assert!(
            output.errors.is_empty(),
            "parse errors: {:?}",
            output.errors
        );
    }
}
