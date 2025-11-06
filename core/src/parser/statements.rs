use super::*;

impl Parser {
    pub(super) fn parse_block(&mut self) -> Option<Block> {
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

    pub(super) fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.current_kind() {
            TokenKind::KwConst => self.parse_const_stmt(),
            TokenKind::KwLet => self.parse_let_stmt(),
            TokenKind::KwVar => self.parse_var_stmt(),
            TokenKind::KwReturn => self.parse_return_stmt(),
            TokenKind::KwThrow => self.parse_throw_stmt(),
            TokenKind::KwPanic => self.parse_panic_stmt(),
            TokenKind::KwFor => self.parse_for_stmt(),
            TokenKind::KwWhile => self.parse_while_stmt(),
            TokenKind::KwIf => self.parse_if_stmt(),
            TokenKind::KwBreak => self.parse_break_stmt(),
            TokenKind::KwContinue => self.parse_continue_stmt(),
            TokenKind::KwLoop => self.parse_loop_stmt(),
            TokenKind::LBrace => self.parse_block().map(Stmt::Block),
            _ => self.parse_expr_stmt(),
        }
    }

    pub(super) fn parse_const_stmt(&mut self) -> Option<Stmt> {
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

    pub(super) fn parse_let_stmt(&mut self) -> Option<Stmt> {
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

    pub(super) fn parse_var_stmt(&mut self) -> Option<Stmt> {
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

    pub(super) fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.bump();

        let value = if matches!(self.current_kind(), TokenKind::Semicolon) {
            None
        } else {
            self.parse_expression()
        };

        self.expect_token(TokenKind::Semicolon, "`;` after return statement");

        Some(Stmt::Return(value))
    }

    pub(super) fn parse_throw_stmt(&mut self) -> Option<Stmt> {
        self.bump();

        let value = self.parse_expression()?;

        self.expect_token(TokenKind::Semicolon, "`;` after throw statement");

        Some(Stmt::Throw(value))
    }

    pub(super) fn parse_panic_stmt(&mut self) -> Option<Stmt> {
        self.bump();

        let value = if matches!(self.current_kind(), TokenKind::Semicolon) {
            None
        } else {
            self.parse_expression()
        };

        self.expect_token(TokenKind::Semicolon, "`;` after panic statement");

        Some(Stmt::Panic(value))
    }

    pub(super) fn parse_while_stmt(&mut self) -> Option<Stmt> {
        self.bump();

        let cond = self.parse_expression()?;

        let body = self.parse_block()?;

        Some(Stmt::While { cond, body })
    }

    pub(super) fn parse_break_stmt(&mut self) -> Option<Stmt> {
        self.bump();

        self.expect_token(TokenKind::Semicolon, "`;` after break statement");

        Some(Stmt::Break)
    }

    pub(super) fn parse_continue_stmt(&mut self) -> Option<Stmt> {
        self.bump();

        self.expect_token(TokenKind::Semicolon, "`;` after continue statement");

        Some(Stmt::Continue)
    }

    pub(super) fn parse_for_stmt(&mut self) -> Option<Stmt> {
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

    pub(super) fn parse_loop_stmt(&mut self) -> Option<Stmt> {
        self.bump();

        let body = self.parse_block()?;

        Some(Stmt::Loop(body))
    }

    pub(super) fn parse_if_stmt(&mut self) -> Option<Stmt> {
        self.bump();

        self.expect_token(TokenKind::LParen, "`(` after `if`")?;

        let cond = self.parse_expression()?;

        self.expect_token(TokenKind::RParen, "`)` after `if` condition");

        let then_branch = self.parse_block()?;

        let else_branch = if matches!(self.current_kind(), TokenKind::KwElse) {
            self.bump();

            if matches!(self.current_kind(), TokenKind::KwIf) {
                Some(Box::new(self.parse_if_stmt()?))
            } else {
                Some(Box::new(Stmt::Block(self.parse_block()?)))
            }
        } else {
            None
        };

        Some(Stmt::If(Box::new(IfStmt {
            cond,
            then_branch,
            else_branch,
        })))
    }

    pub(super) fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression()?;

        self.expect_token(TokenKind::Semicolon, "`;` after expression statement");

        Some(self.expr_to_stmt(expr))
    }

    pub(super) fn parse_for_initializer(&mut self) -> Option<Stmt> {
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
}
