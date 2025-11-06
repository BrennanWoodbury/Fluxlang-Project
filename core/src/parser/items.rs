use super::*;

impl Parser {
    pub(super) fn parse_items(&mut self) -> Vec<Item> {
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
                            .with_expected("item keyword (fn, obj, interface, impl, use)")
                            .with_code(ParseErrorCode::ExpectedItem),
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
                    .with_expected("identifier")
                    .with_code(ParseErrorCode::ExpectedUsePath),
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
                None => self.synchronize_item(),
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
                        .with_expected("fn")
                        .with_code(ParseErrorCode::ExpectedFunction),
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
}
