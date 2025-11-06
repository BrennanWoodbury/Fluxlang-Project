use super::*;

impl Parser {
    pub(super) fn parse_type_expr(&mut self) -> Option<TypeExpr> {
        let mut ty = self.parse_type_atom()?;

        ty = self.normalize_type_expr(ty);

        loop {
            let mut consumed_suffix = false;

            if matches!(self.current_kind(), TokenKind::LParen) {
                if let TypeExpr::Array(mut array) = ty {
                    self.bump();

                    if matches!(self.current_kind(), TokenKind::RParen) {
                        self.bump();

                        array.dynamic = true;
                        array.length = None;
                    } else {
                        let length_token = self.bump();

                        let length = match &length_token.kind {
                            TokenKind::IntLit { value, .. } => match value.parse::<u32>() {
                                Ok(value) => Some(value),
                                Err(_) => {
                                    self.push_error(
                                        ParseError::new(
                                            "array length must be a positive integer",
                                            length_token.span,
                                        )
                                        .with_found(length_token.kind.clone())
                                        .with_code(ParseErrorCode::InvalidArrayLength),
                                    );

                                    None
                                }
                            },
                            _ => {
                                self.push_error(
                                    ParseError::new(
                                        "array length must be a positive integer",
                                        length_token.span,
                                    )
                                    .with_found(length_token.kind.clone())
                                    .with_code(ParseErrorCode::InvalidArrayLength),
                                );

                                None
                            }
                        };

                        self.expect_token(TokenKind::RParen, "`)` to close array length")?;

                        array.dynamic = length.is_none();
                        array.length = length;
                    }

                    ty = TypeExpr::Array(array);
                    consumed_suffix = true;
                }
            }

            if !consumed_suffix {
                break;
            }
        }

        Some(ty)
    }

    fn parse_type_atom(&mut self) -> Option<TypeExpr> {
        match self.current_kind() {
            TokenKind::Ampersand => {
                self.bump();

                let inner = self.parse_type_expr()?;
                Some(TypeExpr::Pointer(Box::new(inner)))
            }
            TokenKind::LParen => {
                self.bump();

                if matches!(self.current_kind(), TokenKind::RParen) {
                    self.bump();
                    Some(TypeExpr::Tuple(Vec::new()))
                } else {
                    let first = self.parse_type_expr()?;

                    if matches!(self.current_kind(), TokenKind::Comma) {
                        let mut items = vec![first];

                        while matches!(self.current_kind(), TokenKind::Comma) {
                            self.bump();

                            let ty = self.parse_type_expr()?;
                            items.push(ty);
                        }

                        self.expect_token(TokenKind::RParen, "`)` to close tuple type")?;

                        Some(TypeExpr::Tuple(items))
                    } else {
                        self.expect_token(TokenKind::RParen, "`)` to close grouped type")?;
                        Some(first)
                    }
                }
            }
            TokenKind::LBracket => {
                self.bump();

                self.expect_token(TokenKind::RBracket, "`]` to close array type")?;

                Some(TypeExpr::Array(ArrayType {
                    element: None,
                    length: None,
                    dynamic: true,
                }))
            }
            TokenKind::LBrace => {
                self.bump();

                self.expect_token(TokenKind::RBrace, "`}` to close map type")?;

                Some(TypeExpr::Map(MapType {
                    key: None,
                    value: None,
                }))
            }
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

    fn normalize_type_expr(&mut self, ty: TypeExpr) -> TypeExpr {
        match ty {
            TypeExpr::Generic { base, args } => {
                let lowered = base.name.to_ascii_lowercase();

                match lowered.as_str() {
                    "tuple" => TypeExpr::Tuple(args),
                    "array" => {
                        let element = args.get(0).cloned().map(Box::new);

                        TypeExpr::Array(ArrayType {
                            element,
                            length: None,
                            dynamic: true,
                        })
                    }
                    "map" => {
                        let key = args.get(0).cloned().map(Box::new);
                        let value = args.get(1).cloned().map(Box::new);

                        TypeExpr::Map(MapType { key, value })
                    }
                    _ => TypeExpr::Generic { base, args },
                }
            }
            other => other,
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
}
