use super::*;
use std::str::FromStr;

impl Parser {
    pub(super) fn parse_expression(&mut self) -> Option<Expr> {
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

                if matches!(self.current_kind(), TokenKind::RParen) {
                    self.bump();
                    Expr::Tuple(Vec::new())
                } else {
                    let first = self.parse_expression()?;

                    if matches!(self.current_kind(), TokenKind::Comma) {
                        let mut items = vec![first];

                        loop {
                            if !matches!(self.current_kind(), TokenKind::Comma) {
                                break;
                            }

                            self.bump();

                            if matches!(self.current_kind(), TokenKind::RParen) {
                                break;
                            }

                            let expr = self.parse_expression()?;
                            items.push(expr);
                        }

                        self.expect_token(TokenKind::RParen, "`)` to close tuple");

                        Expr::Tuple(items)
                    } else {
                        self.expect_token(TokenKind::RParen, "`)` to close grouping");
                        first
                    }
                }
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
            TokenKind::LBracket => self.parse_bracket_literal()?,
            TokenKind::LBrace => self.parse_map_literal()?,
            _ => {
                self.push_error(
                    ParseError::new("expected expression", self.current_span())
                        .with_expected("literal or identifier")
                        .with_code(ParseErrorCode::ExpectedExpression),
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

    fn parse_bracket_literal(&mut self) -> Option<Expr> {
        let open = self.expect_token(TokenKind::LBracket, "`[` to start literal")?;
        let start = open.span.start;

        if matches!(self.current_kind(), TokenKind::RBracket) {
            let close = self.bump();

            return Some(Expr::Array(ArrayLiteral {
                elements: Vec::new(),
                span: Some(Span {
                    start,
                    end: close.span.end,
                }),
            }));
        }

        let first = self.parse_expression()?;

        match self.current_kind() {
            TokenKind::RBracket => {
                let close = self.bump();

                Some(Expr::Array(ArrayLiteral {
                    elements: vec![first],
                    span: Some(Span {
                        start,
                        end: close.span.end,
                    }),
                }))
            }
            TokenKind::Comma => {
                self.bump();

                if matches!(self.current_kind(), TokenKind::RBracket) {
                    let close = self.bump();

                    return Some(Expr::Array(ArrayLiteral {
                        elements: vec![first],
                        span: Some(Span {
                            start,
                            end: close.span.end,
                        }),
                    }));
                }

                let second = self.parse_expression()?;

                match self.current_kind() {
                    TokenKind::RParen => {
                        self.bump();

                        Some(Expr::Range {
                            start: Box::new(first),
                            end: Box::new(second),
                            inclusive_end: false,
                        })
                    }
                    TokenKind::RBracket => {
                        self.bump();

                        Some(Expr::Range {
                            start: Box::new(first),
                            end: Box::new(second),
                            inclusive_end: true,
                        })
                    }
                    TokenKind::Comma => {
                        let mut elements = vec![first, second];

                        loop {
                            self.bump();

                            if matches!(self.current_kind(), TokenKind::RBracket) {
                                let close = self.bump();

                                return Some(Expr::Array(ArrayLiteral {
                                    elements,
                                    span: Some(Span {
                                        start,
                                        end: close.span.end,
                                    }),
                                }));
                            }

                            let expr = match self.parse_expression() {
                                Some(expr) => expr,
                                None => return None,
                            };

                            elements.push(expr);

                            match self.current_kind() {
                                TokenKind::Comma => continue,
                                TokenKind::RBracket => {
                                    let close = self.bump();

                                    return Some(Expr::Array(ArrayLiteral {
                                        elements,
                                        span: Some(Span {
                                            start,
                                            end: close.span.end,
                                        }),
                                    }));
                                }
                                _ => {
                                    self.push_error(
                                        ParseError::new(
                                            "expected `,` or `]` after array element",
                                            self.current_span(),
                                        )
                                        .with_expected(",` or `]")
                                        .with_code(ParseErrorCode::ExpectedArrayElement),
                                    );

                                    return None;
                                }
                            }
                        }
                    }
                    _ => {
                        self.push_error(
                            ParseError::new(
                                "expected `]`, `)` or `,` after bracket literal element",
                                self.current_span(),
                            )
                            .with_expected("]`, `)` or `,")
                            .with_code(ParseErrorCode::ExpectedArrayElement),
                        );

                        None
                    }
                }
            }
            _ => {
                self.push_error(
                    ParseError::new(
                        "expected `]` or `,` after bracket literal element",
                        self.current_span(),
                    )
                    .with_expected("]` or `,")
                    .with_code(ParseErrorCode::ExpectedArrayElement),
                );

                None
            }
        }
    }

    fn parse_map_literal(&mut self) -> Option<Expr> {
        let open = self.expect_token(TokenKind::LBrace, "`{` to start map literal")?;
        let start = open.span.start;

        if matches!(self.current_kind(), TokenKind::RBrace) {
            let close = self.bump();

            return Some(Expr::Map(MapLiteral {
                entries: Vec::new(),
                span: Some(Span {
                    start,
                    end: close.span.end,
                }),
            }));
        }

        let mut entries = Vec::new();

        let close = loop {
            let key = self.parse_expression()?;

            self.expect_token(TokenKind::Colon, "`:` between map key and value")?;

            let value = self.parse_expression()?;

            entries.push(MapEntry { key, value });

            match self.current_kind() {
                TokenKind::Comma => {
                    self.bump();

                    if matches!(self.current_kind(), TokenKind::RBrace) {
                        break self.bump();
                    }
                }
                TokenKind::RBrace => break self.bump(),
                _ => {
                    self.push_error(
                        ParseError::new("expected `,` or `}` after map entry", self.current_span())
                            .with_expected(",` or `}`")
                            .with_code(ParseErrorCode::ExpectedMapEntry),
                    );

                    return None;
                }
            }
        };

        Some(Expr::Map(MapLiteral {
            entries,
            span: Some(Span {
                start,
                end: close.span.end,
            }),
        }))
    }

    fn parse_int_literal(&mut self) -> Option<Expr> {
        let token = self.bump();

        if let TokenKind::IntLit { value, ty } = token.kind.clone() {
            match i128::from_str(&value) {
                Ok(parsed) => Some(Expr::IntLit { value: parsed, ty }),
                Err(_) => {
                    self.push_error(
                        ParseError::new("integer literal out of range", token.span)
                            .with_found(TokenKind::IntLit { value, ty })
                            .with_code(ParseErrorCode::InvalidIntegerLiteral),
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
                            .with_found(TokenKind::FloatLit { value, ty })
                            .with_code(ParseErrorCode::InvalidFloatLiteral),
                    );

                    None
                }
            }
        } else {
            None
        }
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
