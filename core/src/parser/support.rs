use super::*;

impl Parser {
    pub(super) fn parse_ident(&mut self, context: &str) -> Option<Ident> {
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

    pub(super) fn expect_token(&mut self, kind: TokenKind, expected: &str) -> Option<Token> {
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

    pub(super) fn skip_trivia_tokens(&mut self) {
        while matches!(self.current_kind(), TokenKind::Semicolon) {
            self.bump();
        }
    }

    pub(super) fn current_kind(&self) -> &TokenKind {
        &self.current_token().kind
    }

    pub(super) fn current_token(&self) -> &Token {
        self.tokens
            .get(self.cursor)
            .unwrap_or_else(|| self.tokens.last().unwrap())
    }

    pub(super) fn current_span(&self) -> Span {
        self.current_token().span.clone()
    }

    pub(super) fn previous_span(&self) -> Span {
        if self.cursor == 0 {
            return Span { start: 0, end: 0 };
        }

        self.tokens[self.cursor - 1].span.clone()
    }

    pub(super) fn bump(&mut self) -> Token {
        let token = self.current_token().clone();

        if self.cursor + 1 < self.tokens.len() {
            self.cursor += 1;
        } else {
            self.cursor = self.tokens.len() - 1;
        }

        token
    }

    pub(super) fn push_error(&mut self, error: ParseError) {
        self.errors.push(error);

        if !self.is_eof() {
            self.cursor = (self.cursor + 1).min(self.tokens.len() - 1);
        }
    }

    pub(super) fn is_eof(&self) -> bool {
        matches!(self.current_kind(), TokenKind::Eof)
    }
}
