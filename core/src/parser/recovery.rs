use super::*;

impl Parser {
    pub(super) fn synchronize_item(&mut self) {
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

    pub(super) fn synchronize_statement(&mut self) {
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
                | TokenKind::KwLoop
                | TokenKind::KwIf
                | TokenKind::KwElse
                | TokenKind::KwFor
                | TokenKind::KwWhile
                | TokenKind::KwBreak
                | TokenKind::KwContinue => break,

                _ => {
                    self.bump();
                }
            }
        }
    }

    pub(super) fn synchronize_field(&mut self) {
        while !self.is_eof() {
            match self.current_kind() {
                TokenKind::Semicolon | TokenKind::RBrace => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    pub(super) fn synchronize_params(&mut self) {
        while !self.is_eof() {
            match self.current_kind() {
                TokenKind::Comma | TokenKind::RParen => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    pub(super) fn synchronize_type_arguments(&mut self) {
        while !self.is_eof() {
            match self.current_kind() {
                TokenKind::Comma | TokenKind::Gt => break,
                _ => {
                    self.bump();
                }
            }
        }
    }
}
