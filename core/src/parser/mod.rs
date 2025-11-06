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

mod expressions;
mod items;
mod recovery;
mod statements;
mod support;
mod types;

use crate::ast::nodes::{
    ArrayLiteral, ArrayType, Block, ConstDecl, Expr, FieldDecl, FnDecl, FnSig, ForStmt,
    GenericParams, Ident, IfStmt, ImplBlock, InterfaceDecl, Item, LetDecl, MapEntry, MapLiteral,
    MapType, ObjDecl, Param, Stmt, TypeExpr, UseDecl, VarDecl, Visibility,
};
use crate::ast::{BinaryOp, TokenKind, TypeIdentKind, UnaryOp};
use crate::diag::{ParseErrorCode, Span};
use lexer::{LexError, Lexer, Token};

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

#[derive(Debug, Default)]
pub struct ParseOutput {
    pub items: Vec<Item>,
    pub errors: Vec<ParseError>,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
    pub expected: Vec<String>,
    pub found: Option<TokenKind>,
    pub code: ParseErrorCode,
}

impl ParseError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            expected: Vec::new(),
            found: None,
            code: ParseErrorCode::UnexpectedToken,
        }
    }

    fn with_code(mut self, code: ParseErrorCode) -> Self {
        self.code = code;
        self
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
            code: ParseErrorCode::UnexpectedToken,
        }
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

    #[test]
    fn parse_if_else_chain() {
        let source =
            "fn main() { if (ready) { return; } else if (fallback) { return; } else { panic; } }";

        let output = parse_ok(source);

        assert!(
            output.errors.is_empty(),
            "parse errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn parse_tuple_and_range_literals() {
        let source = "fn demo() { let t = (1, 2, 3); let single = (value,); let unit = (); let span = [0, 10); let inclusive = [0, 10]; }";

        let output = parse_ok(source);

        assert!(
            output.errors.is_empty(),
            "parse errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn parse_array_and_map_literals() {
        let source = "fn literals() { let empty = []; let single = [value]; let pair = [left, right,]; let list = [1, 2, 3]; let mapping = { key: 1, other: 2, }; }";

        let output = parse_ok(source);

        assert!(
            output.errors.is_empty(),
            "parse errors: {:?}",
            output.errors
        );
    }
}
