//! Source lexer for FluxLang.
//!
//! Converts raw source text into a stream of `TokenKind` values enriched with
//! `Span` information. The lexer is designed as a simple pull-based iterator so
//! the upcoming parser can consume tokens on demand.

use crate::ast::{TokenKind, TypeIdentKind};
use crate::diag::Span;
use std::fmt;

/// A token paired with its location in the source file.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    fn new(kind: TokenKind, start: usize, end: usize) -> Self {
        Self {
            kind,
            span: Span { start, end },
        }
    }
}

/// Errors that can occur while lexing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} (at {}..{})",
            self.message, self.span.start, self.span.end
        )
    }
}

impl std::error::Error for LexError {}

/// Convenient alias for lexer results.
pub type LexResult<T> = Result<T, LexError>;

/// Pull-based lexer producing tokens from a source string.
pub struct Lexer<'a> {
    source: &'a str,
    cursor: usize,
    line: usize,
    column: usize,
    finished: bool,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer positioned at the start of `source`.
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            cursor: 0,
            line: 1,
            column: 1,
            finished: false,
        }
    }

    /// Pull the next token from the stream.
    pub fn next_token(&mut self) -> LexResult<Token> {
        if self.finished {
            return Ok(Token::new(TokenKind::Eof, self.cursor, self.cursor));
        }

        self.skip_trivia()?;

        let start = self.cursor;
        match self.peek_char() {
            None => {
                self.finished = true;
                Ok(Token::new(TokenKind::Eof, start, start))
            }
            Some(ch) if is_ident_start(ch) => self.lex_identifier_or_keyword(),
            Some(ch) if ch.is_ascii_digit() => self.lex_number(),
            Some('"') => self.lex_string(),
            Some(_) => self.lex_symbol(),
        }
    }

    /// Consume whitespace and comments.
    fn skip_trivia(&mut self) -> LexResult<()> {
        loop {
            match self.peek_char() {
                Some(ch) if ch.is_whitespace() => {
                    self.bump_char();
                }
                Some('/') => {
                    if self.peek_next_char() == Some('/') {
                        self.bump_char();
                        self.bump_char();
                        while let Some(ch) = self.peek_char() {
                            self.bump_char();
                            if ch == '\n' {
                                break;
                            }
                        }
                    } else if self.peek_next_char() == Some('*') {
                        self.bump_char();
                        self.bump_char();
                        self.skip_block_comment()?;
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        Ok(())
    }

    fn skip_block_comment(&mut self) -> LexResult<()> {
        let start = self.cursor.saturating_sub(2);
        while let Some(ch) = self.peek_char() {
            if ch == '*' && self.peek_next_char() == Some('/') {
                self.bump_char();
                self.bump_char();
                return Ok(());
            }
            self.bump_char();
        }
        Err(LexError {
            message: "unterminated block comment".into(),
            span: Span {
                start,
                end: self.cursor,
            },
        })
    }

    fn lex_identifier_or_keyword(&mut self) -> LexResult<Token> {
        let start = self.cursor;
        let mut ident = String::new();
        while let Some(ch) = self.peek_char() {
            if is_ident_continue(ch) {
                ident.push(ch);
                self.bump_char();
            } else {
                break;
            }
        }

        let kind = match ident.as_str() {
            "obj" => TokenKind::KwObj,
            "interface" => TokenKind::KwInterface,
            "impl" => TokenKind::KwImpl,
            "pub" => TokenKind::KwPub,
            "use" => TokenKind::KwUse,
            "as" => TokenKind::KwAs,
            "for" => TokenKind::KwFor,
            "return" => TokenKind::KwReturn,
            "throw" => TokenKind::KwThrow,
            "panic" => TokenKind::KwPanic,
            "loop" => TokenKind::KwLoop,
            "fn" => TokenKind::KwFn,
            "const" => TokenKind::KwConst,
            "let" => TokenKind::KwLet,
            "var" => TokenKind::KwVar,
            "int" => TokenKind::TypeIdent(TypeIdentKind::Int),
            "uint" => TokenKind::TypeIdent(TypeIdentKind::UInt),
            "float" => TokenKind::TypeIdent(TypeIdentKind::Float),
            "string" => TokenKind::TypeIdent(TypeIdentKind::String),
            _ => {
                if let Some(kind) = parse_builtin_type(&ident) {
                    TokenKind::TypeIdent(kind)
                } else {
                    TokenKind::Ident(ident)
                }
            }
        };

        Ok(Token::new(kind, start, self.cursor))
    }

    fn lex_number(&mut self) -> LexResult<Token> {
        let start = self.cursor;
        let mut literal = String::new();
        let mut is_float = false;

        consume_digits(self, &mut literal);

        if self.peek_char() == Some('.')
            && self
                .peek_next_char()
                .map(|c| c.is_ascii_digit())
                .unwrap_or(false)
        {
            is_float = true;
            literal.push('.');
            self.bump_char();
            consume_digits(self, &mut literal);
        }

        if let Some(ch) = self.peek_char() {
            if ch == 'e' || ch == 'E' {
                is_float = true;
                literal.push(ch);
                self.bump_char();
                if let Some(sign @ ('+' | '-')) = self.peek_char() {
                    literal.push(sign);
                    self.bump_char();
                }
                if consume_digits(self, &mut literal) == 0 {
                    return Err(LexError {
                        message: "expected digits after exponent".into(),
                        span: Span {
                            start,
                            end: self.cursor,
                        },
                    });
                }
            }
        }

        let suffix_start = self.cursor;
        let mut suffix = String::new();
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_alphanumeric() {
                suffix.push(ch);
                self.bump_char();
            } else {
                break;
            }
        }

        let ty = if suffix.is_empty() {
            None
        } else {
            match parse_builtin_type(&suffix) {
                Some(kind) => Some(kind),
                None => {
                    return Err(LexError {
                        message: format!("unknown numeric suffix `{}`", suffix),
                        span: Span {
                            start: suffix_start,
                            end: self.cursor,
                        },
                    });
                }
            }
        };

        let kind = if is_float {
            TokenKind::FloatLit { value: literal, ty }
        } else {
            TokenKind::IntLit { value: literal, ty }
        };

        Ok(Token::new(kind, start, self.cursor))
    }

    fn lex_string(&mut self) -> LexResult<Token> {
        let start = self.cursor;
        self.bump_char(); // opening quote

        let mut value = String::new();
        while let Some(ch) = self.peek_char() {
            match ch {
                '\\' => {
                    self.bump_char();
                    match self.peek_char() {
                        Some('n') => {
                            value.push('\n');
                            self.bump_char();
                        }
                        Some('r') => {
                            value.push('\r');
                            self.bump_char();
                        }
                        Some('t') => {
                            value.push('\t');
                            self.bump_char();
                        }
                        Some('"') => {
                            value.push('"');
                            self.bump_char();
                        }
                        Some('\\') => {
                            value.push('\\');
                            self.bump_char();
                        }
                        Some(other) => {
                            value.push(other);
                            self.bump_char();
                        }
                        None => {
                            return Err(LexError {
                                message: "unterminated escape sequence".into(),
                                span: Span {
                                    start,
                                    end: self.cursor,
                                },
                            });
                        }
                    }
                }
                '"' => {
                    self.bump_char();
                    break;
                }
                '\n' => {
                    return Err(LexError {
                        message: "unterminated string literal".into(),
                        span: Span {
                            start,
                            end: self.cursor,
                        },
                    });
                }
                other => {
                    value.push(other);
                    self.bump_char();
                }
            }
        }

        let mut size_suffix = None;
        // Optional fixed-length suffix syntax: "literal":32
        if self.peek_char() == Some(':') {
            self.bump_char();
            let suffix_start = self.cursor;
            let mut digits = String::new();
            while let Some(ch) = self.peek_char() {
                if ch.is_ascii_digit() {
                    digits.push(ch);
                    self.bump_char();
                } else {
                    break;
                }
            }
            if digits.is_empty() {
                return Err(LexError {
                    message: "expected digits after string size suffix".into(),
                    span: Span {
                        start: suffix_start,
                        end: self.cursor,
                    },
                });
            }
            size_suffix = match digits.parse::<u32>() {
                Ok(value) => Some(value),
                Err(_) => {
                    return Err(LexError {
                        message: "string size suffix out of range".into(),
                        span: Span {
                            start: suffix_start,
                            end: self.cursor,
                        },
                    });
                }
            };
        }

        Ok(Token::new(
            TokenKind::StringLit { value, size_suffix },
            start,
            self.cursor,
        ))
    }

    fn lex_symbol(&mut self) -> LexResult<Token> {
        let start = self.cursor;
        let ch = self.bump_char().expect("lexer invariant");

        let kind = match ch {
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            ':' => {
                if self.peek_char() == Some(':') {
                    self.bump_char();
                    TokenKind::ColonColon
                } else {
                    TokenKind::Colon
                }
            }
            '-' => {
                if self.peek_char() == Some('>') {
                    self.bump_char();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '+' => TokenKind::Plus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '%' => TokenKind::Percent,
            '!' => {
                if self.peek_char() == Some('=') {
                    self.bump_char();
                    TokenKind::NotEq
                } else {
                    TokenKind::Bang
                }
            }
            '=' => {
                if self.peek_char() == Some('=') {
                    self.bump_char();
                    if self.peek_char() == Some('=') {
                        self.bump_char();
                        TokenKind::EqEqEq
                    } else {
                        TokenKind::EqEq
                    }
                } else {
                    TokenKind::Assign
                }
            }
            '<' => {
                if self.peek_char() == Some('=') {
                    self.bump_char();
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }
            '>' => {
                if self.peek_char() == Some('=') {
                    self.bump_char();
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }
            other => {
                return Err(LexError {
                    message: format!("unexpected character `{}`", other),
                    span: Span {
                        start,
                        end: self.cursor,
                    },
                });
            }
        };

        Ok(Token::new(kind, start, self.cursor))
    }

    fn peek_char(&self) -> Option<char> {
        self.source[self.cursor..].chars().next()
    }

    fn peek_next_char(&self) -> Option<char> {
        let mut chars = self.source[self.cursor..].chars();
        chars.next()?;
        chars.next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.cursor += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(ch)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        match self.next_token() {
            Ok(token) => {
                if token.kind == TokenKind::Eof {
                    self.finished = true;
                    None
                } else {
                    Some(Ok(token))
                }
            }
            Err(err) => {
                self.finished = true;
                Some(Err(err))
            }
        }
    }
}

fn parse_builtin_type(text: &str) -> Option<TypeIdentKind> {
    match text {
        "i8" => Some(TypeIdentKind::I8),
        "i16" => Some(TypeIdentKind::I16),
        "i32" => Some(TypeIdentKind::I32),
        "i64" => Some(TypeIdentKind::I64),
        "i128" => Some(TypeIdentKind::I128),
        "u8" => Some(TypeIdentKind::U8),
        "u16" => Some(TypeIdentKind::U16),
        "u32" => Some(TypeIdentKind::U32),
        "u64" => Some(TypeIdentKind::U64),
        "u128" => Some(TypeIdentKind::U128),
        "f32" => Some(TypeIdentKind::F32),
        "f64" => Some(TypeIdentKind::F64),
        "int" => Some(TypeIdentKind::Int),
        "uint" => Some(TypeIdentKind::UInt),
        "float" => Some(TypeIdentKind::Float),
        "string" => Some(TypeIdentKind::String),
        _ => None,
    }
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

fn is_ident_continue(ch: char) -> bool {
    is_ident_start(ch) || ch.is_ascii_digit()
}

/// Read a run of decimal digits, ignoring embedded underscores.
fn consume_digits(lexer: &mut Lexer<'_>, buf: &mut String) -> usize {
    let mut count = 0;
    while let Some(ch) = lexer.peek_char() {
        if ch.is_ascii_digit() {
            buf.push(ch);
            lexer.bump_char();
            count += 1;
        } else if ch == '_' {
            lexer.bump_char();
        } else {
            break;
        }
    }
    count
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_kinds(src: &str) -> Vec<TokenKind> {
        let mut lexer = Lexer::new(src);
        let mut kinds = Vec::new();
        loop {
            match lexer.next_token() {
                Ok(token) => {
                    if token.kind == TokenKind::Eof {
                        break;
                    }
                    kinds.push(token.kind);
                }
                Err(err) => panic!("{}", err),
            }
        }
        kinds
    }

    #[test]
    fn lex_let_statement() {
        let kinds = collect_kinds("let x = 42;");
        assert_eq!(
            kinds,
            vec![
                TokenKind::KwLet,
                TokenKind::Ident("x".into()),
                TokenKind::Assign,
                TokenKind::IntLit {
                    value: "42".into(),
                    ty: None
                },
                TokenKind::Semicolon
            ]
        );
    }

    #[test]
    fn lex_float_literal_with_suffix() {
        let mut lexer = Lexer::new("3.14f32");
        let token = lexer.next_token().unwrap();
        assert_eq!(
            token.kind,
            TokenKind::FloatLit {
                value: "3.14".into(),
                ty: Some(TypeIdentKind::F32)
            }
        );
    }

    #[test]
    fn lex_string_with_escape_and_suffix() {
        let mut lexer = Lexer::new("\"hi\\n\":8");
        let token = lexer.next_token().unwrap();
        assert_eq!(
            token.kind,
            TokenKind::StringLit {
                value: "hi\n".into(),
                size_suffix: Some(8)
            }
        );
    }

    #[test]
    fn unterminated_comment_errors() {
        let mut lexer = Lexer::new("/* oops");
        let err = lexer.next_token().expect_err("expected error");
        assert_eq!(err.message, "unterminated block comment");
    }
}
