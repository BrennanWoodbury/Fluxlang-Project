use core::ast::nodes::Expr;
use core::types::TypeId;

#[derive(Debug, Clone, PartialEq)]
pub enum IRExpr {
    Literal(IRLiteral),
    Ast(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRLiteral {
    Int { value: i128, ty: Option<TypeId> },
    Float { value: f64, ty: Option<TypeId> },
    String { value: String, ty: Option<TypeId> },
    Array(IRArray),
    Map(IRMap),
    Range(IRRange),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRArray {
    pub elements: Vec<IRExpr>,
    pub element_type: Option<TypeId>,
    pub type_hint: Option<TypeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRMap {
    pub entries: Vec<(IRExpr, IRExpr)>,
    pub key_type: Option<TypeId>,
    pub value_type: Option<TypeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRRange {
    pub start: Box<IRExpr>,
    pub end: Box<IRExpr>,
    pub inclusive: bool,
    pub type_hint: Option<TypeId>,
}
