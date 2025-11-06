use crate::ir::{IRArray, IRExpr, IRLiteral, IRMap, IRRange};
use core::ast::nodes::{ArrayLiteral, Expr, MapEntry, MapLiteral};
use core::types::TypeId;

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayTypeHints {
    pub element_type: TypeId,
    pub array_type: TypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapTypeHints {
    pub key_type: TypeId,
    pub value_type: TypeId,
}

#[derive(Debug, Default)]
pub struct LiteralAnalyzer;

impl LiteralAnalyzer {
    pub fn lower_expr(&self, expr: &Expr) -> IRExpr {
        match self.lower_literal(expr) {
            Some(literal) => IRExpr::Literal(literal),
            None => IRExpr::Ast(expr.clone()),
        }
    }

    pub fn lower_literal(&self, expr: &Expr) -> Option<IRLiteral> {
        match expr {
            Expr::IntLit { value, .. } => Some(IRLiteral::Int {
                value: *value,
                ty: Some(TypeId::int()),
            }),
            Expr::FloatLit { value, .. } => Some(IRLiteral::Float {
                value: *value,
                ty: Some(TypeId::float()),
            }),
            Expr::StringLit { value, .. } => Some(IRLiteral::String {
                value: value.clone(),
                ty: Some(TypeId::string()),
            }),
            Expr::Array(array) => {
                let hints = self.infer_array_types(array);
                let elements = array
                    .elements
                    .iter()
                    .map(|element| self.lower_expr(element))
                    .collect();
                Some(IRLiteral::Array(IRArray {
                    elements,
                    element_type: Some(hints.element_type.clone()),
                    type_hint: Some(hints.array_type),
                }))
            }
            Expr::Map(map) => {
                let hints = self.infer_map_types(map);
                let entries = map
                    .entries
                    .iter()
                    .map(|entry| (self.lower_expr(&entry.key), self.lower_expr(&entry.value)))
                    .collect();
                Some(IRLiteral::Map(IRMap {
                    entries,
                    key_type: Some(hints.key_type),
                    value_type: Some(hints.value_type),
                }))
            }
            Expr::Range {
                start,
                end,
                inclusive_end,
            } => {
                let start_ir = self.lower_expr(start);
                let end_ir = self.lower_expr(end);
                Some(IRLiteral::Range(IRRange {
                    start: Box::new(start_ir),
                    end: Box::new(end_ir),
                    inclusive: *inclusive_end,
                    type_hint: Some(TypeId::range()),
                }))
            }
            _ => None,
        }
    }

    pub fn infer_expr_type(&self, expr: &Expr) -> Option<TypeId> {
        match expr {
            Expr::IntLit { .. } => Some(TypeId::int()),
            Expr::FloatLit { .. } => Some(TypeId::float()),
            Expr::StringLit { .. } => Some(TypeId::string()),
            Expr::Array(array) => {
                let hints = self.infer_array_types(array);
                Some(hints.array_type)
            }
            Expr::Map(map) => {
                let hints = self.infer_map_types(map);
                Some(TypeId::map_of(&hints.key_type, &hints.value_type))
            }
            Expr::Range { .. } => Some(TypeId::range()),
            _ => None,
        }
    }

    pub fn infer_array_types(&self, array: &ArrayLiteral) -> ArrayTypeHints {
        if array.elements.is_empty() {
            let element_type = TypeId::any();
            return ArrayTypeHints {
                array_type: TypeId::array_of(&element_type),
                element_type,
            };
        }

        let mut element_type: Option<TypeId> = None;
        for element in &array.elements {
            let candidate = self.infer_expr_type(element).unwrap_or_else(TypeId::any);
            element_type = Some(match element_type {
                None => candidate,
                Some(current) => self.unify_types(current, candidate),
            });
            if element_type
                .as_ref()
                .map(|ty| ty.name == "any")
                .unwrap_or(false)
            {
                break;
            }
        }

        let element_type = element_type.unwrap_or_else(TypeId::any);
        ArrayTypeHints {
            array_type: TypeId::array_of(&element_type),
            element_type,
        }
    }

    pub fn infer_map_types(&self, map: &MapLiteral) -> MapTypeHints {
        if map.entries.is_empty() {
            return MapTypeHints {
                key_type: TypeId::any(),
                value_type: TypeId::any(),
            };
        }

        let mut key_type: Option<TypeId> = None;
        let mut value_type: Option<TypeId> = None;
        for MapEntry { key, value } in &map.entries {
            let key_candidate = self.infer_expr_type(key).unwrap_or_else(TypeId::any);
            key_type = Some(match key_type {
                None => key_candidate,
                Some(current) => self.unify_types(current, key_candidate),
            });

            let value_candidate = self.infer_expr_type(value).unwrap_or_else(TypeId::any);
            value_type = Some(match value_type {
                None => value_candidate,
                Some(current) => self.unify_types(current, value_candidate),
            });
        }

        MapTypeHints {
            key_type: key_type.unwrap_or_else(TypeId::any),
            value_type: value_type.unwrap_or_else(TypeId::any),
        }
    }

    fn unify_types(&self, left: TypeId, right: TypeId) -> TypeId {
        if left == right {
            return left;
        }

        if self.is_numeric(&left) && self.is_numeric(&right) {
            if self.is_float(&left) || self.is_float(&right) {
                TypeId::float()
            } else {
                TypeId::int()
            }
        } else {
            TypeId::any()
        }
    }

    fn is_numeric(&self, ty: &TypeId) -> bool {
        self.is_int(ty) || self.is_float(ty)
    }

    fn is_int(&self, ty: &TypeId) -> bool {
        ty.name == "int"
    }

    fn is_float(&self, ty: &TypeId) -> bool {
        ty.name == "float"
    }
}
