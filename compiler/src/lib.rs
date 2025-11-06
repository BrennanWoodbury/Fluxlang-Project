pub mod ir;
pub mod semantics;

#[cfg(test)]
mod tests {
    use super::ir::{IRExpr, IRLiteral};
    use super::semantics::literals::LiteralAnalyzer;
    use core::ast::nodes::{ArrayLiteral, Expr, MapEntry, MapLiteral};
    use core::types::TypeId;

    fn int_expr(value: i128) -> Expr {
        Expr::IntLit { value, ty: None }
    }

    fn float_expr(value: f64) -> Expr {
        Expr::FloatLit { value, ty: None }
    }

    fn string_expr(value: &str) -> Expr {
        Expr::StringLit {
            value: value.to_string(),
            size: None,
        }
    }

    #[test]
    fn array_literal_promotes_numeric_types() {
        let analyzer = LiteralAnalyzer::default();
        let array_expr = Expr::Array(ArrayLiteral {
            elements: vec![int_expr(1), float_expr(2.0)],
            span: None,
        });

        match analyzer.lower_expr(&array_expr) {
            IRExpr::Literal(IRLiteral::Array(ir_array)) => {
                assert_eq!(ir_array.element_type, Some(TypeId::float()));
                assert_eq!(ir_array.type_hint, Some(TypeId::array_of(&TypeId::float())));
            }
            other => panic!("expected array literal, got {:?}", other),
        }
    }

    #[test]
    fn empty_map_defaults_to_any_any() {
        let analyzer = LiteralAnalyzer::default();
        let map_expr = Expr::Map(MapLiteral {
            entries: Vec::new(),
            span: None,
        });

        match analyzer.lower_expr(&map_expr) {
            IRExpr::Literal(IRLiteral::Map(ir_map)) => {
                assert_eq!(ir_map.key_type, Some(TypeId::any()));
                assert_eq!(ir_map.value_type, Some(TypeId::any()));
            }
            other => panic!("expected map literal, got {:?}", other),
        }
    }

    #[test]
    fn map_literal_infers_key_and_value_types() {
        let analyzer = LiteralAnalyzer::default();
        let map_expr = Expr::Map(MapLiteral {
            entries: vec![MapEntry {
                key: string_expr("id"),
                value: int_expr(42),
            }],
            span: None,
        });

        match analyzer.lower_expr(&map_expr) {
            IRExpr::Literal(IRLiteral::Map(ir_map)) => {
                assert_eq!(ir_map.key_type, Some(TypeId::string()));
                assert_eq!(ir_map.value_type, Some(TypeId::int()));
            }
            other => panic!("expected map literal, got {:?}", other),
        }
    }
}
