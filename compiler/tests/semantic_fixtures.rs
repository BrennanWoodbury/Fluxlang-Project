use compiler::semantics::lowering::LoweringContext;
use core::parser::parse_source;
use std::fs;
use std::path::{Path, PathBuf};

#[test]
fn semantic_fixture_expectations() {
    let fixtures_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("semantic");

    let mut fixtures = collect_fixture_files(&fixtures_dir);
    fixtures.sort();

    for fixture in fixtures {
        let source = fs::read_to_string(&fixture)
            .unwrap_or_else(|err| panic!("failed to read {:?}: {}", fixture, err));

        let parse_output = parse_source(&source);
        let mut reports = Vec::new();

        for error in parse_output.errors {
            reports.push(format!(
                "parse({:?}) {}..{} {}",
                error.code, error.span.start, error.span.end, error.message
            ));
        }

        let lowering = LoweringContext::lower(&parse_output.items, None);
        for diag in lowering.diagnostics {
            reports.push(format!(
                "semantic({:?}) {}..{} {}",
                diag.code, diag.span.start, diag.span.end, diag.message
            ));
        }

        let actual = if reports.is_empty() {
            "<no diagnostics>\n".to_string()
        } else {
            let mut output = reports.join("\n");
            output.push('\n');
            output
        };

        let expected_path = fixture.with_extension("expected");
        let expected = fs::read_to_string(&expected_path).unwrap_or_else(|_| {
            panic!(
                "missing expected file for fixture {:?}. create {}",
                fixture,
                expected_path.display()
            )
        });
        let expected = normalize_line_endings(expected);

        assert_eq!(actual, expected, "diagnostic mismatch for {:?}", fixture);
    }
}

fn collect_fixture_files(dir: &Path) -> Vec<PathBuf> {
    let mut fixtures = Vec::new();
    if !dir.exists() {
        return fixtures;
    }

    for entry in fs::read_dir(dir).expect("failed to read fixtures directory") {
        let entry = entry.expect("failed to read fixture entry");
        let path = entry.path();
        if path.extension().and_then(|ext| ext.to_str()) == Some("flux") {
            fixtures.push(path);
        }
    }

    fixtures
}

fn normalize_line_endings(text: String) -> String {
    text.replace("\r\n", "\n")
}
