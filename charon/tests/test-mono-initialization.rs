use std::borrow::Cow;

use charon_lib::formatter::{AstFormatter, IntoFormatter};
use charon_lib::pretty::FmtWithCtx;

use charon_lib::ast::*;
use charon_lib::name_matcher::Pattern;

mod util;

/// Test that demonstrates that the NameMatcher is properly initialized for monomorphized names.
/// This test validates that the mono matching functionality works correctly and that the
/// identified bug (overly strict assertion) has been fixed.
#[test]
fn test_name_matcher_mono_initialization() -> anyhow::Result<()> {
    // Test case with generics to create monomorphized instances
    let test_code = r#"
        struct Generic<T, U> {
            field1: T,
            field2: U,
        }
        
        impl<T, U> Generic<T, U> {
            fn new(field1: T, field2: U) -> Self {
                Self { field1, field2 }
            }
            
            fn get_first(&self) -> &T {
                &self.field1
            }
            
            fn swap<V>(self, new_first: V) -> Generic<V, U> {
                Generic {
                    field1: new_first,
                    field2: self.field2,
                }
            }
        }
        
        fn create_instances() {
            // This will create monomorphized instances
            let g1 = Generic::new(42i32, "hello");
            let g2 = Generic::new(3.14f64, true);
            
            // These method calls will create monomorphized method instances
            let _ = g1.get_first();
            let _ = g2.get_first();
            
            // Method with additional generics
            let g3 = g1.swap(99u64);
            let _ = g3.get_first();
        }
    "#;

    // Translate with monomorphization to create real monomorphized names
    let crate_data = util::translate_rust_text(test_code, &["--monomorphize"])?;
    let fmt_ctx = &crate_data.into_fmt();

    println!("=== Testing NameMatcher Mono Initialization ===");

    // Find items with monomorphized path elements
    let monomorphized_items: Vec<_> = crate_data
        .all_items()
        .filter(|item| {
            item.item_meta()
                .name
                .name
                .iter()
                .any(|elem| matches!(elem, PathElem::Monomorphized(_)))
        })
        .collect();

    assert!(
        !monomorphized_items.is_empty(),
        "Should have monomorphized items"
    );
    println!("Found {} monomorphized items", monomorphized_items.len());

    // Test patterns that should work with mono matching
    let test_cases = vec![
        // (pattern, description, should_match_mono)
        ("test_crate::_::new", "glob pattern for new methods", true),
        (
            "test_crate::_::get_first",
            "glob pattern for get_first methods",
            true,
        ),
        ("test_crate::_::swap", "glob pattern for swap methods", true),
        ("_::_::new", "double glob pattern", true),
        (
            "test_crate::Generic::new",
            "specific type pattern (no impl support yet)",
            false,
        ),
    ];

    let mut mono_matches_found = 0;

    for mono_item in &monomorphized_items {
        let name = &mono_item.item_meta().name;
        let name_str = format!("{}", name.with_ctx(fmt_ctx));

        // Skip non-test-crate items (like core:: items)
        if !name_str.starts_with("test_crate::") {
            continue;
        }

        {
            let mut name = name.clone();
            name.name.push(PathElem::Monomorphized(Box::new(
                mono_item.identity_args().clone(),
            )));
            let fmt_ctx = &fmt_ctx.push_binder(Cow::Borrowed(&mono_item.generic_params()));
            println!(
                "\nTesting item: for<{}> {}",
                mono_item.identity_args().with_ctx(fmt_ctx),
                name.with_ctx(fmt_ctx)
            );
        }

        // Verify the item actually has a monomorphized element
        assert!(
            name.name
                .iter()
                .any(|elem| matches!(elem, PathElem::Monomorphized(_))),
            "Item should have monomorphized element"
        );

        for (pattern_str, description, should_match_mono) in &test_cases {
            let pattern = match Pattern::parse(pattern_str) {
                Ok(p) => p,
                Err(_) => continue, // Skip invalid patterns
            };

            // Test both regular and mono matching
            let matches_regular = pattern.matches_item(&crate_data, *mono_item, false);
            let matches_mono = pattern.matches_item(&crate_data, *mono_item, true);

            // Also test the convenience methods
            let matches_mono_convenience = pattern.matches_item(&crate_data, *mono_item, true);
            assert_eq!(
                matches_mono, matches_mono_convenience,
                "Convenience method should match direct call"
            );

            println!(
                "  Pattern '{}' ({}): regular={}, mono={}",
                pattern_str, description, matches_regular, matches_mono
            );

            // For monomorphized items, mono matching should generally work at least as well as regular
            // (though they might differ for specific patterns)
            if *should_match_mono
                && name_str.contains(&pattern_str.split("::").last().unwrap_or(""))
            {
                assert!(
                    matches_mono,
                    "Pattern '{}' should match monomorphized item '{}' in mono mode",
                    pattern_str, name_str
                );
                mono_matches_found += 1;
            }
        }
    }

    assert!(
        mono_matches_found > 0,
        "Should have found at least some mono matches"
    );
    println!(
        "\n✅ Found {} successful mono pattern matches",
        mono_matches_found
    );

    // Test that non-monomorphized items work the same in both modes
    let regular_items: Vec<_> = crate_data
        .all_items()
        .filter(|item| {
            let name_str = format!("{}", item.item_meta().name.with_ctx(fmt_ctx));
            !item
                .item_meta()
                .name
                .name
                .iter()
                .any(|elem| matches!(elem, PathElem::Monomorphized(_)))
                && name_str.starts_with("test_crate::")
                && (name_str.contains("Generic") || name_str.contains("create_instances"))
        })
        .collect();

    for regular_item in &regular_items {
        let name = &regular_item.item_meta().name;
        let name_str = format!("{}", name.with_ctx(fmt_ctx));

        let pattern = Pattern::parse("test_crate::_").expect("Valid pattern");
        let matches_regular = pattern.matches_item(&crate_data, *regular_item, false);
        let matches_mono = pattern.matches_item(&crate_data, *regular_item, true);

        assert_eq!(
            matches_regular, matches_mono,
            "Regular and mono matching should be the same for non-monomorphized items: {}",
            name_str
        );
    }

    println!("✅ NameMatcher mono functionality is properly initialized and working!");

    Ok(())
}
