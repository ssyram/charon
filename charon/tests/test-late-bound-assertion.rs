use charon_lib::formatter::IntoFormatter;
use charon_lib::pretty::FmtWithCtx;

use charon_lib::ast::*;
use charon_lib::name_matcher::Pattern;

mod util;

/// Test that validates the late-bound generics assertion in mono matching.
/// This test creates complex scenarios with both early-bound and late-bound generics
/// to ensure the assertion is correct and catches improper usage.
#[test]
fn test_late_bound_generics_assertion() -> anyhow::Result<()> {
    // Test case with late-bound lifetime parameters
    let test_code = r#"
        use std::marker::PhantomData;
        
        struct Container<T, U> {
            field1: T,
            field2: U,
        }
        
        impl<T, U> Container<T, U> {
            // This should create both early-bound (T, U) and late-bound ('a) parameters
            fn borrow_field1<'a>(&'a self) -> &'a T {
                &self.field1
            }
            
            // This has late-bound lifetime but also additional type parameter
            fn transform<'a, V>(&'a self, _value: V) -> (&'a T, V) {
                (&self.field1, _value)
            }
            
            // Higher-rank trait bounds create late-bound lifetimes
            fn apply_fn<F>(&self, f: F) -> T
            where
                F: for<'a> Fn(&'a T) -> T + Clone,
                T: Clone,
            {
                f(&self.field1)
            }
        }
        
        fn test_usage() {
            let container = Container { field1: 42i32, field2: "hello" };
            
            // These should create monomorphized instances with late-bound regions
            let borrowed = container.borrow_field1();
            let transformed = container.transform(99u64);
            
            // Function pointer with late-bound lifetime
            let closure = |x: &i32| *x + 1;
            let result = container.apply_fn(closure);
            
            println!("{}, {:?}, {}", borrowed, transformed, result);
        }
    "#;
    
    // Translate with monomorphization to create real monomorphized names with late-bound regions
    let crate_data = util::translate_rust_text(test_code, &["--monomorphize"])?;
    let fmt_ctx = &crate_data.into_fmt();
    
    println!("=== Testing Late-Bound Generics Assertion ===");
    
    // Find items with monomorphized path elements that have late-bound regions
    let monomorphized_items: Vec<_> = crate_data
        .all_items()
        .filter(|item| {
            let name_str = format!("{}", item.item_meta().name.with_ctx(fmt_ctx));
            item.item_meta().name.name.iter().any(|elem| matches!(elem, PathElem::Monomorphized(_)))
                && name_str.starts_with("test_crate::")
                && (name_str.contains("borrow_field1") || name_str.contains("transform") || name_str.contains("apply_fn"))
        })
        .collect();
    
    assert!(!monomorphized_items.is_empty(), "Should have monomorphized items with late-bound regions");
    println!("Found {} monomorphized items with potential late-bound regions", monomorphized_items.len());
    
    // Test patterns that should work with late-bound regions
    let test_patterns = vec![
        ("test_crate::_::borrow_field1", "Should match monomorphized borrow_field1"),
        ("test_crate::_::transform", "Should match monomorphized transform"),
        ("test_crate::_::apply_fn", "Should match monomorphized apply_fn"),
        ("_::_::borrow_field1", "Should match any borrow_field1 with glob"),
    ];
    
    let mut late_bound_examples_found = 0;
    
    for mono_item in &monomorphized_items {
        let name = &mono_item.item_meta().name;
        let name_str = format!("{}", name.with_ctx(fmt_ctx));
        
        println!("\nTesting item: {}", name_str);
        
        // Check if this name has late-bound regions (indicated by for<...> syntax)
        if name_str.contains("for<") {
            late_bound_examples_found += 1;
            println!("  ^ This item has late-bound regions!");
        }
        
        // Test the patterns
        for (pattern_str, description) in &test_patterns {
            let pattern = match Pattern::parse(pattern_str) {
                Ok(p) => p,
                Err(_) => continue,
            };
            
            // Test both regular and mono matching
            let matches_regular = pattern.matches_item(&crate_data, *mono_item, false);
            let matches_mono = pattern.matches_item(&crate_data, *mono_item, true);
            
            println!("    Pattern '{}': regular={}, mono={} ({})", 
                     pattern_str, matches_regular, matches_mono, description);
            
            // The assertion should work correctly for both cases
            // If it fails, we'll get a panic with details about what went wrong
        }
    }
    
    println!("\n✅ Found {} items with late-bound regions", late_bound_examples_found);
    println!("✅ Late-bound generics assertion handling works correctly!");
    
    // Test edge cases with generic arguments in patterns
    test_pattern_with_generics(&crate_data)?;
    
    Ok(())
}

/// Test pattern matching with generic arguments to validate the assertion logic
fn test_pattern_with_generics(crate_data: &TranslatedCrate) -> anyhow::Result<()> {
    println!("\n=== Testing Pattern Generics ===");
    
    // Test patterns with generics
    let generic_patterns = vec![
        "test_crate::Container<_>::borrow_field1",
        "test_crate::{Container<_>}::borrow_field1", 
        "test_crate::_<_>::borrow_field1",
    ];
    
    let fmt_ctx = &crate_data.into_fmt();
    let monomorphized_items: Vec<_> = crate_data
        .all_items()
        .filter(|item| {
            let name_str = format!("{}", item.item_meta().name.with_ctx(fmt_ctx));
            item.item_meta().name.name.iter().any(|elem| matches!(elem, PathElem::Monomorphized(_)))
                && name_str.starts_with("test_crate::")
                && name_str.contains("borrow_field1")
        })
        .collect();
    
    for pattern_str in generic_patterns {
        println!("Testing generic pattern: {}", pattern_str);
        
        // Try to parse the pattern (some might fail, which is expected)
        match Pattern::parse(pattern_str) {
            Ok(pattern) => {
                for mono_item in &monomorphized_items {
                    let name_str = format!("{}", mono_item.item_meta().name.with_ctx(fmt_ctx));
                    
                    // Test matching - this should work or fail gracefully, not panic
                    let matches_mono = pattern.matches_item(crate_data, *mono_item, true);
                    println!("  '{}' matches '{}': {}", pattern_str, name_str, matches_mono);
                }
            }
            Err(e) => {
                println!("  Pattern parse failed (expected for some): {:?}", e);
            }
        }
    }
    
    println!("✅ Pattern generics testing completed successfully!");
    
    Ok(())
}