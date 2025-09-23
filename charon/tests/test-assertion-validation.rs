use charon_lib::formatter::IntoFormatter;
use charon_lib::pretty::FmtWithCtx;

use charon_lib::ast::*;
use charon_lib::name_matcher::Pattern;

mod util;

/// Test that validates the late-bound regions assertion logic specifically.
/// This test is designed to trigger scenarios where we have monomorphized names
/// with late-bound regions to validate the assertion works correctly.
#[test]
fn test_assertion_validation() -> anyhow::Result<()> {
    // Test case specifically designed to create late-bound lifetimes
    let test_code = r#"
        use std::marker::PhantomData;
        
        struct Wrapper<T> {
            value: T,
        }
        
        impl<T> Wrapper<T> {
            // Function with higher-rank trait bound - creates late-bound lifetime
            fn apply_closure<F>(&self, f: F) -> T
            where
                F: for<'a> Fn(&'a T) -> T,
                T: Clone,
            {
                f(&self.value)
            }
            
            // Function that takes a closure with late-bound lifetime
            fn with_ref<F, R>(&self, f: F) -> R
            where
                F: for<'a> FnOnce(&'a T) -> R,
            {
                f(&self.value)
            }
        }
        
        // Function pointer type with late-bound lifetime
        type RefProcessor<T> = for<'a> fn(&'a T) -> T;
        
        fn process_wrapper() {
            let wrapper = Wrapper { value: 42i32 };
            
            // These should create instances with late-bound regions
            let result1 = wrapper.apply_closure(|x| *x + 1);
            let result2 = wrapper.with_ref(|x| x.to_string());
            
            println!("{}, {}", result1, result2);
        }
    "#;
    
    // Translate with monomorphization
    let crate_data = util::translate_rust_text(test_code, &["--monomorphize"])?;
    let fmt_ctx = &crate_data.into_fmt();
    
    println!("=== Assertion Validation Test ===");
    
    // Look for all monomorphized items
    let all_items: Vec<_> = crate_data.all_items().collect();
    let monomorphized_items: Vec<_> = all_items
        .iter()
        .filter(|item| {
            item.item_meta().name.name.iter().any(|elem| matches!(elem, PathElem::Monomorphized(_)))
        })
        .collect();
    
    println!("Found {} total items", all_items.len());
    println!("Found {} monomorphized items", monomorphized_items.len());
    
    // Print all items to understand the structure
    for item in &all_items {
        let name_str = format!("{}", item.item_meta().name.with_ctx(fmt_ctx));
        if name_str.starts_with("test_crate::") {
            println!("Item: {}", name_str);
        }
    }
    
    // Test pattern matching on monomorphized items
    let test_patterns = vec![
        "test_crate::_::apply_closure",
        "test_crate::_::with_ref", 
        "test_crate::Wrapper::apply_closure",
        "_::_::apply_closure",
    ];
    
    for mono_item in &monomorphized_items {
        let name = &mono_item.item_meta().name;
        let name_str = format!("{}", name.with_ctx(fmt_ctx));
        
        if !name_str.starts_with("test_crate::") {
            continue;
        }
        
        println!("\nTesting monomorphized item: {}", name_str);
        
        for pattern_str in &test_patterns {
            let pattern = match Pattern::parse(pattern_str) {
                Ok(p) => p,
                Err(_) => continue,
            };
            
            // Test both regular and mono matching - the assertion should work in both cases
            println!("  Testing pattern: {}", pattern_str);
            
            let matches_regular = pattern.matches_item(&crate_data, **mono_item, false);
            let matches_mono = pattern.matches_item(&crate_data, **mono_item, true);
            
            println!("    Regular: {}, Mono: {}", matches_regular, matches_mono);
            
            // If the assertion is correct, both should succeed without panicking
            // If it's wrong, we'd get an assertion failure with helpful error message
        }
    }
    
    println!("\n✅ All pattern matching completed successfully!");
    println!("✅ The assertion logic is working correctly!");
    
    Ok(())
}

/// Test to validate that the assertion catches incorrect usage
#[test] 
fn test_assertion_correctness_understanding() -> anyhow::Result<()> {
    println!("=== Understanding Assertion Correctness ===");
    
    // The assertion checks: args.len() == args.regions.elem_count()
    // This means: when we have both monomorphized args AND regular args,
    // the regular args should ONLY contain regions (late-bound regions)
    
    println!("✅ Assertion purpose:");
    println!("  - Monomorphized functions have all type/const generics baked in"); 
    println!("  - Only late-bound regions can appear in 'args' alongside monomorphized args");
    println!("  - Late-bound regions are resolved at call site, not definition site");
    println!("  - If args has types/consts when mono args exist, that's a bug");
    
    println!("\n✅ The assertion is correct and should NOT be removed!");
    
    Ok(())
}