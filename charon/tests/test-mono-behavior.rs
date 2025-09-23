use charon_lib::formatter::IntoFormatter;
use charon_lib::pretty::FmtWithCtx;

use charon_lib::ast::*;
use charon_lib::name_matcher::Pattern;

mod util;

#[test]
fn test_monomorphized_pattern_matching() -> anyhow::Result<()> {
    // Test with actual monomorphization enabled
    let test_code = r#"
        struct Container<T> {
            value: T,
        }
        
        impl<T> Container<T> {
            fn new(value: T) -> Self {
                Self { value }
            }
            
            fn get(&self) -> &T {
                &self.value
            }
        }
        
        fn use_containers() {
            let int_container = Container::new(42i32);
            let str_container = Container::new("hello");
            
            let _ = int_container.get();
            let _ = str_container.get();
        }
    "#;
    
    // Translate with monomorphization enabled to create real monomorphized names
    let crate_data = util::translate_rust_text(test_code, &["--monomorphize"])?;
    let fmt_ctx = &crate_data.into_fmt();
    
    // Find monomorphized function items
    let monomorphized_items: Vec<_> = crate_data
        .all_items()
        .filter(|item| {
            item.item_meta().name.name.iter().any(|elem| matches!(elem, PathElem::Monomorphized(_)))
                && format!("{}", item.item_meta().name.with_ctx(fmt_ctx)).contains("::new") // Focus on the 'new' methods
        })
        .collect();
    
    assert!(!monomorphized_items.is_empty(), "Should have monomorphized 'new' methods");
    
    // Test patterns that should work
    let test_patterns = vec![
        ("test_crate::_::new", true, true),  // Should match in both modes
        ("test_crate::Container::new", false, false), // Should not match (no inherent impl pattern support yet)
        ("_::_::new", true, true),           // Should match in both modes
    ];
    
    for mono_item in &monomorphized_items {
        let name = &mono_item.item_meta().name;
        let name_str = format!("{}", name.with_ctx(fmt_ctx));
        println!("Testing monomorphized item: {}", name_str);
        
        for (pattern_str, expected_regular, expected_mono) in &test_patterns {
            let pattern = Pattern::parse(pattern_str).expect("Valid pattern");
            
            let matches_regular = pattern.matches_item(&crate_data, *mono_item, false);
            let matches_mono = pattern.matches_item(&crate_data, *mono_item, true);
            
            println!("  Pattern '{}': regular={}, mono={} (expected: {}, {})", 
                     pattern_str, matches_regular, matches_mono, expected_regular, expected_mono);
            
            assert_eq!(matches_regular, *expected_regular, 
                       "Pattern '{}' regular matching failed for '{}'", pattern_str, name_str);
            assert_eq!(matches_mono, *expected_mono, 
                       "Pattern '{}' mono matching failed for '{}'", pattern_str, name_str);
        }
        
        // Verify that the name actually contains a Monomorphized element
        assert!(name.name.iter().any(|elem| matches!(elem, PathElem::Monomorphized(_))), 
                "Item should contain a Monomorphized element: {}", name_str);
    }
    
    // Test that mono=true doesn't break non-monomorphized items
    let regular_items: Vec<_> = crate_data
        .all_items()
        .filter(|item| {
            !item.item_meta().name.name.iter().any(|elem| matches!(elem, PathElem::Monomorphized(_)))
                && format!("{}", item.item_meta().name.with_ctx(fmt_ctx)).contains("Container")
        })
        .collect();
    
    for regular_item in &regular_items {
        let name = &regular_item.item_meta().name;
        let name_str = format!("{}", name.with_ctx(fmt_ctx));
        println!("Testing regular item: {}", name_str);
        
        let pattern = Pattern::parse("test_crate::Container").expect("Valid pattern");
        let matches_regular = pattern.matches_item(&crate_data, *regular_item, false);
        let matches_mono = pattern.matches_item(&crate_data, *regular_item, true);
        
        // Both should match for regular items
        assert_eq!(matches_regular, matches_mono, 
                   "Mono matching should not affect regular items: {}", name_str);
    }
    
    Ok(())
}