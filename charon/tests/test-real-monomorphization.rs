use charon_lib::formatter::IntoFormatter;
use charon_lib::pretty::FmtWithCtx;
use itertools::Itertools;

use charon_lib::ast::*;
use charon_lib::name_matcher::Pattern;

mod util;

#[test]
fn test_real_monomorphization() -> anyhow::Result<()> {
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
        
        fn make_containers() {
            let int_container = Container::new(42i32);
            let str_container = Container::new("hello");
            
            // Use the containers to force monomorphization
            let _ = int_container.get();
            let _ = str_container.get();
        }
    "#;
    
    // Translate with monomorphization enabled
    let crate_data = util::translate_rust_text(test_code, &["--monomorphize"])?;
    let fmt_ctx = &crate_data.into_fmt();
    
    println!("=== All items with monomorphization ===");
    for item in crate_data.all_items() {
        let name = &item.item_meta().name;
        let name_str = format!("{}", name.with_ctx(fmt_ctx));
        println!("Item: {}", name_str);
        
        // Check if this name contains a monomorphized element
        if name.name.iter().any(|elem| matches!(elem, PathElem::Monomorphized(_))) {
            println!("  ^ Contains monomorphized element!");
            
            // Test pattern matching with mono=true vs mono=false
            let patterns_to_test = vec![
                "test_crate::_::new",
                "test_crate::_::get", 
                "test_crate::Container::new",
                "_::Container::new",
            ];
            
            for pattern_str in &patterns_to_test {
                if let Ok(pattern) = Pattern::parse(pattern_str) {
                    let matches_regular = pattern.matches_item(&crate_data, item, false);
                    let matches_mono = pattern.matches_item(&crate_data, item, true);
                    
                    println!("    Pattern '{}': regular={}, mono={}", 
                             pattern_str, matches_regular, matches_mono);
                }
            }
        }
    }
    
    Ok(())
}