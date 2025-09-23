use charon_lib::formatter::IntoFormatter;
use charon_lib::pretty::FmtWithCtx;

use charon_lib::name_matcher::Pattern;

mod util;

#[test]
fn test_pattern_matching_debug() -> anyhow::Result<()> {
    // Create a simple test case to understand pattern matching
    let test_code = r#"
        struct TestStruct<T> {
            value: T,
        }
        
        impl<T> TestStruct<T> {
            fn new(value: T) -> Self {
                Self { value }
            }
        }
    "#;

    let crate_data = util::translate_rust_text(test_code, &[])?;
    let fmt_ctx = &crate_data.into_fmt();

    println!("=== All items ===");
    for item in crate_data.all_items() {
        let name = &item.item_meta().name;
        println!("Item: {}", name.with_ctx(fmt_ctx));
    }

    // Test various patterns
    let patterns_to_test = vec![
        "test_crate::{TestStruct<_>}::new",
        "test_crate::{impl TestStruct<_>}::new",
        "_::{TestStruct<_>}::new",
        "*::{TestStruct<_>}::new",
        "test_crate::_::new",
    ];

    for pattern_str in patterns_to_test {
        let pattern = Pattern::parse(pattern_str).expect("Valid pattern");

        for item in crate_data.all_items() {
            let name = &item.item_meta().name;
            let name_str = format!("{}", name.with_ctx(fmt_ctx));
            if name_str.contains("::new") {
                let matches_regular = pattern.matches_item(&crate_data, item, false);
                let matches_mono = pattern.matches_item(&crate_data, item, true);
                println!("Pattern: {}", pattern_str);
                println!("  Item: {}", name_str);
                println!("  Matches (regular): {}", matches_regular);
                println!("  Matches (mono): {}", matches_mono);
                println!();
            }
        }
    }

    Ok(())
}
