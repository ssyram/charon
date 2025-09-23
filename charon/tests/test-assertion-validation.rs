use std::borrow::Cow;

use charon_lib::formatter::{AstFormatter, IntoFormatter};
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
    // === Basic Structs with Early-bound Regions ===

    struct Wrapper<'w, T> {
        value: &'w T,
    }

    struct RefPair<'a, 'b, T, U> {
        first: &'a T,
        second: &'b U,
    }

    // === Implementations with Early-bound Regions ===

    impl<'w, T> Wrapper<'w, T> {
        // Function with late-bound regions (for<'a>)
        fn apply_closure<F>(&self, f: F) -> T
            where
                F: for<'a> Fn(&'a T) -> T,
                T: Clone,
        {
            f(self.value)
        }
        
        fn with_ref<F, R>(&self, f: F) -> R
            where
                F: for<'a> FnOnce(&'a T) -> R,
        {
            f(self.value)
        }
        
        // Functions with early-bound regions
        fn borrow_value<'b>(&self) -> &'b T 
            where 
                'w: 'b
        {
            self.value
        }
        
        fn compare_with<'other>(&self, other: &'other T) -> bool
            where
                T: PartialEq,
                'w: 'other,
        {
            self.value == other
        }
    }

    impl<'a, 'b, T, U> RefPair<'a, 'b, T, U> {
        // Mix of early-bound and late-bound regions
        fn process_both<F, R>(&self, f: F) -> R
            where
                F: for<'x> Fn(&'x T, &'x U) -> R,
        {
            f(self.first, self.second)
        }
        
        fn get_first(&self) -> &'a T {
            self.first
        }
        
        fn get_second(&self) -> &'b U {
            self.second
        }

        fn get_dummy<'c, X>(&self, dummy: &'c X) -> &'c X
            where
                'a: 'c,
        {
            dummy
        }
    }

    // === Standalone Functions with Early-bound Regions ===

    fn process_refs<'x, 'y, T>(first: &'x T, _second: &'y T) -> &'x T
    where
        'y: 'x,
        T: Clone,
    {
        first
    }

    // Function pointer type with late-bound lifetime
    type RefProcessor<T> = for<'a> fn(&'a T) -> T;

    fn process_wrapper() {
        let wrapper = Wrapper { value: &42i32 };
        
        // These should create instances with late-bound regions
        let result1 = wrapper.apply_closure(|x| *x + 1);
        let result2 = wrapper.with_ref(|x| x.to_string());
        
        println!("{}, {}", result1, result2);
    }

    fn use_more() {
        let pair = RefPair { first: &"Hello", second: &"World" };
        
        let combined = pair.process_both(|a, b| format!("{}, {}", a, b));
        println!("{}", combined);
        
        let first = pair.get_first();
        let second = pair.get_second();
        println!("First: {}, Second: {}", first, second);
        
        assert!(pair.get_dummy(&10) == &10);
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
            item.item_meta()
                .name
                .name
                .iter()
                .any(|elem| matches!(elem, PathElem::Monomorphized(_)))
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
        "_::_::get_dummy::_",
    ];

    for mono_item in &monomorphized_items {
        let name = &mono_item.item_meta().name;
        let name_str = format!("{}", name.with_ctx(fmt_ctx));

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

        for pattern_str in &test_patterns {
            let pattern = match Pattern::parse(pattern_str) {
                Ok(p) => p,
                Err(e) => panic!(
                    "Failed to parse pattern: {}, with error: {}",
                    pattern_str, e
                ),
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
