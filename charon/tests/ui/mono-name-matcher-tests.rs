#![feature(register_tool)]
#![register_tool(pattern)]

//! Tests for monomorphized name matching
//! This ensures that the NameMatcher works correctly when match_mono=true

mod foo {
    // Test basic monomorphized matching
    #[pattern::pass("test_crate::foo::generic_fn")]
    #[pattern::mono_pass("test_crate::foo::generic_fn")]
    fn generic_fn<T>() {}
}

// Simple test without complex trait bounds
struct SimpleStruct<T> {
    value: T,
}

impl<T> SimpleStruct<T> {
    // Use glob patterns that should work
    #[pattern::pass("test_crate::_::new")]
    #[pattern::mono_pass("test_crate::_::new")]
    #[pattern::pass("_::_::new")]
    #[pattern::mono_pass("_::_::new")]
    fn new(value: T) -> Self {
        Self { value }
    }
}

// Test function to create instances (this will generate monomorphized versions)
#[pattern::pass("test_crate::test_usage")]
#[pattern::mono_pass("test_crate::test_usage")]
fn test_usage() {
    // These should create monomorphized instances when compiled
    let _int_struct = SimpleStruct::new(42i32);
    let _str_struct = SimpleStruct::new("hello");
}