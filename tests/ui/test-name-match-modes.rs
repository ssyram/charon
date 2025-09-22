#![feature(register_tool)]
#![register_tool(pattern)]

// Test for different name matching modes
pub fn generic_function<T>(x: T) -> T {
    x
}

// The monomorphized version would be generated for specific types
pub fn test_function() {
    let _x = generic_function::<i32>(42);
    let _y = generic_function::<String>("hello".to_string());
}

// Test patterns that should match different modes
#[pattern::pass("test_crate::generic_function")]  // Should match in Both and GenericOnly
#[pattern::pass("test_crate::generic_function<_>")]  // Should match specific instantiations
pub fn annotated_generic<T>() -> T {
    panic!()
}

#[pattern::pass("test_crate::test_function")]
pub fn annotated_mono() {
    panic!()
}