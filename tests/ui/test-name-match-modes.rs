#![feature(register_tool)]
#![register_tool(pattern)]

// Test for different name matching modes

// Generic function that will have monomorphized instances
pub fn generic_function<T>(x: T) -> T {
    x
}

// Regular function 
pub fn regular_function(x: i32) -> i32 {
    x + 1
}

// The test function that calls generic functions with specific types
pub fn test_function() {
    let _x = generic_function::<i32>(42);
    let _y = generic_function::<String>("hello".to_string());
    let _z = regular_function(42);
}

// Test patterns that should match different modes
#[pattern::pass("test_crate::generic_function")]  // Should match in Both and GenericOnly  
pub fn annotated_generic<T>() -> T {
    panic!()
}

#[pattern::pass("test_crate::regular_function")]  // Should match in Both and GenericOnly
pub fn annotated_regular() {
    panic!()
}

#[pattern::pass("test_crate::test_function")]
pub fn annotated_mono() {
    panic!()
}

fn main() {
    test_function();
}