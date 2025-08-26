// Test specifically designed to trigger vtable instance initialization for closures
// This should trigger the error path that was previously unhandled

pub trait CustomTrait {
    fn custom_method(&self) -> i32;
}

// A generic function that works with any type implementing our trait
pub fn use_trait_object<T: CustomTrait + 'static>(obj: T) -> Box<dyn CustomTrait> {
    Box::new(obj)
}

// Implementation for closures - this would trigger vtable generation
impl<F> CustomTrait for F 
where 
    F: Fn() -> i32
{
    fn custom_method(&self) -> i32 {
        self()
    }
}

pub fn test_closure_with_custom_trait() -> i32 {
    let value = 123;
    let closure = move || value * 2;
    
    // This should trigger vtable instance creation for the closure trait impl
    let trait_obj = use_trait_object(closure);
    trait_obj.custom_method()
}

// Test with multiple closure kinds
pub fn test_multiple_closure_kinds() -> (i32, String, i32) {
    // FnOnce closure
    let s = String::from("test");
    let fn_once_closure = move || s.len() as i32;
    
    // Fn closure
    let multiplier = 3;
    let fn_closure = move |x: i32| x * multiplier;
    
    // FnMut closure
    let mut counter = 0;
    let fn_mut_closure = move || {
        counter += 1;
        counter
    };
    
    // Create trait objects - should all work now
    let fn_once_obj: Box<dyn FnOnce() -> i32> = Box::new(fn_once_closure);
    let fn_obj: Box<dyn Fn(i32) -> i32> = Box::new(fn_closure);
    let mut fn_mut_obj: Box<dyn FnMut() -> i32> = Box::new(fn_mut_closure);
    
    (fn_once_obj(), fn_obj(10), fn_mut_obj())
}

pub fn main() {
    let result1 = test_closure_with_custom_trait();
    let result2 = test_multiple_closure_kinds();
    println!("Results: {}, {:?}", result1, result2);
}