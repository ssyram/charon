// Test case for closure vtable instance generation
// This ensures that closures can be properly used as trait objects

fn test_basic_closure_trait_object() {
    let x = 42;
    let closure = move |y: i32| x + y;
    
    // Create a trait object - this should trigger vtable instance creation
    let _fn_obj: Box<dyn Fn(i32) -> i32> = Box::new(closure);
}

fn test_closure_fnonce_trait_object() {
    let s = String::from("hello");
    let closure = move || s;
    
    // FnOnce trait object
    let _fn_once_obj: Box<dyn FnOnce() -> String> = Box::new(closure);
}

fn test_closure_fnmut_trait_object() {
    let mut counter = 0;
    let closure = move || {
        counter += 1;
        counter
    };
    
    // FnMut trait object  
    let _fn_mut_obj: Box<dyn FnMut() -> i32> = Box::new(closure);
}

// Test with higher-order function that accepts trait objects
fn apply_fn<F>(f: F) -> i32 
where 
    F: Fn() -> i32
{
    f()
}

fn test_higher_order_closure() {
    let value = 100;
    let closure = move || value;
    
    // Pass closure directly
    let _result = apply_fn(closure);
}