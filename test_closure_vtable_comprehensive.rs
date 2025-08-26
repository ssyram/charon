// Test case to specifically exercise closure vtable generation

pub fn test_closure_as_dyn_fn() -> i32 {
    let x = 42;
    let closure = move |y: i32| x + y;
    
    // This should now work - create a trait object from a closure
    let dyn_fn: Box<dyn Fn(i32) -> i32> = Box::new(closure);
    dyn_fn(10)
}

pub fn test_closure_as_dyn_fn_once() -> String {
    let s = String::from("Hello");
    let closure = move || s + " World";
    
    // This should also work - FnOnce trait object
    let dyn_fn_once: Box<dyn FnOnce() -> String> = Box::new(closure);
    dyn_fn_once()
}

pub fn test_closure_as_dyn_fn_mut() -> i32 {
    let mut counter = 0;
    let mut closure = move |increment: i32| {
        counter += increment;
        counter
    };
    
    // This should work - FnMut trait object  
    let dyn_fn_mut: &mut dyn FnMut(i32) -> i32 = &mut closure;
    dyn_fn_mut(5) + dyn_fn_mut(3)
}

// Test with higher-order functions
pub fn apply_fn_to_number<F>(f: F, x: i32) -> i32 
where 
    F: Fn(i32) -> i32 
{
    f(x)
}

pub fn test_higher_order_with_closure() -> i32 {
    let multiplier = 3;
    let closure = move |x| x * multiplier;
    apply_fn_to_number(closure, 10)
}

// Main function for testing
pub fn main() {
    let _ = test_closure_as_dyn_fn();
    let _ = test_closure_as_dyn_fn_once();
    let _ = test_closure_as_dyn_fn_mut();
    let _ = test_higher_order_with_closure();
}