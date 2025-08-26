// Test case to reproduce vtable issue for closures

fn main() {
    // Create a closure that captures variables
    let x = 5;
    let closure = |y: i32| x + y;
    
    // Try to use the closure as a trait object
    let boxed_closure: Box<dyn Fn(i32) -> i32> = Box::new(closure);
    
    let result = boxed_closure(10);
    println!("Result: {}", result);
}