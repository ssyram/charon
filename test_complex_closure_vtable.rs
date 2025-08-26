// Test case to reproduce vtable issue for closures with more complex scenarios

use std::marker::PhantomData;

trait MyTrait {
    fn my_method(&self) -> i32;
}

struct MyStruct<T> {
    data: i32,
    phantom: PhantomData<T>,
}

impl<T> MyTrait for MyStruct<T> {
    fn my_method(&self) -> i32 {
        self.data
    }
}

fn test_closure_as_trait_object() {
    let x = 42;
    let closure = move |_: ()| x;
    
    // Try to create a vtable instance for a closure trait impl
    let fn_once_obj: Box<dyn FnOnce(()) -> i32> = Box::new(closure);
    let result = fn_once_obj(());
    println!("FnOnce result: {}", result);
}

fn test_complex_closure() {
    let data = vec![1, 2, 3, 4, 5];
    let multiplier = 2;
    
    // Complex closure that captures multiple variables
    let complex_closure = move |index: usize| -> i32 {
        if index < data.len() {
            data[index] * multiplier
        } else {
            0
        }
    };
    
    // Use as trait object - this should trigger vtable generation
    let trait_obj: Box<dyn Fn(usize) -> i32> = Box::new(complex_closure);
    println!("Complex result: {}", trait_obj(2));
}

fn main() {
    test_closure_as_trait_object();
    test_complex_closure();
}