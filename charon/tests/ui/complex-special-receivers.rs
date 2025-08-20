trait Simple {
    fn value(&self) -> i32;
}

impl Simple for i32 {
    fn value(&self) -> i32 {
        *self
    }
}

fn test_box_dyn_trait() {
    // Test Box<dyn Trait> without Pin
    let val = Box::new(200i32);
    let trait_obj: Box<dyn Simple> = val;
    
    // Call method through the smart pointer
    let result = trait_obj.value();
    assert_eq!(result, 200);
}

fn main() {
    test_box_dyn_trait();
}