use std::rc::Rc;

trait Simple {
    fn value(&self) -> i32;
}

impl Simple for i32 {
    fn value(&self) -> i32 {
        *self
    }
}

fn test_simple() {
    let rc_val = Rc::new(42i32);
    let trait_obj: Rc<dyn Simple> = rc_val;
    // Actually call the method to trigger vtable code
    let result = trait_obj.value();
    assert_eq!(result, 42);
}

fn main() {
    test_simple();
}