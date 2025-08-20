use std::pin::Pin;
use std::rc::Rc;

trait Simple {
    fn value(&self) -> i32;
}

impl Simple for i32 {
    fn value(&self) -> i32 {
        *self
    }
}

fn test_pin_simple() {
    // Test Pin<Rc<dyn Simple>>
    let val = Rc::new(42i32);
    let trait_obj: Rc<dyn Simple> = val;
    let pinned: Pin<Rc<dyn Simple>> = unsafe { Pin::new_unchecked(trait_obj) };
    let result = pinned.value();
    assert_eq!(result, 42);
}

fn main() {
    test_pin_simple();
}