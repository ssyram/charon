use std::pin::Pin;
use std::rc::Rc;
use std::sync::Arc;

trait Simple {
    fn value(&self) -> i32;
}

impl Simple for i32 {
    fn value(&self) -> i32 {
        *self
    }
}

fn test_pin_rc_dyn_trait() {
    // Test Pin<Rc<dyn Trait>> - a complex nested smart pointer
    let val = Rc::new(42i32);
    let trait_obj: Rc<dyn Simple> = val;
    // Use Pin::new_unchecked since we're just testing the type structure
    let pinned: Pin<Rc<dyn Simple>> = unsafe { Pin::new_unchecked(trait_obj) };
    
    // Call method through the complex smart pointer chain
    let result = pinned.value();
    assert_eq!(result, 42);
}

fn test_pin_arc_dyn_trait() {
    // Test Pin<Arc<dyn Trait>> - another complex combination
    let val = Arc::new(100i32);
    let trait_obj: Arc<dyn Simple> = val;
    let pinned: Pin<Arc<dyn Simple>> = unsafe { Pin::new_unchecked(trait_obj) };
    
    // Call method through the complex smart pointer chain
    let result = pinned.value();
    assert_eq!(result, 100);
}

// Note: Pin<Box<dyn Trait>> tests are commented out because Box<dyn Trait>
// has issues with unsize coercions in the current Charon implementation.
// This is a known limitation - see tests/ui/dyn-trait.rs where Box<dyn Display>
// constructions are marked as #[charon::opaque].

/*
fn test_pin_box_dyn_trait() {
    let val = Box::new(200i32);
    let trait_obj: Box<dyn Simple> = val;
    let pinned: Pin<Box<dyn Simple>> = unsafe { Pin::new_unchecked(trait_obj) };
    
    let result = pinned.value();
    assert_eq!(result, 200);
}
*/

fn main() {
    test_pin_rc_dyn_trait();
    test_pin_arc_dyn_trait();
    // test_pin_box_dyn_trait(); // Disabled due to Box<dyn Trait> limitations
}