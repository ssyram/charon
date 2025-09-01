//@ charon-args=--mir=optimized
//@ no-default-options

fn test_box_drop() {
    let b = Box::new(42);
    drop(b);
}

fn main() {
    test_box_drop();
}