//@ charon-args=--monomorphize

// Minimal reproducer for missing default trait method implementations in impls
// The bug: When a trait has a default implementation for a method, and an impl
// doesn't override it, the monomorphized impl block should include a reference
// to the default implementation, but it may be missing.

trait MyOrd {
    fn my_cmp(&self, other: &Self) -> bool;
    
    // Default implementation that should appear in the impl
    fn my_min(self, other: Self) -> Self
    where
        Self: Sized,
    {
        if self.my_cmp(&other) {
            self
        } else {
            other
        }
    }
}

impl MyOrd for usize {
    fn my_cmp(&self, other: &Self) -> bool {
        self <= other
    }
    // Note: my_min is not overridden, uses default implementation
}

pub fn f(a: usize, b: usize) -> usize {
    a.my_min(b)
}

fn main() {
    let expected = 0;
    let actual = f(0, 0);
    assert_eq!(expected, actual);
}
