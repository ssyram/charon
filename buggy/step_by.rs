//@ charon-args=--monomorphize

// Minimal reproduction of the IntoIterator error
// This is caused by using `for` loops which desugar to IntoIterator
// The error occurs because monomorphization tries to process the std
// iterator traits, which reference themselves recursively

fn main() {
    for _ in 0..1 {
    }
}
