//@ charon-args=--monomorphize

// Minimal reproduction of enum discriminant index out of bounds error
// This happens when monomorphizing code that has:
// 1. An enum with #[derive(PartialEq)] and multiple variants with explicit discriminants
// 2. Another enum that is cast to an integer type via a function
// 3. Both are used in main

#[derive(PartialEq)]
enum E1 {
    A = 1,
    B = 2,
}

enum E2 {
    C = 3,
    D = 4,
}

fn fun(e: E2) -> i32 {
    e as i32
}

fn main() {
    let _ = E1::A as isize;
    let _ = fun(E2::C);
}
