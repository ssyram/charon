//! Simple test file for printing tests

pub fn add(x: u32, y: u32) -> u32 {
    x + y
}

pub fn incr(x: &mut u32) {
    *x = *x + 1;
}

pub const ZERO: u32 = 0;
pub const ONE: u32 = 1;

pub struct Point {
    pub x: i32,
    pub y: i32,
}

pub enum Option<T> {
    Some(T),
    None,
}

pub fn test_match(opt: Option<u32>) -> u32 {
    match opt {
        Option::Some(x) => x,
        Option::None => 0,
    }
}
