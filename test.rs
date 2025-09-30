trait MyTrait<T> {
    fn process(&self, x: T) -> T;
}

impl MyTrait<i32> for i32 {
    fn process(&self, x: Self) -> Self {
        self + x
    }
}
impl MyTrait<i32> for bool {
    fn process(&self, x: i32) -> i32 {
        if *self { x } else { 0 }
    }
}

fn main() {
    let a = 10;
    let b = true;
    assert!(a.process(20) == b.process(30));
}