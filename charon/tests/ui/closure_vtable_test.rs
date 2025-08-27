fn main() {
  let y = 10;
  let f: &dyn Fn(i32) -> i32 = &|x| x + y;
  assert!(f(0) > 0);
}