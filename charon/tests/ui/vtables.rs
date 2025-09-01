#![feature(trait_alias)]
trait Super<T> {
    type Output;
    fn super_method(&self, arg: T) -> Self::Output;
}
trait Checkable<T>: Super<T> {
    fn check(&self) -> bool;
}
impl Super<i32> for i32 {
    type Output = i32;
    fn super_method(&self, arg: i32) -> i32 {
        *self + arg
    }
}
impl Checkable<i32> for i32 {
    fn check(&self) -> bool {
        self.super_method(10) > 0
    }
}
fn use_checkable(x: &dyn Checkable<i32, Output = i32>) -> bool {
    x.check()
}
impl Super<i32> for String {
    type Output = i32;
    fn super_method(&self, arg: i32) -> i32 {
        self.len() as i32 + arg
    }
}
impl Checkable<i32> for String {
    fn check(&self) -> bool {
        self.super_method(0) >= 0
    }
}

impl<const N: usize> Super<i32> for [Vec<i32>; N] {
    type Output = i32;
    fn super_method(&self, arg: i32) -> i32 {
        if self[0].len() > N { arg + 1 } else { arg }
    }
}
impl<const N: usize> Checkable<i32> for [Vec<i32>; N] {
    fn check(&self) -> bool {
        self.super_method(0) >= 0
    }
}

impl Super<i32> for (i32, Vec<i32>) {
    type Output = i32;
    fn super_method(&self, arg: i32) -> i32 {
        self.0 + self.1.iter().copied().sum::<i32>() + arg
    }
}
impl Checkable<i32> for (i32, Vec<i32>) {
    fn check(&self) -> bool {
        self.super_method(0) > 0
    }
}

fn extra_checks() {
    let b : String = String::from("Hello");
    assert!(use_checkable(&b as &dyn Checkable<i32, Output = i32>));

    let arr = [vec![0], vec![2, 3]];
    assert!(use_checkable(&arr as &dyn Checkable<i32, Output = i32>));

    let tup = (10, vec![20, 30]);
    assert!(use_checkable(&tup as &dyn Checkable<i32, Output = i32>));
}

trait NoParam {
    fn dummy(&self);
}
impl NoParam for i32 {
    fn dummy(&self) {
        assert!(*self > 0);
    }
}
fn to_dyn_obj<T: NoParam>(arg: &T) -> &dyn NoParam {
    arg
}

trait Modifiable<T> {
    fn modify(&mut self, arg: &T) -> T;
}
impl<T: Clone> Modifiable<T> for i32 {
    fn modify(&mut self, arg: &T) -> T {
        *self += 1;
        arg.clone()
    }
}
fn modify_trait_object<T: Clone>(arg: &T) -> T {
    let x: &mut dyn Modifiable<T> = &mut 199;
    x.modify(arg)
}

trait BaseOn<T> {
    fn operate_on(&self, t: &T);
}
trait Both32And64: BaseOn<i32> + BaseOn<i64> {
    fn both_operate(&self, t32: &i32, t64: &i64) {
        self.operate_on(t32);
        self.operate_on(t64);
    }
}
impl BaseOn<i32> for i32 {
    fn operate_on(&self, t: &i32) {
        assert!(*self > *t);
    }
}
impl BaseOn<i64> for i32 {
    fn operate_on(&self, t: &i64) {
        assert!(*self as i64 > *t);
    }
}
impl Both32And64 for i32 {}
trait Alias = Both32And64;

trait LifetimeTrait {
    type Ty;
    fn lifetime_method<'a>(&self, arg: &'a Self::Ty) -> &'a Self::Ty;
}
impl LifetimeTrait for i32 {
    type Ty = i32;
    fn lifetime_method<'a>(&self, arg: &'a Self::Ty) -> &'a Self::Ty {
        assert!(*self > *arg);
        arg
    }
}
fn use_lifetime_trait<'a>(x: &dyn LifetimeTrait<Ty = i32>, y: &'a i32) -> &'a i32 {
    x.lifetime_method(y)
}

fn use_alias(x: &dyn Alias) {
    x.both_operate(&100, &200);
}



fn main() {
    let x: &dyn Checkable<i32, Output = i32> = &42;
    assert!(x.check());
    let y: &mut dyn Modifiable<i32> = &mut 99;
    assert!(!modify_trait_object(&"Hello".to_string()).is_empty());
    assert_eq!(y.modify(&mut 100), 100);
    let z: &dyn NoParam = to_dyn_obj(&42);
    z.dummy();
    let a: &dyn Both32And64 = &42;
    a.both_operate(&100, &200);
}
