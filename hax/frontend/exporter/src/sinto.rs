use crate::prelude::{derive_group, JsonSchema};

#[cfg(not(feature = "rustc"))]
pub trait SInto<S, To> {
    fn sinto(&self, s: &S) -> To;
}

#[cfg(feature = "rustc")]
pub trait SInto<S, To>: std::marker::PointeeSized {
    fn sinto(&self, s: &S) -> To;
}

#[macro_export]
macro_rules! sinto_todo {
    ($($mod:ident)::+, $type:ident$(<$($lts:lifetime),*$(,)?>)? as $renamed:ident) => {
        #[derive_group(Serializers)]
        #[derive(Clone, Debug, JsonSchema, Hash, PartialEq, Eq, PartialOrd, Ord)]
        pub enum $renamed {
            $type {
                todo: String
            },
        }
        #[cfg(feature = "rustc")]
        impl<$($($lts,)*)? S> SInto<S, $renamed> for $($mod)::+::$type$(<$($lts,)*>)? {
            fn sinto(&self, _: &S) -> $renamed {
                $renamed::$type{todo: format!("{:?}", self)}
            }
        }
    };
    ($($mod:ident)::+, $type:ident$(<$($lts:lifetime),*$(,)?>)?) => {
        sinto_todo!($($mod)::+, $type$(<$($lts),*>)? as $type);
    }
}

#[macro_export]
macro_rules! sinto_as_usize {
    ($($mod:ident)::+, $type:ident$(<$($lts:lifetime),*$(,)?>)?) => {
        pub type $type = usize;
        #[cfg(feature = "rustc")]
        impl<$($($lts,)*)? S> SInto<S, $type> for $($mod)::+::$type$(<$($lts,)*>)? {
            fn sinto(&self, _: &S) -> $type {
                self.as_usize()
            }
        }
    }
}

#[allow(dead_code)]
mod test {
    #[derive(Debug)]
    pub struct Foo(usize);
    impl Foo {
        pub fn as_usize(&self) -> usize {
            self.0
        }
    }
}

sinto_todo!(test, Foo);
// sinto_as_usize!(test, Foo);

impl<S, LL, RR, L: SInto<S, LL>, R: SInto<S, RR>> SInto<S, (LL, RR)> for (L, R) {
    fn sinto(&self, s: &S) -> (LL, RR) {
        (self.0.sinto(s), self.1.sinto(s))
    }
}

impl<S, AA, BB, CC, A: SInto<S, AA>, B: SInto<S, BB>, C: SInto<S, CC>> SInto<S, (AA, BB, CC)>
    for (A, B, C)
{
    fn sinto(&self, s: &S) -> (AA, BB, CC) {
        (self.0.sinto(s), self.1.sinto(s), self.2.sinto(s))
    }
}

impl<S, D, T: SInto<S, D>> SInto<S, Option<D>> for Option<T> {
    fn sinto(&self, s: &S) -> Option<D> {
        self.as_ref().map(|x| x.sinto(s))
    }
}
impl<S, D, T: SInto<S, D>> SInto<S, D> for Box<T> {
    fn sinto(&self, s: &S) -> D {
        (**self).sinto(s)
    }
}
impl<S, D, T: SInto<S, D>> SInto<S, D> for &T {
    fn sinto(&self, s: &S) -> D {
        (**self).sinto(s)
    }
}
impl<S, D: Clone, T: SInto<S, D>> SInto<S, Vec<D>> for [T] {
    fn sinto(&self, s: &S) -> Vec<D> {
        self.iter().map(|x| x.sinto(s)).collect()
    }
}
impl<S, D: Clone, T: SInto<S, D>> SInto<S, Vec<D>> for Box<[T]> {
    fn sinto(&self, s: &S) -> Vec<D> {
        self.into_iter().map(|x| x.sinto(s)).collect()
    }
}

impl<S, D: Clone, T: SInto<S, D>> SInto<S, Vec<D>> for Vec<T> {
    fn sinto(&self, s: &S) -> Vec<D> {
        self.iter().map(|x| x.sinto(s)).collect()
    }
}

macro_rules! sinto_clone {
    ($t:ty) => {
        impl<S> SInto<S, $t> for $t {
            fn sinto(&self, _: &S) -> $t {
                self.clone()
            }
        }
    };
    ($t:ty, $($rest:tt)*) => {
        sinto_clone!($t);
        sinto_clone!($($rest)+);
    };
    () => {};
}

sinto_clone!(bool, String, char);
sinto_clone!(u8, u16, u32, u64, u128, usize);
sinto_clone!(i8, i16, i32, i64, i128, isize);
