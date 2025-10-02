//@ charon-args=--monomorphize

// Minimal reproduction of the Iterator/IntoIterator trait ambiguity error
// This reproduces the issue locally without using std library.
// The bug occurs when monomorphization encounters traits with recursive bounds.

trait MyIterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
    
    // Method with recursive bound: Self::Item: MyIntoIterator
    // This is similar to Iterator::flatten in std
    fn my_flatten(self) -> MyFlatten<Self>
    where
        Self: Sized,
        Self::Item: MyIntoIterator,
    {
        MyFlatten { iter: self }
    }
}

trait MyIntoIterator {
    type Item;
    type IntoIter: MyIterator<Item = Self::Item>;
    fn into_iter(self) -> Self::IntoIter;
}

struct MyRange {
    start: usize,
    end: usize,
}

struct MyFlatten<I> {
    iter: I,
}

impl MyIterator for MyRange {
    type Item = usize;
    fn next(&mut self) -> Option<usize> {
        if self.start < self.end {
            let val = self.start;
            self.start += 1;
            Some(val)
        } else {
            None
        }
    }
}

impl MyIntoIterator for MyRange {
    type Item = usize;
    type IntoIter = MyRange;
    fn into_iter(self) -> MyRange {
        self
    }
}

fn use_iterator() {
    let mut iter = MyRange { start: 0, end: 256 };
    while let Some(_) = iter.next() {
    }
}

fn main() {
    use_iterator();
}
