//@ charon-args=--monomorphize

// Simplified version that reproduces the Result<Infallible, E> bug
// without using std library

enum Never {}

enum MyResult<T, E> {
    Ok(T),
    Err(E),
}

fn use_result(_r: MyResult<Never, u8>) {}

fn main() {
    use_result(MyResult::Err(1));
}
