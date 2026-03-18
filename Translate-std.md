# Translate Rust Standard Library

This document introduces how to translate parts of the Rust standard library incrementally.
It origins from the [issue](https://github.com/AeneasVerif/charon/issues/863) in the authority of Charon in Github and the discussion on the [zulip](https://aeneas-verif.zulipchat.com/#narrow/channel/423740-dev/topic/Extracting.20the.20standard.20library/with/546486336).

## Instructions

1. cd to the `charon` subdirectory.

```
cd charon
```

2. setup miri by (miri offers the prebuilt of the standard library that will be used by rustc).

```
rustup component add miri
```

3. setup `SYSROOT` env, which tells rustc the location of the `std` to look for.

```
SYSROOT=$(cargo miri setup -v --print-sysroot)
```

4. create a dummy rust file with the `main` function.

```
echo "fn main() {}" > file.rs
```

5. build `charon`.

```
cargo build
```

6. translate the std function by specifying `--start-from`. For example, the following command translates `std::cmp::min`.

```
RUST_LOG=trace ./target/debug/charon rustc --start-from=std::cmp::min --include=std --no-serialize -- --sysroot="$SYSROOT" file.rs 2>trace.txt
```

7. check the LLBC after translation by opening `trace.txt` and searching `LLBC`.


## TODO

1. Support output `.llbc` files with the `print-llbc` compilation option.

2. Support recursively translate all the items in a std module.