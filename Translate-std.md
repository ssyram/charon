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

6. translate the std by modules.

```
python3 translate_std.py --sysroot "$(rustc --print sysroot)"
```

7. A directory `translate_std` and a file `failed_modules` will be generated. For each std module, if Charon translated it successfully, a file `std_xxx_yyy.txt` will be generated in `translate_std`; otherwise a file `std_xxx_yyy_error.txt` will be generated. All the failed modules are recorded in `failed_modules`.
