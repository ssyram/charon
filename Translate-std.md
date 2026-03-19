# Translating the Rust Standard Library Incrementally

This document explains how to incrementally translate parts of the Rust standard library using Charon. It originates from [this github issue](https://github.com/AeneasVerif/charon/issues/863) and the subsequent discussion on [zulip](https://aeneas-verif.zulipchat.com/#narrow/channel/423740-dev/topic/Extracting.20the.20standard.20library/with/546486336).

## Instructions

1. Navigate to the the `charon` subdirectory:

```
cd charon
```

2. Set up Miri, which provides a prebuilt version of the standard library that `rustc` will use:

```
rustup component add miri
```

3. Set the `SYSROOT` nvironment variable to tell `rustc` where to look for the standard library:

```
SYSROOT=$(cargo miri setup -v --print-sysroot)
```

4. Create a dummy Rust file with a `main` function (required for the translation process):

```
echo "fn main() {}" > file.rs
```

5. Build Charon:

```
cargo build
```

6. Translate the standard library module by module using the provided Python script:

```
python3 translate_std.py --sysroot "$(rustc --print sysroot)"
```

7. After execution, a directory named `translate_std` and a file named `failed_modules` will be created. For each standard library module:

  - If Charon translates it successfully, a file named `std_xxx_yyy.txt` is generated inside `translate_std`.
  - If translation fails, a file named `std_xxx_yyy_error.txt` is generated instead.
  - All modules that failed are listed in `failed_modules.txt`