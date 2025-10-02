# Buggy Examples - Monomorphization Issues

This directory contains minimal reproducers for bugs in Charon's `--monomorphize` option. All examples have been simplified to not depend on the standard library, making them easier to analyze and fix.

## Bug 1: Uninhabited Type Variant (issue_105.rs)

**Error**: `assertion failed: *index == r_abi::VariantIdx::ZERO`

**Trigger**: Using an enum type where one generic instantiation results in an uninhabited variant (like `MyResult<Never, E>`).

**Details**: When Rust optimizes enums with uninhabited types, it can create a variant with a non-zero index, which violates Charon's assertion.

## Bug 2: Enum Discriminant Index (issue_123.rs)

**Error**: `index out of bounds: the len is 0 but the index is 0`

**Trigger**: Combination of:
1. An enum with `#[derive(PartialEq)]` and multiple variants with explicit discriminants
2. Another enum that is cast to an integer type via a function
3. Both used in main

**Details**: The `remove_read_discriminant` transform has an indexing bug when processing multiple enums with discriminants during monomorphization.

## Bug 3 & 4: Iterator Trait Ambiguity (step_by.rs, symcrust.rs)

**Error**: `Could not find a clause for <T as MyIntoIterator> in the current context: Unimplemented`

**Trigger**: Traits with recursive bounds, specifically a trait method that has a where clause like `Self::Item: MyIntoIterator`, creating a recursive dependency between traits.

**Details**: The monomorphization process encounters traits with recursive bounds (similar to `Iterator::flatten` in std which has `where Self::Item: IntoIterator`). When monomorphizing, charon cannot resolve the clause for the associated type implementing the trait, causing a "Could not find a clause" error. This is now reproduced locally without std dependencies by implementing custom `MyIterator` and `MyIntoIterator` traits with the same recursive structure.

## Bug 5: Missing Default Trait Method in Impl (issue_49.rs)

**Error**: No compile-time error, but subtle extraction bug

**Trigger**: 
1. A trait with a method that has a default implementation
2. An impl that doesn't override the method
3. Code that calls the method

**Details**: When extracting with `--monomorphize`, trait methods with default implementations may be properly included in the impl block in some cases but not others. The extracted impl should include all methods (both overridden and default), but in some cases the default methods are missing.

**Status**: This file demonstrates the correct behavior - the impl includes `my_min` which uses the default implementation. The original version using `usize.min()` from std had the bug where the impl was missing the `min` method.

## Bug 6: Missing Trait Methods in Extracted Code (where_clauses_fncg.rs)

**Error**: No compile-time error, but subtle extraction bug

**Trigger**:
1. Traits with generic methods involving const generics
2. Impls that provide implementations for these methods
3. Code that calls these methods

**Details**: When monomorphizing, trait method declarations may be missing from the extracted trait definitions, and the corresponding impl blocks don't properly reference the implemented methods. For example:
- The `Foo` trait should declare `fn bar<const L: usize>(...) -> Self`
- The `{impl Foo<...>}` should reference `fn bar = ...`
- But the extracted code is missing these method declarations/references

**Current State**: The file demonstrates the bug - when you extract with `--monomorphize` and pretty-print the `.llbc` file, you'll see that the trait definitions are missing their method declarations, and the impl blocks are missing the method references.

## Working Examples

- **issue_49.rs**: Works correctly with `--monomorphize`
- **where_clauses_fncg.rs**: Works correctly with `--monomorphize`

## Testing

To test all examples:

```bash
# Should fail with MyResult assertion error
charon rustc --monomorphize --input buggy/issue_105.rs

# Should fail with index out of bounds
charon rustc --monomorphize --input buggy/issue_123.rs

# Should fail with IntoIterator ambiguity
charon rustc --monomorphize --input buggy/step_by.rs

# Should fail with IntoIterator ambiguity
charon rustc --monomorphize --input buggy/symcrust.rs

# Should fail with trait clause error (now in local code, not std)
charon rustc --monomorphize --input buggy/step_by.rs
charon rustc --monomorphize --input buggy/symcrust.rs

# Should succeed but check extracted code for completeness
charon rustc --monomorphize --input buggy/issue_49.rs
# Then: charon pretty-print issue_49.llbc | grep -A 5 "impl MyOrd"
# Verify that the impl includes both my_cmp and my_min methods

# Should succeed but check extracted code for missing trait methods
charon rustc --monomorphize --input buggy/where_clauses_fncg.rs
# Then: charon pretty-print where_clauses_fncg.llbc | grep -A 5 "trait Foo"
# Verify that the trait definitions include their method declarations
```
