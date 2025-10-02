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

**Error**: `Could not find a clause for <T as std::iter::IntoIterator> in the current context: Ambiguity`

**Trigger**: Using `for` loops, which desugar to `IntoIterator` calls.

**Details**: The monomorphization process encounters recursive or self-referential trait implementations in the standard library's iterator traits, causing trait resolution ambiguity.

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

# Should succeed
charon rustc --monomorphize --input buggy/issue_49.rs
charon rustc --monomorphize --input buggy/where_clauses_fncg.rs
```
