# Manual Implementations in generate-hs

This document explains why certain types require manual implementations in the Haskell code generation (`charon/src/bin/generate-hs/main.rs`), and cannot be automatically generated from the Rust AST definitions.

## Manual JSON Deserializers (`manual_json_impls`)

### ScalarValue
**Reason**: Special integer parsing for large values (u128/i128)

Large integers (128-bit values) may be serialized as either JSON numbers or JSON strings (when they exceed JavaScript's safe integer range). The `ScalarValue` type requires a custom `parseIntegerValue` helper function that can handle both representations:

```haskell
parseIntegerValue :: Value -> Parser Integer
parseIntegerValue (String s) = ... -- Parse from string for large values
parseIntegerValue (Number n) = ... -- Parse from number for smaller values
```

This cannot be automated because the standard `parseJSON` for `Integer` only handles numbers, not strings.

**Similar in generate-ml**: Yes - OCaml also has special handling, though it uses `big_int_of_json` which handles the dual representation.

## Manually Implemented Types (`manually_implemented`)

These types are excluded from automatic FromJSON instance generation:

### Core Types (shared with generate-ml)

#### ItemOpacity, PredicateOrigin, Ty, Opaque
**Reason**: External/non-local types or special handling required

These are types that either:
- Come from external crates and aren't part of the charon AST
- Require special deserialization logic not covered by the standard patterns
- Have complex dependencies that make automatic generation difficult

**In generate-ml**: Same types are manually implemented

#### Body, FunDecl, TranslatedCrate
**Reason**: Complex types with LLBC/ULLBC variant dependencies

These types have variant-specific fields (LLBC vs ULLBC) and complex nested structures that would require sophisticated code generation logic. They are manually implemented in the templates to handle their complexity correctly.

**In generate-ml**: Same types are manually implemented

### Haskell-Specific Types

#### Vector
**Reason**: Phantom type parameter causes conflicts

`Vector<I, T>` is a Rust type with two type parameters where `I` is a phantom type (index type) that doesn't affect the JSON representation. The type is serialized as a list of values (filtering out `None` entries), but generating a standard `FromJSON` instance for it would conflict with Haskell's built-in list instance.

**In generate-ml**: Also requires manual implementation with `filter_map` to handle `None` values

**Manual definition in template**: Type alias `type Vector k v = [v]` with manual FromJSON that ignores the phantom type parameter `k`

#### Field
**Reason**: Name conflict with ProjectionElem variant

There is both a struct named `Field` (in Types module) and a variant named `Field` in the `ProjectionElem` enum. In Haskell, both types and constructors share the same namespace, so we cannot have both an auto-generated type and an auto-generated constructor with the same name. The `Field` struct is manually defined in the template to avoid this conflict.

**In generate-ml**: No conflict because OCaml has separate namespaces for types and constructors

**Manual definition in templates**:
- Type definition in GAst.hs template
- FromJSON instance in GAstOfJson.hs template (qualified import to resolve conflict)

#### TraitImpl, TraitMethod
**Reason**: Name conflicts between GAst structs and Types enum variants

Both `TraitImpl` and `TraitMethod` exist as:
1. Struct types in the GAst module
2. Enum variant constructors in the Types module (e.g., in `TraitRefKind` and `FnPtrKind`)

In Haskell, type constructors and data constructors share the same namespace within a module. The auto-generated code would create naming conflicts.

**In generate-ml**: No conflict because OCaml has separate namespaces

**Manual definition in templates**:
- Type definitions in GAst.hs template  
- FromJSON instances in GAstOfJson.hs template with qualified imports (e.g., `T.TraitImpl` for the Types variant)

#### Local
**Reason**: Name conflict - struct vs variant

Similar to TraitImpl/TraitMethod, `Local` exists as both a struct (in GAst) and potentially as a variant name in other enums. To avoid conflicts, it's manually defined.

**In generate-ml**: No conflict due to separate namespaces

**Manual definition in templates**:
- Type definition in GAst.hs template
- FromJSON instance in GAstOfJson.hs template

#### Assert (renamed to Assertion)
**Reason**: Reserved keyword in Haskell and name mismatch with JSON

The Rust type is named `Assert`, but:
1. In JSON it's serialized with a different name  
2. `assert` is a keyword/common term in Haskell
3. To avoid confusion, it's renamed to `Assertion` in the Haskell code

**In generate-ml**: Uses `assert` directly (not a reserved word in OCaml)

**Manual definition in templates**:
- Type definition in GAst.hs template (named `Assertion`)
- FromJSON instance in GAstOfJson.hs template parsing from JSON "Assert" field

#### Call, CopyNonOverlapping
**Reason**: Name conflicts or special field naming requirements

These types likely have similar naming conflicts with enum variants or require special handling of their fields to avoid conflicts in Haskell's namespace.

**In generate-ml**: Not in the manual list

**Manual definition in templates**:
- Type definitions in GAst.hs template
- FromJSON instances in GAstOfJson.hs template

## Summary

### Comparison with generate-ml

- **manual_json_impls**: generate-hs has 1 (down from 7), generate-ml has 2
  - Both have Vector (filters None values)
  - Both had special handling needs that were automated in this PR
  - ScalarValue in generate-hs requires dual String/Number parsing

- **manually_implemented**: generate-hs has 16, generate-ml has 7
  - 7 are shared (core types with complex dependencies)
  - 9 are Haskell-specific due to namespace conflicts

### Why Haskell needs more manual implementations

Haskell requires more manual implementations than OCaml primarily due to **namespace differences**:

1. **OCaml** has separate namespaces for:
   - Type names (lowercase)
   - Constructor names (Capitalized)
   - Module names (Capitalized)

2. **Haskell** has unified namespaces:
   - Type constructors and data constructors share the same namespace
   - This causes conflicts when a type name matches a variant constructor name

This is why types like `Field`, `TraitImpl`, `TraitMethod`, `Local`, `Assert`, `Call`, and `CopyNonOverlapping` need manual handling in Haskell but not in OCaml.

## Improvements Made

This PR reduced manual_json_impls from 7 to 1 by adding:

1. **serde(transparent) support**: Automatically handles single-field wrappers like `Name`, `TraitItemName`, `DeBruijnId`
2. **Struct variant support**: Automatically handles enum variants with named fields like `ItemSource`, `TraitRefKind`, `TagEncoding`

These improvements bring the Haskell code generation closer to the OCaml version's level of automation, while accounting for Haskell's namespace constraints.
