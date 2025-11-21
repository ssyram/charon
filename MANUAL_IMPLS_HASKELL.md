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

**Total manual_json_impls**: 1 (down from 7 originally)

## Manually Implemented Types (`manually_implemented`)

These types are excluded from automatic type and FromJSON instance generation:

### Core Types (shared with generate-ml)

#### ItemOpacity, PredicateOrigin, Ty
**Reason**: External/non-local types or special handling required

These are types that either:
- Come from external crates and aren't part of the charon AST  
- Require special deserialization logic not covered by the standard patterns
- Have complex dependencies that make automatic generation difficult

**In generate-ml**: Same types are manually implemented

### Haskell-Specific Types

#### Vector
**Reason**: Phantom type parameter causes conflicts

`Vector<I, T>` is a Rust type with two type parameters where `I` is a phantom type (index type) that doesn't affect the JSON representation. The type is serialized as a list of values (filtering out `None` entries), but generating a standard `FromJSON` instance for it would conflict with Haskell's built-in list instance.

**In generate-ml**: Also requires manual implementation with `filter_map` to handle `None` values

**Manual definition in template**: Type alias `type Vector k v = [v]` with manual FromJSON that ignores the phantom type parameter `k`

**Total manually_implemented**: 5 (down from 16 originally)

## Previously Manual, Now Automated

These types were previously manually implemented but are now automatically generated using module qualification to resolve naming conflicts:

### Body, FunDecl, TranslatedCrate (NOW AUTO-GENERATED!)
**Former reason**: Complex types with LLBC/ULLBC variant dependencies

These types were moved from manual implementation to the auto-generated `Generated_Krate` module. The circular dependency issue was resolved by:
1. Creating a dedicated `Krate` module that sits at the top of the dependency hierarchy
2. Auto-generating Body, FunDecl, and TranslatedCrate with full FromJSON instances
3. Using qualified imports (G., L., U.) to handle LLBC/ULLBC type references
4. **TranslatedCrate now properly handles fun_decls** with full deserialization!

**Solution**: Automatic generation with module separation
- Types auto-generated in Generated_Krate module (depends on all other modules)
- No circular dependencies: Meta → Values → Types → Expressions → GAst → LlbcAst/UllbcAst → Krate
- Body variants use qualified types: G.GexprBody (M.Vector U.BlockId U.Block) and G.GexprBody L.Block
- FromJSON instances handle all complex structures including HashMap fields serialized as arrays

**In generate-ml**: Same types are auto-generated

These types were previously manually implemented but are now automatically generated using module qualification to resolve naming conflicts:

### TraitImpl, TraitMethod, Local, Call, Assertion, CopyNonOverlapping
**Former reason**: Name conflicts between GAst structs and Types/Expressions module variants

These GAst struct types conflicted with:
- `TraitImpl` and `TraitMethod` variants in Types module (in `TraitRefKind` and `FnPtrKind`)
- `Local` variant in Meta/Types modules
- Potential conflicts for `Call`, `Assertion`, `CopyNonOverlapping`

**Solution**: Automatic generation with qualified module prefixes
- Types are auto-generated in their respective modules (Generated_GAst)
- FromJSON instances use `G.TypeName` prefix to disambiguate from Types module variants
- Template imports use `qualified Generated_GAst as G` to access these types

**Benefits**:
- Automatically tracks changes in Rust AST (the main risk highlighted by @ssyram)
- Correct field handling - e.g., `Call` was missing fields in manual impl, `Assertion` was missing `on_failure`
- Less maintenance burden

**In generate-ml**: These are not in the manual list because OCaml has separate namespaces

### Field
**Former reason**: Name conflict - exists in both Types and Expressions modules

**Solution**: Automatic generation with `T.` qualification
- Auto-generated in Generated_Types module
- FromJSON instance uses `T.Field` prefix to disambiguate from Expressions module variant

**In generate-ml**: No conflict due to separate namespaces

## Summary

### Comparison with generate-ml

- **manual_json_impls**: generate-hs has 1 (down from 7), generate-ml has 2
  - Both have Vector (filters None values)
  - ScalarValue in generate-hs requires dual String/Number parsing

- **manually_implemented**: generate-hs has 5 (down from 16!), generate-ml has 7
  - 4 are shared (ItemOpacity, PredicateOrigin, Ty, Vector)
  - Body, FunDecl, TranslatedCrate are now auto-generated in Haskell (still manual in ML)
  - 1 is Haskell-specific (TraitTypeConstraintId - marker trait)

### Why Haskell previously needed more manual implementations

Haskell required more manual implementations than OCaml primarily due to **namespace differences**:

1. **OCaml** has separate namespaces for:
   - Type names (lowercase)
   - Constructor names (Capitalized)
   - Module names (Capitalized)

2. **Haskell** has unified namespaces:
   - Type constructors and data constructors share the same namespace
   - This caused conflicts when a type name matched a variant constructor name

### How we resolved this

By using **module qualification with prefixes** (G., T., E., M.), we can now:
- Auto-generate types even when naming conflicts exist
- Use qualified imports to disambiguate: `G.TraitImpl` (struct) vs `T.TraitImpl` (variant)
- Keep definitions synchronized with Rust AST automatically

## Improvements Made

This implementation reduced:
- **manual_json_impls** from 7 to 1 (86% reduction)
- **manually_implemented** from 16 to 5 (69% reduction!)

Changes made:
1. Added serde(transparent) support (first PR phase)
2. Added struct variant support (first PR phase)
3. **Added module qualification for naming conflicts** (auto-generation phase):
   - Automatically qualifies GAst types with `G.` prefix when they conflict with Types variants
   - Automatically qualifies Types types with `T.` prefix when they conflict with Expressions variants
   - Removed 7 types from manually_implemented list: TraitImpl, TraitMethod, Local, Field, Call, Assertion, CopyNonOverlapping
4. **Created Krate module with full auto-generation** (final phase):
   - Extracted Body, FunDecl, TranslatedCrate to dedicated Krate module
   - Resolved circular dependency issues with proper module hierarchy
   - **TranslatedCrate now fully handles fun_decls with automatic deserialization!**
   - Removed 3 more types from manual list: Body, FunDecl, TranslatedCrate

These improvements bring the Haskell code generation significantly beyond the OCaml version's level of automation (5 manual vs 7), while safely handling Haskell's namespace constraints through module qualification and strategic module organization.
