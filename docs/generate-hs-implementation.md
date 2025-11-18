# Implementation Summary: generate-hs

## Overview

This document summarizes the implementation of the `generate-hs` functionality, which generates Haskell bindings for Charon's LLBC AST.

## Analysis of generate-ml

The `generate-ml` binary works by:
1. Running Charon on itself to produce LLBC JSON output
2. Parsing the JSON to extract type definitions
3. Generating OCaml types and `*_of_json` functions
4. Replacing markers in template files with generated code
5. Writing the final modules to `charon-ml/src/generated/`

## Implementation Approach

The `generate-hs` implementation follows the same architecture:

### 1. Code Generation (main.rs)

**Key Components:**
- `GenerateCtx` - Context holding type information and mappings
- `type_to_haskell_name()` - Converts Rust types to Haskell types
- `type_decl_to_haskell_decl()` - Generates Haskell data declarations
- `type_decl_to_json_deserializer()` - Generates Aeson FromJSON instances

**Type Conversions:**
- `Option<T>` → `Maybe T`
- `String` → `Text`
- `Vec<T>` → `[T]`
- `Box<T>` → `T`
- Tuples preserved as Haskell tuples
- Rust enums → Haskell sum types
- Rust structs → Haskell record types

### 2. Template System

Template files in `charon/src/bin/generate-hs/templates/` contain:
- Module headers and imports
- Manual type definitions (e.g., `type FileId = Text`)
- `{- __REPLACEn__ -}` markers for generated code

### 3. Generated Modules

The system generates 10 Haskell modules:

**Type Declarations:**
- `Generated_Meta.hs` - File, Span, AttrInfo
- `Generated_Values.hs` - Literal, IntegerType, ScalarValue
- `Generated_Types.hs` - TypeVar, TyKind, TraitRef, TypeDecl
- `Generated_Expressions.hs` - Rvalue, Operand
- `Generated_GAst.hs` - Call, Assert, FunSig, GlobalDecl, TraitDecl
- `Generated_LlbcAst.hs` - LLBC-specific statements
- `Generated_UllbcAst.hs` - Unstructured LLBC statements

**JSON Deserializers:**
- `Generated_GAstOfJson.hs`
- `Generated_LlbcOfJson.hs`
- `Generated_UllbcOfJson.hs`

### 4. Build Integration

Added to Makefile:
```makefile
.PHONY: generate-hs
generate-hs:
	cd charon && cargo build --release && cargo run --release --bin generate-hs

.PHONY: generate-hs-keep-llbc
generate-hs-keep-llbc:
	CHARON_HS_REUSE_LLBC=1 $(MAKE) generate-hs
```

## Key Design Decisions

1. **Type Safety**: All generated types derive Show, Eq, and Ord for maximum utility
2. **Idiomatic Haskell**: Use Text instead of String, Maybe instead of Option
3. **Consistency**: Mirror the structure and approach of generate-ml
4. **Manual Overrides**: Support manual type implementations for special cases
5. **Template-based**: Use markers in templates for maintainability

## Testing

The implementation was tested by:
1. Building the generate-hs binary
2. Running it to generate Haskell code
3. Inspecting generated types and instances
4. Verifying proper type conversions
5. Checking module imports and exports

## Generated Code Statistics

- 10 Haskell modules created
- ~4,500 lines of generated code
- Covers all major AST types from Charon
- Full FromJSON instance support

## Usage Example

```haskell
import Data.Aeson (eitherDecodeFileStrict)
import Generated_GAst
import Generated_Types

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "crate.llbc"
  case result of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right crate -> analyzeCrate crate

analyzeCrate :: TranslatedCrate -> IO ()
analyzeCrate crate = do
  -- Work with the parsed LLBC AST
  print $ length (typeDeclsTranslatedCrate crate)
```

## Future Improvements

Potential enhancements:
1. Add ToJSON instances for round-trip serialization
2. Generate lenses for easier field access
3. Add pretty-printing instances
4. Generate Generic instances for derivation
5. Improve formatting of generated code
6. Add validation of parsed data

## Conclusion

The `generate-hs` implementation successfully provides Haskell developers with auto-generated bindings to work with Charon's LLBC JSON output, enabling static analysis, verification, and transformation of Rust programs in Haskell.
