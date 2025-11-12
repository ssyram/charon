# Charon Haskell Bindings

This directory contains auto-generated Haskell bindings for Charon's LLBC (Low-Level Borrow Calculus) AST.

## Overview

The Haskell types and Aeson `FromJSON` instances in `src/generated/` are automatically generated from the Rust type definitions in `charon/src/ast/`. This allows you to deserialize the `.llbc` JSON files produced by Charon into Haskell data structures for static analysis, verification, or transformation.


## Generated Modules

The generation process creates the following modules:

- `Generated_Meta` - Metadata types (file information, spans, attributes)
- `Generated_Values` - Primitive values and literals
- `Generated_Types` - Type system representation (types, generics, traits)
- `Generated_Expressions` - Expression and operand types
- `Generated_GAst` - Generic AST components (function signatures, declarations)
- `Generated_LlbcAst` - LLBC-specific AST nodes
- `Generated_UllbcAst` - Unstructured LLBC AST nodes
- `Generated_GAstOfJson` - JSON deserializers for generic AST
- `Generated_LlbcOfJson` - JSON deserializers for LLBC
- `Generated_UllbcOfJson` - JSON deserializers for unstructured LLBC

## Regenerating the Code

To regenerate the Haskell bindings after changes to the Rust AST:

```bash
# From the repository root
make generate-hs

# Or to skip re-running charon on itself (faster during development)
make generate-hs-keep-llbc
```

## Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (eitherDecodeFileStrict)
import Generated_GAst
import Generated_Types
-- ... other imports

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "path/to/crate.llbc"
  case result of
    Left err -> putStrLn $ "Failed to parse LLBC: " ++ err
    Right crate -> do
      -- Work with the parsed TranslatedCrate
      print crate
```

## Dependencies

The generated code requires:

- `aeson` - JSON parsing
- `text` - Text types
- `unordered-containers` - HashMap for JSON objects

## Note

**DO NOT** manually edit the files in `src/generated/`. They are automatically generated and will be overwritten. If you need to make changes:

1. Edit the template files in `charon/src/bin/generate-hs/templates/`
2. Or improve the code generation in `charon/src/bin/generate-hs/main.rs`
3. Then regenerate using `make generate-hs`
