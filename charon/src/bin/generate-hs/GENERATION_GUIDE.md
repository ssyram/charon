# Haskell Code Generation Guide

This document describes the Haskell code generation process and identifies potential issues that could cause generation to fail.

## Overview

The Haskell code generator (`generate-hs`) automatically generates Haskell type definitions and Aeson `FromJSON` instances from Charon's Rust AST types. The generated code is placed in `charon-hs/src/generated/`.

## Architecture

### Module Structure

The generated code is organized into modules that mirror the Rust AST structure:

- **Generated_Meta.hs**: Meta-information types (File, Span, AttrInfo, etc.)
- **Generated_Values.hs**: Literal values and primitive types
- **Generated_Types.hs**: Type system types (TypeDecl, TyKind, TraitImpl, etc.)
- **Generated_Expressions.hs**: Expression and operation types (Rvalue, Operand, Place, etc.)
- **Generated_GAst.hs**: Generic AST types (Call, Assert, GlobalDecl, TraitDecl, etc.)
- **Generated_LlbcAst.hs**: LLBC-specific AST types (Statement, Switch, etc.)
- **Generated_UllbcAst.hs**: ULLBC-specific AST types (Terminator, etc.)

Each module contains both:
1. Type declarations (data types and newtypes)
2. FromJSON instances for deserializing from JSON

## Vulnerable Points

### 1. Name Conflicts Between Modules

**Problem**: Haskell requires unambiguous names. If the same type or constructor name exists in multiple modules, you must use qualified imports or hiding clauses.

**Current Conflicts**:
- `Field`: Exists in both `Types` and `Expressions`
- `PtrMetadata`: Exists in both `Types` and `Expressions`
- `Local`: Exists in both `Meta` and `GAst`
- `TraitImpl`, `TraitMethod`: Exist in both `Types` and `GAst`
- `Call`, `CopyNonOverlapping`: Data types in `GAst`, variant constructors in `LlbcAst`/`UllbcAst`

**Solution**:
- Use `hiding` clauses in imports (e.g., `import Generated_Types hiding (Field)`)
- Use qualified imports (e.g., `import qualified Generated_GAst as G`)
- Qualify type references in generated code when needed

**Detection**: Haskell compiler will report "Ambiguous occurrence" errors.

**Prevention**:
- When adding new types to the Rust AST, check if the name already exists in other modules
- Run `grep "^data TypeName\|  | TypeName" charon-hs/src/generated/*.hs` to find conflicts
- The auto-detection logic in main.rs helps identify some conflicts automatically

### 2. Reserved Haskell Keywords

**Problem**: Some Rust identifiers may be Haskell keywords and cannot be used as type or constructor names.

**Current Handling**:
- The `make_haskell_ident` function handles some common cases
- Reserved words like "type", "data", "case" are suffixed with `_`

**Detection**: Haskell parser will report syntax errors.

**Prevention**:
- Update `make_haskell_ident` to handle new reserved words
- Consider using `#[charon::rename("AlternativeName")]` in Rust for problematic names

### 3. Circular Module Dependencies

**Problem**: If module A needs types from module B, and B needs types from A, you have a circular dependency that Haskell cannot resolve.

**Current Structure**: Dependencies flow in one direction:
```
Values → Meta → Types → Expressions → GAst → {LlbcAst, UllbcAst}
```

**Detection**: Haskell will report "Cycle in module dependencies" or "Trying to load interface for X while checking X".

**Prevention**:
- Maintain the hierarchical module structure
- Never add imports that create cycles
- If a type needs to be shared, move it to a lower-level module

### 4. Type Qualification in Generated Code

**Problem**: When generating type references in one module that point to types defined in another module, incorrect qualification can cause compilation errors.

**Current Solution**:
- `type_to_haskell_name` automatically qualifies certain types (e.g., `G.Call`, `G.CopyNonOverlapping`)
- This is needed when a type from module A is used in module B, but B hides that type's constructor

**Detection**: Compiler will report "Not in scope: type constructor X" or suggest adding to imports.

**Manual Fix Required**: If new cross-module type references are added:
1. Check if the referenced type conflicts with local names
2. If so, update `type_to_haskell_name` to qualify it
3. Or update the template's import list

### 5. Variant Constructor vs Type Name Conflicts

**Problem**: In Haskell, data constructors and type constructors are in different namespaces, but in some contexts (like pattern matching in FromJSON instances), they can be ambiguous.

**Example**:
```haskell
-- In GAst module:
data Call = Call { ... }  -- Both type and constructor named "Call"

-- In LlbcAst module:
data StatementKind = Call Call | ...  
-- First "Call" is variant constructor, second "Call" is type from GAst
```

**Detection**: Compiler reports "Ambiguous occurrence" in FromJSON instances.

**Solution**: 
- Hide the conflicting constructor from imports: `import Generated_GAst hiding (Call)`
- Qualify the type in generated code: use `G.Call` for the type

### 6. Missing Aeson Imports

**Problem**: The generated FromJSON instances use Aeson functions (withObject, parseJSON, (.:), etc.) that must be imported.

**Required Imports**:
```haskell
import Data.Aeson
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
```

**Detection**: Compiler will report "Variable not in scope: parseJSON" etc.

**Prevention**: All templates include these imports by default.

### 7. Orphan Instances

**Problem**: Defining a type in module A and its FromJSON instance in module B creates an "orphan instance" which Haskell warns about.

**Solution**: We merged the type declarations and FromJSON instances into the same modules. Each `XX.hs` file now contains both type definitions and their FromJSON instances.

**Pragma**: Use `{-# OPTIONS_GHC -Wno-orphans #-}` only if truly necessary (not currently needed).

### 8. Template Marker Mismatches

**Problem**: Each template has markers like `{- __REPLACE0__ -}` and `{- __REPLACE1__ -}` that are replaced with generated code. If the number of markers doesn't match the number of generation passes, code will be lost or duplicated.

**Current Structure**:
- `__REPLACE0__`: Type declarations
- `__REPLACE1__`: FromJSON instances

**Detection**: Manual inspection or missing types/instances in generated code.

**Prevention**: Always ensure templates have the correct number of markers matching the `markers` vector in main.rs.

### 9. Complex Type Mappings

**Problem**: Some Rust types need special handling when converting to Haskell.

**Special Cases**:
- `Vec<T>` → `[T]` (Haskell list)
- `Option<T>` → `Maybe T`
- `String`, `Ustr` → `Text`
- `Vector<K, V>` → `Vector K V` (custom newtype)
- `Box<T>` → `T` (unwrapped)

**Detection**: Type errors in Haskell code or incorrect deserialization.

**Manual Fix**: Update `type_to_haskell_name` function to handle new special cases.

### 10. Renamed Types

**Problem**: Some Rust types are renamed for Haskell using `#[charon::rename("NewName")]`. The generator must use the renamed name, not the original.

**Current Handling**: `type_name_to_haskell_ident` checks for `#[charon::rename]` attributes.

**Detection**: "Not in scope: type constructor OriginalName".

**Prevention**: Always check for rename attributes before using type names.

## Testing the Generation

After making changes to the generator or Rust AST:

```bash
# Clean build
make generate-hs

# Check for Rust warnings in the generator
cd charon && cargo build --bin generate-hs

# Test the generated Haskell code
make test-hs

# Verify no warnings (should be clean)
cd charon-hs && stack build
```

## Debugging Generation Issues

### If Generation Fails:

1. **Check the error message**: Often indicates which type or module has the issue
2. **Examine the generated file**: Look at the specific generated code around the error
3. **Check for new types**: Use `git diff` to see what changed in the Rust AST
4. **Review imports**: Ensure all necessary types are imported and conflicts are resolved

### Common Fixes:

- **"Not in scope"**: Add missing import or qualify the name
- **"Ambiguous occurrence"**: Add hiding clause or use qualified names
- **"Cycle in module dependencies"**: Reorder imports or move types
- **"orphan instance"**: Move instance to same module as type definition

## Auto-Detection Features

The generator includes automatic conflict detection:

### Variant Name Conflicts
The generator collects variant constructor names from each module and detects conflicts with type names in other modules.

**Limitations**: Currently only detects conflicts between:
- GAst types ↔ Types/Expressions/Meta variant names
- Types types ↔ Expressions types/variants

### Adding New Conflict Detection

To add detection for new modules:
1. Add variant collection in the conflict detection section of main.rs
2. Update the qualification logic in `type_to_haskell_name` if needed
3. Update template imports if hiding/qualification is required

## Best Practices

1. **Keep module hierarchy clean**: Avoid circular dependencies
2. **Use qualified imports**: When in doubt, use `qualified` imports for clarity
3. **Test incrementally**: After adding new types, regenerate and test immediately
4. **Document conflicts**: Update this guide when new conflicts are discovered
5. **Use hiding judiciously**: Only hide what's necessary to avoid confusion
6. **Leverage auto-detection**: The conflict detection helps but isn't exhaustive

## Future Improvements

Potential enhancements to make generation more robust:

1. **Full conflict detection**: Automatically detect ALL naming conflicts, not just subset
2. **Context-aware generation**: Know which module is being generated to qualify correctly
3. **Automated import management**: Generate optimal import lists automatically
4. **Better error messages**: Provide hints on how to fix conflicts
5. **Validation step**: Check generated Haskell code before writing to files
