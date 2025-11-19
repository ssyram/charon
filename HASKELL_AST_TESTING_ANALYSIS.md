# Haskell AST Testing Infrastructure Analysis

## Issue Summary

The Haskell AST and its testing infrastructure have a critical design flaw that allows the generated Haskell code to become severely out of sync with the Rust definitions without any test failures.

## The Problem

After merging the latest changes from upstream/main (which moved `Drop` from `StatementKind` to `TerminatorKind` in ULLBC and added a new CLI option `clioptionsDesugarDrops`), all tests passed **without running `make generate-hs`** to regenerate the Haskell AST.

This is **NOT reasonable** - it indicates the tests are not actually validating that the JSON can be properly parsed into the Haskell AST types.

## Root Causes

### 1. Tests Only Check LLBC Files, Not ULLBC Files

The test suite in `charon-hs/test/Test/Deserialize.hs` only searches for and tests `.llbc` files:

```haskell
findLlbcFiles :: FilePath -> IO [FilePath]
findLlbcFiles dir = do
  -- ... searches for files with .llbc extension
  let llbcFiles = filter (\f -> takeExtension f == ".llbc") files
```

**No ULLBC files are tested**, so changes to ULLBC AST structure go undetected.

### 2. Aeson Silently Ignores Unknown JSON Fields

Aeson's `FromJSON` parser, by default, silently ignores any JSON fields that aren't defined in the Haskell data type. This means:
- Missing Haskell fields → no error (JSON data is discarded)
- Extra JSON fields → no error (they're simply ignored)
- Mismatched structure → only detected when trying to parse a field that exists in both

### 3. Critical Fields Are Missing from Generated Haskell Types

The `TranslatedCrate` type in `Generated_GAst.hs` is missing essential fields:

```haskell
-- Current Haskell definition (INCOMPLETE):
data TranslatedCrate = TranslatedCrate
  { translatedCrateCrate_name :: String
  , translatedCrateType_decls :: Vector TypeDeclId TypeDecl
  , translatedCrateGlobal_decls :: Vector GlobalDeclId GlobalDecl
  , translatedCrateTrait_decls :: Vector TraitDeclId TraitDecl
  , translatedCrateTrait_impls :: Vector TraitImplId TraitImpl
  }

-- What JSON actually contains:
{
  "crate_name": "...",
  "fun_decls": [...],      // ← MISSING in Haskell!
  "type_decls": [...],
  "global_decls": [...],
  "trait_decls": [...],
  "trait_impls": [...],
  "files": [...],          // ← MISSING in Haskell!
  "item_names": {...},     // ← MISSING in Haskell!
  "options": {...},        // ← MISSING in Haskell!
  "ordered_decls": [...],  // ← MISSING in Haskell!
  "short_names": {...},    // ← MISSING in Haskell!
  "target_information": {...}, // ← MISSING in Haskell!
  "unit_metadata": {...}   // ← MISSING in Haskell!
}
```

Since `fun_decls` is missing, **function bodies are never parsed**, which means:
- The `Drop` terminator mismatch is never detected
- Any issues in function body parsing go unnoticed
- Tests pass even though the AST is incomplete

## Specific Mismatches Found

### 1. Drop Location Mismatch

**Rust (`charon/src/ast/ullbc_ast.rs`):**
```rust
pub enum TerminatorKind {
    Goto { target: BlockId },
    Switch { discr: Operand, targets: SwitchTargets },
    Call { call: Call, target: BlockId, on_unwind: BlockId },
    Drop { place: Place, tref: TraitRef },  // ← HERE
    Abort(AbortKind),
    Return,
    UnwindResume,
}

pub enum StatementKind {
    Assign(Place, Rvalue),
    SetDiscriminant(Place, VariantId),
    // ... Drop is NOT here anymore
}
```

**Haskell (`Generated_UllbcAst.hs`):**
```haskell
data StatementKind = Assign Place Rvalue
  | SetDiscriminant Place VariantId
  | CopyNonOverlapping G.CopyNonOverlapping
  | StorageLive LocalId
  | StorageDead LocalId
  | Deinit Place
  | Drop Place TraitRef    -- ← Still HERE (WRONG!)
  | Assert Assertion
  | Nop

data TerminatorKind = Goto BlockId
  | Switch Operand Switch
  | Call G.Call BlockId BlockId
  | Abort AbortKind
  | Return
  | UnwindResume
  -- Drop is NOT here (should be!)
```

### 2. Missing CLI Option

**Rust (`charon/src/options.rs`):**
```rust
pub struct CliOpts {
    // ... other fields ...
    #[clap(long = "desugar-drops")]
    #[serde(default)]
    pub desugar_drops: bool,  // ← NEW FIELD
}
```

**Haskell (`Generated_GAst.hs`):**
```haskell
data CliOptions = CliOptions
  { clioptionsUllbc :: Bool
  -- ... many other fields ...
  , clioptionsRawBoxes :: Bool
  , clioptionsPreset :: Maybe Preset
  -- clioptionsDesugarDrops is MISSING!
  }
```

## Why This Wasn't Caught

1. **LLBC vs ULLBC**: LLBC (Low-Level Borrow-Checked) files have already undergone control-flow reconstruction, converting ULLBC's (Unstructured LLBC) terminator-based Drop into something else. So LLBC tests don't exercise ULLBC-specific structures.

2. **Lenient Parsing**: The JSON parser doesn't fail on:
   - Extra fields in JSON that aren't in Haskell type (silently ignored)
   - Missing Haskell fields if they can be parsed from different JSON structure
   - The `clioptionsDesugarDrops` field in JSON is simply ignored

3. **Lazy Evaluation**: Even if function declarations were parsed, Haskell's lazy evaluation means the function bodies might not be fully evaluated unless explicitly forced.

4. **No ULLBC Generation in Tests**: The test infrastructure doesn't generate or test `.ullbc` files, only `.llbc` files.

## Proof of Concept

A test was added to parse a ULLBC file with a Drop terminator:

```haskell
test_ullbc_drop_file :: Assertion
test_ullbc_drop_file = do
  result <- eitherDecodeFileStrict "test/data/test_ullbc_drop.ullbc" 
            :: IO (Either String LlbcFile)
  -- This SHOULD fail but doesn't because fun_decls is not parsed
```

The test passes because the parser never attempts to parse function bodies.

## Recommendations

### Immediate Actions

1. **Run `make generate-hs`** to regenerate the Haskell AST from current Rust definitions

2. **Add comprehensive tests** that:
   - Test ULLBC files, not just LLBC files
   - Force evaluation of all parsed structures (including function bodies)
   - Validate field completeness

3. **Use stricter JSON parsing**:
   ```haskell
   -- Consider using rejectUnknownFields or manual validation
   -- to detect when JSON has fields not in Haskell type
   ```

### Long-term Improvements

1. **Add CI check** that fails if generated files are out of sync:
   ```bash
   make generate-hs
   git diff --exit-code charon-hs/src/generated/
   ```

2. **Generate test files** for both LLBC and ULLBC formats in the test suite

3. **Add schema validation** that ensures Haskell types cover all fields in JSON

4. **Document the generation process** and make it part of the regular development workflow

5. **Consider compile-time checks** using Template Haskell or similar to validate that generated types match expected structure

## Conclusion

The testing infrastructure has three critical flaws:
1. It only tests LLBC, not ULLBC
2. It doesn't parse function bodies (missing `fun_decls`)
3. It silently ignores schema mismatches

These combine to create a situation where the generated Haskell AST can be severely out of sync with Rust definitions without any test failures. This is a **significant risk** for the project as it means type safety guarantees are illusory - the code compiles and tests pass, but runtime parsing of actual ULLBC files would fail.

The solution is to:
1. Regenerate the Haskell AST immediately
2. Add proper ULLBC testing
3. Implement stricter JSON parsing validation
4. Add CI checks to prevent this from happening again
