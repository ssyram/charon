{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_GAst.hs`
by hand. Edit `templates/GAst.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_GAst where

import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Generated_Meta
import Generated_Values
import Generated_Types
import Generated_Expressions

-- Manually defined types

data TargetInfo = TargetInfo
  { targetinfoTargetPointerSize :: Int
  , targetinfoIsLittleEndian :: Bool
  }
  deriving (Show, Eq, Ord)

-- Types with name conflicts - defined manually to avoid issues with Types module
data Local = Local
  { localIndex :: LocalId
  , localName :: (Maybe String)
  , localLocalTy :: Ty
  }
  deriving (Show, Eq, Ord)

data TraitImpl = TraitImpl
  { traitimplDefId :: TraitImplId
  , traitimplItemMeta :: ItemMeta
  , traitimplImplTrait :: TraitDeclRef
  , traitimplGenerics :: GenericParams
  , traitimplImpliedTraitRefs :: Vector TraitClauseId TraitRef
  , traitimplConsts :: [(TraitItemName, GlobalDeclRef)]
  , traitimplTypes :: [(TraitItemName, (Binder TraitAssocTyImpl))]
  , traitimplMethods :: [(TraitItemName, (Binder FunDeclRef))]
  , traitimplVtable :: (Maybe GlobalDeclRef)
  }
  deriving (Show, Eq, Ord)

data TraitMethod = TraitMethod
  { traitmethodName :: TraitItemName
  , traitmethodItem :: FunDeclRef
  }
  deriving (Show, Eq, Ord)

data Assertion = Assertion
  { assertionCond :: Operand
  , assertionExpected :: Bool
  }
  deriving (Show, Eq, Ord)

data Call = Call
  { callFunc :: FnOperand
  , callGenerics :: GenericArgs
  , callArgs :: [Operand]
  , callDest :: Place
  }
  deriving (Show, Eq, Ord)

data CopyNonOverlapping = CopyNonOverlapping
  { copysrc :: Operand
  , copydst :: Operand
  , copycount :: Operand
  }
  deriving (Show, Eq, Ord)

data CliOptions = CliOptions
  {   -- | Extract the unstructured LLBC (i.e., don't reconstruct the control-flow)
  clioptionsUllbc :: Bool
  ,   -- | Compile the package's library
  clioptionsLib :: Bool
  ,   -- | Compile the specified binary
  clioptionsBin :: Maybe String
  ,   -- | Deprecated: use `--mir promoted` instead.
  clioptionsMirPromoted :: Bool
  ,   -- | Deprecated: use `--mir optimized` instead.
  clioptionsMirOptimized :: Bool
  ,   -- | The MIR stage to extract. This is only relevant for the current crate; for dpendencies only
  -- | MIR optimized is available.
  clioptionsMir :: Maybe MirLevel
  ,   -- | The input file (the entry point of the crate to extract).
  -- | This is needed if you want to define a custom entry point (to only
  -- | extract part of a crate for instance).
  clioptionsInputFile :: Maybe PathBuf
  ,   -- | Read an llbc file and pretty-print it. This is a terrible API, we should use subcommands.
  clioptionsReadLlbc :: Maybe PathBuf
  ,   -- | The destination directory. Files will be generated as `<dest_dir>/<crate_name>.{u}llbc`,
  -- | unless `dest_file` is set. `dest_dir` defaults to the current directory.
  clioptionsDestDir :: Maybe PathBuf
  ,   -- | The destination file. By default `<dest_dir>/<crate_name>.llbc`. If this is set we ignore
  -- | `dest_dir`.
  clioptionsDestFile :: Maybe PathBuf
  ,   -- | If activated, use Polonius' non-lexical lifetimes (NLL) analysis.
  -- | Otherwise, use the standard borrow checker.
  clioptionsUsePolonius :: Bool
  ,   -- | If activated, this skips borrow-checking of the crate.
  clioptionsSkipBorrowck :: Bool
  ,   -- | Monomorphize the items encountered when possible. Generic items found in the crate are
  -- | skipped. To only translate a particular call graph, use `--start-from`. Note: this doesn't
  -- | currently support `dyn Trait`.
  clioptionsMonomorphize :: Bool
  ,   -- | Partially monomorphize items to make it so that no item is ever monomorphized with a
  -- | mutable reference (or type containing one); said differently, so that the presence of
  -- | mutable references in a type is independent of its generics. This is used by Aeneas.
  clioptionsMonomorphizeMut :: Maybe MonomorphizeMut
  ,   -- | Usually we skip the bodies of foreign methods and structs with private fields. When this
  -- | flag is on, we don't.
  clioptionsExtractOpaqueBodies :: Bool
  ,   -- | Usually we skip the provided methods that aren't used. When this flag is on, we translate
  -- | them all.
  clioptionsTranslateAllMethods :: Bool
  ,   -- | Whitelist of items to translate. These use the name-matcher syntax.
  clioptionsIncluded :: [String]
  ,   -- | Blacklist of items to keep opaque. These use the name-matcher syntax.
  clioptionsOpaque :: [String]
  ,   -- | Blacklist of items to not translate at all. These use the name-matcher syntax.
  clioptionsExclude :: [String]
  ,   -- | List of traits for which we transform associated types to type parameters.
  clioptionsRemoveAssociatedTypes :: [String]
  ,   -- | Whether to hide various marker traits such as `Sized`, `Sync`, `Send` and `Destruct`
  -- | anywhere they show up.
  clioptionsHideMarkerTraits :: Bool
  ,   -- | Remove trait clauses from type declarations. Must be combined with
  -- | `--remove-associated-types` for type declarations that use trait associated types in their
  -- | fields, otherwise this will result in errors.
  clioptionsRemoveAdtClauses :: Bool
  ,   -- | Hide the `A` type parameter on standard library containers (`Box`, `Vec`, etc).
  clioptionsHideAllocator :: Bool
  ,   -- | Trait method declarations take a `Self: Trait` clause as parameter, so that they can be
  -- | reused by multiple trait impls. This however causes trait definitions to be mutually
  -- | recursive with their method declarations. This flag removes `Self` clauses that aren't used
  -- | to break this mutual recursion.
  clioptionsRemoveUnusedSelfClauses :: Bool
  ,   -- | Whether to add `Destruct` bounds everywhere to enable proper tracking of what code runs on
  -- | a given `drop` call.
  clioptionsAddDropBounds :: Bool
  ,   -- | A list of item paths to use as starting points for the translation. We will translate these
  -- | items and any items they refer to, according to the opacity rules. When absent, we start
  -- | from the path `crate` (which translates the whole crate).
  clioptionsStartFrom :: [String]
  ,   -- | Do not run cargo; instead, run the driver directly.
  clioptionsNoCargo :: Bool
  ,   -- | Extra flags to pass to rustc.
  clioptionsRustcArgs :: [String]
  ,   -- | Extra flags to pass to cargo. Incompatible with `--no-cargo`.
  clioptionsCargoArgs :: [String]
  ,   -- | Panic on the first error. This is useful for debugging.
  clioptionsAbortOnError :: Bool
  ,   -- | Print the errors as warnings
  clioptionsErrorOnWarnings :: Bool
  , clioptionsNoSerialize :: Bool
  , clioptionsPrintOriginalUllbc :: Bool
  , clioptionsPrintUllbc :: Bool
  , clioptionsPrintBuiltLlbc :: Bool
  , clioptionsPrintLlbc :: Bool
  , clioptionsNoMergeGotoChains :: Bool
  , clioptionsNoOpsToFunctionCalls :: Bool
  , clioptionsRawBoxes :: Bool
  ,   -- | Named builtin sets of options. Currently used only for dependent projects, eveentually
  -- | should be replaced with semantically-meaningful presets.
  clioptionsPreset :: Maybe Preset
  }
  deriving (Show, Eq, Ord)

-- | A (group of) top-level declaration(s), properly reordered.
data DeclarationGroup = TypeGroup ((GDeclarationGroup TypeDeclId))
  | FunGroup ((GDeclarationGroup FunDeclId))
  | GlobalGroup ((GDeclarationGroup GlobalDeclId))
  | TraitDeclGroup ((GDeclarationGroup TraitDeclId))
  | TraitImplGroup ((GDeclarationGroup TraitImplId))
  | MixedGroup ((GDeclarationGroup ItemId))
  deriving (Show, Eq, Ord)

-- | A function operand is used in function calls.
-- | It either designates a top-level function, or a place in case
-- | we are using function pointers stored in local variables.
data FnOperand = FnOpRegular FnPtr
  | FnOpMove Place
  deriving (Show, Eq, Ord)

-- | A function signature.
data FunSig = FunSig
  {   -- | Is the function unsafe or not
  funsigIsUnsafe :: Bool
  , funsigGenerics :: GenericParams
  , funsigInputs :: [Ty]
  , funsigOutput :: Ty
  }
  deriving (Show, Eq, Ord)

-- | A (group of) top-level declaration(s), properly reordered.
-- | "G" stands for "generic"
data GDeclarationGroup a0 = NonRecGroup a0
  | RecGroup [a0]
  deriving (Show, Eq, Ord)

-- | An expression body.
-- | TODO: arg_count should be stored in GFunDecl below. But then,
-- |       the print is obfuscated and Aeneas may need some refactoring.
data GexprBody a0 = GexprBody
  { gexprbodySpan :: Span
  ,   -- | The local variables.
  gexprbodyLocals :: Locals
  , gexprbodyBody :: a0
  }
  deriving (Show, Eq, Ord)

-- | A global variable definition (constant or static).
data GlobalDecl = GlobalDecl
  { globaldeclDefId :: GlobalDeclId
  ,   -- | The meta data associated with the declaration.
  globaldeclItemMeta :: ItemMeta
  , globaldeclGenerics :: GenericParams
  , globaldeclTy :: Ty
  ,   -- | The context of the global: distinguishes top-level items from trait-associated items.
  globaldeclSrc :: ItemSource
  ,   -- | The kind of global (static or const).
  globaldeclGlobalKind :: GlobalKind
  ,   -- | The initializer function used to compute the initial value for this constant/static. It
  -- | uses the same generic parameters as the global.
  globaldeclInit :: FunDeclId
  }
  deriving (Show, Eq, Ord)

data GlobalKind = Static
  | NamedConst
  | AnonConst
  deriving (Show, Eq, Ord)

-- | The local variables of a body.
data Locals = Locals
  {   -- | The number of local variables used for the input arguments.
  localsArgCount :: Int
  ,   -- | The local variables.
  -- | We always have, in the following order:
  -- | - the local used for the return value (index 0)
  -- | - the `arg_count` input arguments
  -- | - the remaining locals, used for the intermediate computations
  localsLocals :: (Vector LocalId Local)
  }
  deriving (Show, Eq, Ord)

-- | The MIR stage to use. This is only relevant for the current crate: for dependencies, only mir
-- | optimized is available (or mir elaborated for consts).
data MirLevel = Built
  | Promoted
  | Elaborated
  | Optimized
  deriving (Show, Eq, Ord)

data MonomorphizeMut = All
  | ExceptTypes
  deriving (Show, Eq, Ord)

-- | Presets to make it easier to tweak options without breaking dependent projects. Eventually we
-- | should define semantically-meaningful presets instead of project-specific ones.
data Preset = OldDefaults
  | Aeneas
  | Eurydice
  | Soteria
  | Tests
  deriving (Show, Eq, Ord)

-- | An associated constant in a trait.
data TraitAssocConst = TraitAssocConst
  { traitassocconstName :: TraitItemName
  , traitassocconstTy :: Ty
  , traitassocconstDefault :: Maybe GlobalDeclRef
  }
  deriving (Show, Eq, Ord)

-- | An associated type in a trait.
data TraitAssocTy = TraitAssocTy
  { traitassoctyName :: TraitItemName
  , traitassoctyDefault :: Maybe Ty
  ,   -- | List of trait clauses that apply to this type.
  traitassoctyImpliedClauses :: (Vector TraitClauseId TraitParam)
  }
  deriving (Show, Eq, Ord)

-- | A trait **declaration**.
-- | 
-- | For instance:
-- | ```text
-- | trait Foo {
-- |   type Bar;
-- | 
-- |   fn baz(...); // required method (see below)
-- | 
-- |   fn test() -> bool { true } // provided method (see below)
-- | }
-- | ```
-- | 
-- | In case of a trait declaration, we don't include the provided methods (the methods
-- | with a default implementation): they will be translated on a per-need basis. This is
-- | important for two reasons:
-- | - this makes the trait definitions a lot smaller (the Iterator trait
-- |   has *one* declared function and more than 70 provided functions)
-- | - this is important for the external traits, whose provided methods
-- |   often use features we don't support yet
-- | 
-- | Remark:
-- | In Aeneas, we still translate the provided methods on an individual basis,
-- | and in such a way thay they take as input a trait instance. This means that
-- | we can use default methods *but*:
-- | - implementations of required methods shoudln't call default methods
-- | - trait implementations shouldn't redefine required methods
-- | The use case we have in mind is [std::iter::Iterator]: it declares one required
-- | method (`next`) that should be implemented for every iterator, and defines many
-- | helpers like `all`, `map`, etc. that shouldn't be re-implemented.
-- | Of course, this forbids other useful use cases such as visitors implemented
-- | by means of traits.
data TraitDecl = TraitDecl
  { traitdeclDefId :: TraitDeclId
  , traitdeclItemMeta :: ItemMeta
  , traitdeclGenerics :: GenericParams
  ,   -- | The "parent" clauses: the supertraits.
  -- | 
  -- | Supertraits are actually regular where clauses, but we decided to have
  -- | a custom treatment.
  -- | ```text
  -- | trait Foo : Bar {
  -- |             ^^^
  -- |         supertrait, that we treat as a parent predicate
  -- | }
  -- | ```
  -- | TODO: actually, as of today, we consider that all trait clauses of
  -- | trait declarations are parent clauses.
  traitdeclImpliedClauses :: (Vector TraitClauseId TraitParam)
  ,   -- | The associated constants declared in the trait.
  traitdeclConsts :: [TraitAssocConst]
  ,   -- | The associated types declared in the trait. The binder binds the generic parameters of the
  -- | type if it is a GAT (Generic Associated Type). For a plain associated type the binder binds
  -- | nothing.
  traitdeclTypes :: [(Binder TraitAssocTy)]
  ,   -- | The methods declared by the trait. The binder binds the generic parameters of the method.
  -- | 
  -- | ```rust
  -- | trait Trait<T> {
  -- |   // The `Binder` for this method binds `'a` and `U`.
  -- |   fn method<'a, U>(x: &'a U);
  -- | }
  -- | ```
  traitdeclMethods :: [(Binder TraitMethod)]
  ,   -- | The virtual table struct for this trait, if it has one.
  -- | It is guaranteed that the trait has a vtable iff it is dyn-compatible.
  traitdeclVtable :: Maybe TypeDeclRef
  }
  deriving (Show, Eq, Ord)
