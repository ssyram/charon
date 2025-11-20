{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Types.hs`
by hand. Edit `templates/Types.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Types where

import Data.Aeson (FromJSON(..), Value(..), withObject, withArray, (.:), (.!=))
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import qualified Generated_Meta as M
import {-# SOURCE #-} qualified Generated_Values as Val

-- Re-export commonly used types for convenience
type Vector = M.Vector

-- Manually defined type aliases and newtypes
-- TraitTypeConstraintId is a newtype wrapper around Int
newtype TraitTypeConstraintId = TraitTypeConstraintId { traittypeconstraintidRaw :: Int }
  deriving (Show, Eq, Ord)

-- | (U)LLBC is a language with side-effects: a statement may abort in a way that isn't tracked by
-- | control-flow. The two kinds of abort are:
-- | - Panic (may unwind or not depending on compilation setting);
-- | - Undefined behavior:
data AbortKind = Panic (Maybe Name)
  | UndefinedBehavior
  | UnwindTerminate
  deriving (Show, Eq, Ord)

-- | Describes modifiers to the alignment and packing of the corresponding type.
-- | Represents `repr(align(n))` and `repr(packed(n))`.
data AlignmentModifier = Align Int
  | Pack Int
  deriving (Show, Eq, Ord)

-- | A value of type `T` bound by generic parameters. Used in any context where we're adding generic
-- | parameters that aren't on the top-level item, e.g. `for<'a>` clauses (uses `RegionBinder` for
-- | now), trait methods, GATs (TODO).
data Binder a0 = Binder
  { binderBinderParams :: GenericParams
  ,   -- | Named this way to highlight accesses to the inner value that might be handling parameters
  -- | incorrectly. Prefer using helper methods.
  binderBinderValue :: a0
  }
  deriving (Show, Eq, Ord)

data BinderKind = BkTraitType G.TraitDeclId G.TraitItemName
  | BkTraitMethod G.TraitDeclId G.TraitItemName
  | BkInherentImplBlock
  | BkDyn
  | BkOther
  deriving (Show, Eq, Ord)

-- | An built-in function identifier, identifying a function coming from a
-- | standard library.
data BuiltinFunId = BoxNew
  | ArrayToSliceShared
  | ArrayToSliceMut
  | ArrayRepeat
  | Index E.BuiltinIndexOp
  | PtrFromParts RefKind
  deriving (Show, Eq, Ord)

-- | Describes a built-in impl. Mostly lists the implemented trait, sometimes with more details
-- | about the contents of the implementation.
data BuiltinImplData = BuiltinSized
  | BuiltinMetaSized
  | BuiltinTuple
  | BuiltinSend
  | BuiltinSync
  | BuiltinPointee
  | BuiltinDiscriminantKind
  | BuiltinUnpin
  | BuiltinFreeze
  | BuiltinNoopDestruct
  | BuiltinUntrackedDestruct
  | BuiltinFn
  | BuiltinFnMut
  | BuiltinFnOnce
  | BuiltinCopy
  | BuiltinClone
  deriving (Show, Eq, Ord)

-- | One of 8 built-in indexing operations.
data BuiltinIndexOp = BuiltinIndexOp
  {   -- | Whether this is a slice or array.
  builtinindexopIsArray :: Bool
  ,   -- | Whether we're indexing mutably or not. Determines the type ofreference of the input and
  -- | output.
  builtinindexopMutability :: RefKind
  ,   -- | Whether we're indexing a single element or a subrange. If `true`, the function takes
  -- | two indices and the output is a slice; otherwise, the function take one index and the
  -- | output is a reference to a single element.
  builtinindexopIsRange :: Bool
  }
  deriving (Show, Eq, Ord)

-- | Builtin types identifiers.
-- | 
-- | WARNING: for now, all the built-in types are covariant in the generic
-- | parameters (if there are). Adding types which don't satisfy this
-- | will require to update the code abstracting the signatures (to properly
-- | take into account the lifetime constraints).
-- | 
-- | TODO: update to not hardcode the types (except `Box` maybe) and be more
-- | modular.
-- | TODO: move to builtins.rs?
data BuiltinTy = TBox
  | TArray
  | TSlice
  | TStr
  deriving (Show, Eq, Ord)

-- | Additional information for closures.
data ClosureInfo = ClosureInfo
  { closureinfoKind :: ClosureKind
  ,   -- | The `FnOnce` implementation of this closure -- always exists.
  closureinfoFnOnceImpl :: (RegionBinder TraitImplRef)
  ,   -- | The `FnMut` implementation of this closure, if any.
  closureinfoFnMutImpl :: Maybe (RegionBinder TraitImplRef)
  ,   -- | The `Fn` implementation of this closure, if any.
  closureinfoFnImpl :: Maybe (RegionBinder TraitImplRef)
  ,   -- | The signature of the function that this closure represents.
  closureinfoSignature :: (RegionBinder ([Ty], Ty))
  }
  deriving (Show, Eq, Ord)

data ClosureKind = Fn
  | FnMut
  | FnOnce
  deriving (Show, Eq, Ord)

-- | Const Generic Values. Either a primitive value, or a variable corresponding to a primitve value
data ConstGeneric = CgGlobal G.GlobalDeclId
  | CgVar ((DeBruijnVar ConstGenericVarId))
  | CgValue Val.Literal
  deriving (Show, Eq, Ord)

-- | A const generic variable in a signature or binder.
data ConstGenericParam = ConstGenericParam
  {   -- | Index identifying the variable among other variables bound at the same level.
  constgenericparamIndex :: ConstGenericVarId
  ,   -- | Const generic name
  constgenericparamName :: String
  ,   -- | Type of the const generic
  constgenericparamTy :: LiteralType
  }
  deriving (Show, Eq, Ord)

data ConstGenericVarId = ConstGenericVarId
  { constgenericvaridRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | The index of a binder, counting from the innermost. See [`DeBruijnVar`] for details.
data DeBruijnId = DeBruijnId
  { debruijnidIndex :: Int
  }
  deriving (Show, Eq, Ord)

-- | Type-level variable.
-- | 
-- | Variables are bound in groups. Each item has a top-level binding group in its `generic_params`
-- | field, and then inner binders are possible using the `RegionBinder<T>` and `Binder<T>` types.
-- | Each variable is linked to exactly one binder. The `Id` then identifies the specific variable
-- | among all those bound in that group.
-- | 
-- | For instance, we have the following:
-- | ```text
-- | fn f<'a, 'b>(x: for<'c> fn(&'b u8, &'c u16, for<'d> fn(&'b u32, &'c u64, &'d u128)) -> u64) {}
-- |      ^^^^^^         ^^       ^       ^          ^^       ^        ^        ^
-- |        |       inner binder  |       |     inner binder  |        |        |
-- |  top-level binder            |       |                   |        |        |
-- |                        Bound(1, b)   |              Bound(2, b)   |     Bound(0, d)
-- |                                      |                            |
-- |                                  Bound(0, c)                 Bound(1, c)
-- | ```
-- | 
-- | To make consumption easier for projects that don't do heavy substitution, a micro-pass at the
-- | end changes the variables bound at the top-level (i.e. in the `GenericParams` of items) to be
-- | `Free`. This is an optional pass, we may add a flag to deactivate it. The example above
-- | becomes:
-- | ```text
-- | fn f<'a, 'b>(x: for<'c> fn(&'b u8, &'c u16, for<'d> fn(&'b u32, &'c u64, &'d u128)) -> u64) {}
-- |      ^^^^^^         ^^       ^       ^          ^^       ^        ^        ^
-- |        |       inner binder  |       |     inner binder  |        |        |
-- |  top-level binder            |       |                   |        |        |
-- |                           Free(b)    |                Free(b)     |     Bound(0, d)
-- |                                      |                            |
-- |                                  Bound(0, c)                 Bound(1, c)
-- | ```
-- | 
-- | At the moment only region variables can be bound in a non-top-level binder.
data DeBruijnVar a0 = Bound DeBruijnId a0
  | Free a0
  deriving (Show, Eq, Ord)

data Disambiguator = Disambiguator
  { disambiguatorRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Layout of the discriminant.
-- | Describes the offset of the discriminant field as well as its encoding
-- | as `tag` in memory.
data DiscriminantLayout = DiscriminantLayout
  {   -- | The offset of the discriminant in bytes.
  discriminantlayoutOffset :: Int
  ,   -- | The representation type of the discriminant.
  discriminantlayoutTagTy :: IntegerType
  ,   -- | How the tag is encoding in memory.
  discriminantlayoutEncoding :: TagEncoding
  }
  deriving (Show, Eq, Ord)

-- | The contents of a `dyn Trait` type.
data DynPredicate = DynPredicate
  {   -- | This binder binds a single type `T`, which is considered existentially quantified. The
  -- | predicates in the binder apply to `T` and represent the `dyn Trait` constraints.
  -- | E.g. `dyn Iterator<Item=u32> + Send` is represented as `exists<T: Iterator<Item=u32> + Send> T`.
  -- | 
  -- | Only the first trait clause may have methods. We use the vtable of this trait in the `dyn
  -- | Trait` pointer metadata.
  dynpredicateBinder :: (Binder Ty)
  }
  deriving (Show, Eq, Ord)

data Field = Field
  { fieldSpan :: M.Span
  , fieldAttrInfo :: M.AttrInfo
  , fieldFieldName :: Maybe String
  , fieldFieldTy :: Ty
  }
  deriving (Show, Eq, Ord)

data FieldId = FieldId
  { fieldidRaw :: Int
  }
  deriving (Show, Eq, Ord)

data FloatType = F16
  | F32
  | F64
  | F128
  deriving (Show, Eq, Ord)

-- | This is simlar to the Scalar value above. However, instead of storing
-- | the float value itself, we store its String representation. This allows
-- | to derive the Eq and Ord traits, which are not implemented for floats
data FloatValue = FloatValue
  { floatvalueFloatValue :: String
  , floatvalueFloatTy :: FloatType
  }
  deriving (Show, Eq, Ord)

data FnPtr = FnPtr
  { fnptrKind :: E.FnPtrKind
  , fnptrGenerics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

data FnPtrKind = FunId E.FunId
  | TraitMethod TraitRef G.TraitItemName G.FunDeclId
  deriving (Show, Eq, Ord)

data FunDeclId = FunDeclId
  { fundeclidRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Reference to a function declaration.
data FunDeclRef = FunDeclRef
  { fundeclrefId :: G.FunDeclId
  ,   -- | Generic arguments passed to the function.
  fundeclrefGenerics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

-- | A function identifier. See [crate::ullbc_ast::Terminator]
data FunId = FRegular G.FunDeclId
  | FBuiltin E.BuiltinFunId
  deriving (Show, Eq, Ord)

-- | A set of generic arguments.
data GenericArgs = GenericArgs
  { genericargsRegions :: (M.Vector RegionId Region)
  , genericargsTypes :: (M.Vector TypeVarId Ty)
  , genericargsConstGenerics :: (M.Vector ConstGenericVarId ConstGeneric)
  , genericargsTraitRefs :: (M.Vector TraitClauseId TraitRef)
  }
  deriving (Show, Eq, Ord)

-- | Generic parameters for a declaration.
-- | We group the generics which come from the Rust compiler substitutions
-- | (the regions, types and const generics) as well as the trait clauses.
-- | The reason is that we consider that those are parameters that need to
-- | be filled. We group in a different place the predicates which are not
-- | trait clauses, because those enforce constraints but do not need to
-- | be filled with witnesses/instances.
data GenericParams = GenericParams
  { genericparamsRegions :: (M.Vector RegionId RegionParam)
  , genericparamsTypes :: (M.Vector TypeVarId TypeParam)
  , genericparamsConstGenerics :: (M.Vector ConstGenericVarId ConstGenericParam)
  , genericparamsTraitClauses :: (M.Vector TraitClauseId TraitParam)
  ,   -- | The first region in the pair outlives the second region
  genericparamsRegionsOutlive :: [(RegionBinder (OutlivesPred Region Region))]
  ,   -- | The type outlives the region
  genericparamsTypesOutlive :: [(RegionBinder (OutlivesPred Ty Region))]
  ,   -- | Constraints over trait associated types
  genericparamsTraitTypeConstraints :: (M.Vector TraitTypeConstraintId (RegionBinder TraitTypeConstraint))
  }
  deriving (Show, Eq, Ord)

data GlobalDeclId = GlobalDeclId
  { globaldeclidRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Reference to a global declaration.
data GlobalDeclRef = GlobalDeclRef
  { globaldeclrefId :: G.GlobalDeclId
  , globaldeclrefGenerics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

-- | There are two kinds of `impl` blocks:
-- | - impl blocks linked to a type ("inherent" impl blocks following Rust terminology):
-- |   ```text
-- |   impl<T> List<T> { ...}
-- |   ```
-- | - trait impl blocks:
-- |   ```text
-- |   impl<T> PartialEq for List<T> { ...}
-- |   ```
-- | We distinguish the two.
data ImplElem = ImplElemTy ((Binder Ty))
  | ImplElemTrait G.TraitImplId
  deriving (Show, Eq, Ord)

data IntTy = Isize
  | I8
  | I16
  | I32
  | I64
  | I128
  deriving (Show, Eq, Ord)

data IntegerType = Signed IntTy
  | Unsigned UIntTy
  deriving (Show, Eq, Ord)

-- | The id of a translated item.
data ItemId = IdType G.TypeDeclId
  | IdFun G.FunDeclId
  | IdGlobal G.GlobalDeclId
  | IdTraitDecl G.TraitDeclId
  | IdTraitImpl G.TraitImplId
  deriving (Show, Eq, Ord)

-- | Meta information about an item (function, trait decl, trait impl, type decl, global).
data ItemMeta = ItemMeta
  { itemmetaName :: Name
  , itemmetaSpan :: M.Span
  ,   -- | The source code that corresponds to this item.
  itemmetaSourceText :: Maybe String
  ,   -- | Attributes and visibility.
  itemmetaAttrInfo :: M.AttrInfo
  ,   -- | `true` if the type decl is a local type decl, `false` if it comes from an external crate.
  itemmetaIsLocal :: Bool
  ,   -- | If the item is built-in, record its internal builtin identifier.
  itemmetaLangItem :: Maybe String
  }
  deriving (Show, Eq, Ord)

-- | Item kind: whether this function/const is part of a trait declaration, trait implementation, or
-- | neither.
-- | 
-- | Example:
-- | ```text
-- | trait Foo {
-- |     fn bar(x : u32) -> u32; // trait item decl without default
-- | 
-- |     fn baz(x : bool) -> bool { x } // trait item decl with default
-- | }
-- | 
-- | impl Foo for ... {
-- |     fn bar(x : u32) -> u32 { x } // trait item implementation
-- | }
-- | 
-- | fn test(...) { ... } // regular
-- | 
-- | impl Type {
-- |     fn test(...) { ... } // regular
-- | }
-- | ```
data ItemSource = TopLevelItem
  | ClosureItem ClosureInfo
  | TraitDeclItem TraitDeclRef G.TraitItemName Bool
  | TraitImplItem TraitImplRef TraitDeclRef G.TraitItemName Bool
  | VTableTyItem DynPredicate
  | VTableInstanceItem TraitImplRef
  | VTableMethodShimItem
  deriving (Show, Eq, Ord)

-- | Simplified type layout information.
-- | 
-- | Does not include information about niches.
-- | If the type does not have a fully known layout (e.g. it is ?Sized)
-- | some of the layout parts are not available.
data Layout = Layout
  {   -- | The size of the type in bytes.
  layoutSize :: Maybe Int
  ,   -- | The alignment, in bytes.
  layoutAlign :: Maybe Int
  ,   -- | The discriminant's layout, if any. Only relevant for types with multiple variants.
  layoutDiscriminantLayout :: Maybe DiscriminantLayout
  ,   -- | Whether the type is uninhabited, i.e. has any valid value at all.
  -- | Note that uninhabited types can have arbitrary layouts: `(u32, !)` has space for the `u32`
  -- | and `enum E2 { A, B(!), C(i32, !) }` may have space for a discriminant.
  layoutUninhabited :: Bool
  ,   -- | Map from `VariantId` to the corresponding field layouts. Structs are modeled as having
  -- | exactly one variant, unions as having no variant.
  layoutVariantLayouts :: (M.Vector VariantId VariantLayout)
  }
  deriving (Show, Eq, Ord)

-- | A primitive value.
-- | 
-- | Those are for instance used for the constant operands [crate::expressions::Operand::Const]
data Literal = VScalar Val.ScalarValue
  | VFloat Val.FloatValue
  | VBool Bool
  | VChar Char
  | VByteStr [Int]
  | VStr String
  deriving (Show, Eq, Ord)

-- | Types of primitive values. Either an integer, bool, char
data LiteralType = TInt IntTy
  | TuInt UIntTy
  | TFloat FloatType
  | TBool
  | TChar
  deriving (Show, Eq, Ord)

-- | An item name/path
-- | 
-- | A name really is a list of strings. However, we sometimes need to
-- | introduce unique indices to disambiguate. This mostly happens because
-- | of "impl" blocks:
-- |   ```text
-- |   impl<T> List<T> {
-- |     ...
-- |   }
-- |   ```
-- | 
-- | A type in Rust can have several "impl" blocks, and  those blocks can
-- | contain items with similar names. For this reason, we need to disambiguate
-- | them with unique indices. Rustc calls those "disambiguators". In rustc, this
-- | gives names like this:
-- | - `betree_main::betree::NodeIdCounter{impl#0}::new`
-- | - note that impl blocks can be nested, and macros sometimes generate
-- |   weird names (which require disambiguation):
-- |   `betree_main::betree_utils::_#1::{impl#0}::deserialize::{impl#0}`
-- | 
-- | Finally, the paths used by rustc are a lot more precise and explicit than
-- | those we expose in LLBC: for instance, every identifier belongs to a specific
-- | namespace (value namespace, type namespace, etc.), and is coupled with a
-- | disambiguator.
-- | 
-- | On our side, we want to stay high-level and simple: we use string identifiers
-- | as much as possible, insert disambiguators only when necessary (whenever
-- | we find an "impl" block, typically) and check that the disambiguator is useless
-- | in the other situations (i.e., the disambiguator is always equal to 0).
-- | 
-- | Moreover, the items are uniquely disambiguated by their (integer) ids
-- | (`TypeDeclId`, etc.), and when extracting the code we have to deal with
-- | name clashes anyway. Still, we might want to be more precise in the future.
-- | 
-- | Also note that the first path element in the name is always the crate name.
data Name = Name
  { nameName :: [PathElem]
  }
  deriving (Show, Eq, Ord)

-- | .0 outlives .1
data OutlivesPred a0 a1 = OutlivesPred a0 a1
  deriving (Show, Eq, Ord)

-- | See the comments for [Name]
data PathElem = PeIdent String Disambiguator
  | PeImpl ImplElem
  | PeInstantiated ((Binder GenericArgs))
  deriving (Show, Eq, Ord)

-- | The metadata stored in a pointer. That's the information stored in pointers alongside
-- | their address. It's empty for `Sized` types, and interesting for unsized
-- | aka dynamically-sized types.
data PtrMetadata = NoMetadata
  | Length
  | VTable TypeDeclRef
  | InheritFrom Ty
  deriving (Show, Eq, Ord)

data RefKind = RMut
  | RShared
  deriving (Show, Eq, Ord)

data Region = RVar ((DeBruijnVar RegionId))
  | RStatic
  | RErased
  deriving (Show, Eq, Ord)

-- | A value of type `T` bound by regions. We should use `binder` instead but this causes name clash
-- | issues in the derived ocaml visitors.
-- | TODO: merge with `binder`
data RegionBinder a0 = RegionBinder
  { regionbinderBinderRegions :: (M.Vector RegionId RegionParam)
  ,   -- | Named this way to highlight accesses to the inner value that might be handling parameters
  -- | incorrectly. Prefer using helper methods.
  regionbinderBinderValue :: a0
  }
  deriving (Show, Eq, Ord)

data RegionId = RegionId
  { regionidRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | A region variable in a signature or binder.
data RegionParam = RegionParam
  {   -- | Index identifying the variable among other variables bound at the same level.
  regionparamIndex :: RegionId
  ,   -- | Region name
  regionparamName :: Maybe String
  }
  deriving (Show, Eq, Ord)

-- | Describes which layout algorithm is used for representing the corresponding type.
-- | Depends on the `#[repr(...)]` used.
data ReprAlgorithm = Rust
  | C
  deriving (Show, Eq, Ord)

-- | The representation options as annotated by the user.
-- | 
-- | NOTE: This does not include less common/unstable representations such as `#[repr(simd)]`
-- | or the compiler internal `#[repr(linear)]`. Similarly, enum discriminant representations
-- | are encoded in [`Variant::discriminant`] and [`DiscriminantLayout`] instead.
-- | This only stores whether the discriminant type was derived from an explicit annotation.
data ReprOptions = ReprOptions
  { reproptionsReprAlgo :: ReprAlgorithm
  , reproptionsAlignModif :: Maybe AlignmentModifier
  , reproptionsTransparent :: Bool
  , reproptionsExplicitDiscrType :: Bool
  }
  deriving (Show, Eq, Ord)

-- | A scalar value.
data ScalarValue = UnsignedScalar UIntTy Integer
  | SignedScalar IntTy Integer
  deriving (Show, Eq, Ord)

-- | Describes how we represent the active enum variant in memory.
data TagEncoding = Direct
  | Niche VariantId
  deriving (Show, Eq, Ord)

-- | The value of a trait associated type.
data TraitAssocTyImpl = TraitAssocTyImpl
  { traitassoctyimplValue :: Ty
  }
  deriving (Show, Eq, Ord)

data TraitClauseId = TraitClauseId
  { traitclauseidRaw :: Int
  }
  deriving (Show, Eq, Ord)

data TraitDeclId = TraitDeclId
  { traitdeclidRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | A predicate of the form `Type: Trait<Args>`.
-- | 
-- | About the generics, if we write:
-- | ```text
-- | impl Foo<bool> for String { ... }
-- | ```
-- | 
-- | The substitution is: `[String, bool]`.
data TraitDeclRef = TraitDeclRef
  { traitdeclrefId :: G.TraitDeclId
  , traitdeclrefGenerics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

data TraitImplId = TraitImplId
  { traitimplidRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | A reference to a tait impl, using the provided arguments.
data TraitImplRef = TraitImplRef
  { traitimplrefId :: G.TraitImplId
  , traitimplrefGenerics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

data TraitItemName = TraitItemName Text
  deriving (Show, Eq, Ord)

-- | A trait predicate in a signature, of the form `Type: Trait<Args>`. This functions like a
-- | variable binder, to which variables of the form `TraitRefKind::Clause` can refer to.
data TraitParam = TraitParam
  {   -- | Index identifying the clause among other clauses bound at the same level.
  traitparamClauseId :: TraitClauseId
  , traitparamSpan :: Maybe M.Span
  ,   -- | The trait that is implemented.
  traitparamTrait :: (RegionBinder TraitDeclRef)
  }
  deriving (Show, Eq, Ord)

-- | A reference to a trait
data TraitRef = TraitRef
  { traitrefKind :: TraitRefKind
  ,   -- | Not necessary, but useful
  traitrefTraitDeclRef :: (RegionBinder TraitDeclRef)
  }
  deriving (Show, Eq, Ord)

-- | Identifier of a trait instance.
-- | This is derived from the trait resolution.
-- | 
-- | Should be read as a path inside the trait clauses which apply to the current
-- | definition. Note that every path designated by `TraitInstanceId` refers
-- | to a *trait instance*, which is why the [`TraitRefKind::Clause`] variant may seem redundant
-- | with some of the other variants.
data TraitRefKind = TraitImpl TraitImplRef
  | Clause ((DeBruijnVar TraitClauseId))
  | ParentClause TraitRef TraitClauseId
  | ItemClause TraitRef G.TraitItemName TraitClauseId
  | Self
  | BuiltinOrAuto BuiltinImplData ((M.Vector TraitClauseId TraitRef)) ([(G.TraitItemName, G.TraitAssocTyImpl)])
  | Dyn
  | UnknownTrait String
  deriving (Show, Eq, Ord)

-- | A constraint over a trait associated type.
-- | 
-- | Example:
-- | ```text
-- | T : Foo<S = String>
-- |         ^^^^^^^^^^
-- | ```
data TraitTypeConstraint = TraitTypeConstraint
  { traittypeconstraintTraitRef :: TraitRef
  , traittypeconstraintTypeName :: G.TraitItemName
  , traittypeconstraintTy :: Ty
  }
  deriving (Show, Eq, Ord)

data Ty = TAdt TypeDeclRef
  | TVar ((DeBruijnVar TypeVarId))
  | TLiteral LiteralType
  | TNever
  | TRef Region Ty RefKind
  | TRawPtr Ty RefKind
  | TTraitType TraitRef G.TraitItemName
  | TDynTrait DynPredicate
  | TFnPtr ((RegionBinder ([Ty], Ty)))
  | TFnDef ((RegionBinder E.FnPtr))
  | TPtrMetadata Ty
  | TError String
  deriving (Show, Eq, Ord)

-- | A type declaration.
-- | 
-- | Types can be opaque or transparent.
-- | 
-- | Transparent types are local types not marked as opaque.
-- | Opaque types are the others: local types marked as opaque, and non-local
-- | types (coming from external dependencies).
-- | 
-- | In case the type is transparent, the declaration also contains the
-- | type definition (see [TypeDeclKind]).
-- | 
-- | A type can only be an ADT (structure or enumeration), as type aliases are
-- | inlined in MIR.
data TypeDecl = TypeDecl
  { typedeclDefId :: G.TypeDeclId
  ,   -- | Meta information associated with the item.
  typedeclItemMeta :: M.ItemMeta
  , typedeclGenerics :: GenericParams
  ,   -- | The context of the type: distinguishes top-level items from closure-related items.
  typedeclSrc :: G.ItemSource
  ,   -- | The type kind: enum, struct, or opaque.
  typedeclKind :: TypeDeclKind
  ,   -- | The layout of the type. Information may be partial because of generics or dynamically-
  -- | sized types. If rustc cannot compute a layout, it is `None`.
  typedeclLayout :: Maybe Layout
  ,   -- | The metadata associated with a pointer to the type.
  typedeclPtrMetadata :: PtrMetadata
  ,   -- | The representation options of this type declaration as annotated by the user.
  -- | Is `None` for foreign type declarations.
  typedeclRepr :: Maybe ReprOptions
  }
  deriving (Show, Eq, Ord)

data TypeDeclId = TypeDeclId
  { typedeclidRaw :: Int
  }
  deriving (Show, Eq, Ord)

data TypeDeclKind = Struct ((M.Vector FieldId Field))
  | Enum ((M.Vector VariantId Variant))
  | Union ((M.Vector FieldId Field))
  | Opaque
  | Alias Ty
  | TDeclError String
  deriving (Show, Eq, Ord)

-- | Reference to a type declaration or builtin type.
data TypeDeclRef = TypeDeclRef
  { typedeclrefId :: TypeId
  , typedeclrefGenerics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

-- | Type identifier.
-- | 
-- | Allows us to factorize the code for built-in types, adts and tuples
data TypeId = TAdtId G.TypeDeclId
  | TTuple
  | TBuiltin BuiltinTy
  deriving (Show, Eq, Ord)

-- | A type variable in a signature or binder.
data TypeParam = TypeParam
  {   -- | Index identifying the variable among other variables bound at the same level.
  typeparamIndex :: TypeVarId
  ,   -- | Variable name
  typeparamName :: String
  }
  deriving (Show, Eq, Ord)

data TypeVarId = TypeVarId
  { typevaridRaw :: Int
  }
  deriving (Show, Eq, Ord)

data UIntTy = Usize
  | U8
  | U16
  | U32
  | U64
  | U128
  deriving (Show, Eq, Ord)

data Variant = Variant
  { variantSpan :: M.Span
  , variantAttrInfo :: M.AttrInfo
  , variantVariantName :: String
  , variantFields :: (M.Vector FieldId Field)
  ,   -- | The discriminant value outputted by `std::mem::discriminant` for this variant.
  -- | This can be different than the discriminant stored in memory (called `tag`).
  -- | That one is described by [`DiscriminantLayout`] and [`TagEncoding`].
  variantDiscriminant :: Val.Literal
  }
  deriving (Show, Eq, Ord)

data VariantId = VariantId
  { variantidRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Simplified layout of a single variant.
-- | 
-- | Maps fields to their offset within the layout.
data VariantLayout = VariantLayout
  {   -- | The offset of each field.
  variantlayoutFieldOffsets :: (M.Vector FieldId Int)
  ,   -- | Whether the variant is uninhabited, i.e. has any valid possible value.
  -- | Note that uninhabited types can have arbitrary layouts.
  variantlayoutUninhabited :: Bool
  ,   -- | The memory representation of the discriminant corresponding to this
  -- | variant. It must be of the same type as the corresponding [`DiscriminantLayout::tag_ty`].
  -- | 
  -- | If it's `None`, then this variant is either:
  -- | - the untagged variant (cf. [`TagEncoding::Niche::untagged_variant`]) of a niched enum;
  -- | - the single variant of a struct;
  -- | - uninhabited.
  variantlayoutTag :: Maybe Val.ScalarValue
  }
  deriving (Show, Eq, Ord)

instance FromJSON AbortKind where
  parseJSON v = case v of
    Object o | H.lookup "Panic" o /= Nothing -> do
      v <- o .: "Panic"
      Panic <$> parseJSON v
    String "UndefinedBehavior" -> pure UndefinedBehavior
    String "UnwindTerminate" -> pure UnwindTerminate
    _ -> fail "Unknown variant"


instance FromJSON AlignmentModifier where
  parseJSON v = case v of
    Object o | H.lookup "Align" o /= Nothing -> do
      v <- o .: "Align"
      Align <$> parseJSON v
    Object o | H.lookup "Pack" o /= Nothing -> do
      v <- o .: "Pack"
      Pack <$> parseJSON v
    _ -> fail "Unknown variant"


instance (FromJSON a0) => FromJSON (Binder a0) where
  parseJSON = withObject "Binder" $ \o -> do
    binderBinderParams <- o .: "params"
    binderBinderValue <- o .: "skip_binder"
    pure (Binder binderBinderParams binderBinderValue)


instance FromJSON BinderKind where
  parseJSON v = case v of
    Object o | H.lookup "TraitType" o /= Nothing -> do
      withArray "BkTraitType" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (BkTraitType v0 v1)) =<< o .: "TraitType"
    Object o | H.lookup "TraitMethod" o /= Nothing -> do
      withArray "BkTraitMethod" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (BkTraitMethod v0 v1)) =<< o .: "TraitMethod"
    String "InherentImplBlock" -> pure BkInherentImplBlock
    String "Dyn" -> pure BkDyn
    String "Other" -> pure BkOther
    _ -> fail "Unknown variant"


instance FromJSON BuiltinFunId where
  parseJSON v = case v of
    String "BoxNew" -> pure BoxNew
    String "ArrayToSliceShared" -> pure ArrayToSliceShared
    String "ArrayToSliceMut" -> pure ArrayToSliceMut
    String "ArrayRepeat" -> pure ArrayRepeat
    Object o | H.lookup "Index" o /= Nothing -> do
      v <- o .: "Index"
      Index <$> parseJSON v
    Object o | H.lookup "PtrFromParts" o /= Nothing -> do
      v <- o .: "PtrFromParts"
      PtrFromParts <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON BuiltinImplData where
  parseJSON v = case v of
    String "Sized" -> pure BuiltinSized
    String "MetaSized" -> pure BuiltinMetaSized
    String "Tuple" -> pure BuiltinTuple
    String "Send" -> pure BuiltinSend
    String "Sync" -> pure BuiltinSync
    String "Pointee" -> pure BuiltinPointee
    String "DiscriminantKind" -> pure BuiltinDiscriminantKind
    String "Unpin" -> pure BuiltinUnpin
    String "Freeze" -> pure BuiltinFreeze
    String "NoopDestruct" -> pure BuiltinNoopDestruct
    String "UntrackedDestruct" -> pure BuiltinUntrackedDestruct
    String "Fn" -> pure BuiltinFn
    String "FnMut" -> pure BuiltinFnMut
    String "FnOnce" -> pure BuiltinFnOnce
    String "Copy" -> pure BuiltinCopy
    String "Clone" -> pure BuiltinClone
    _ -> fail "Unknown variant"


instance FromJSON BuiltinIndexOp where
  parseJSON = withObject "BuiltinIndexOp" $ \o -> do
    builtinindexopIsArray <- o .: "is_array"
    builtinindexopMutability <- o .: "mutability"
    builtinindexopIsRange <- o .: "is_range"
    pure (BuiltinIndexOp builtinindexopIsArray builtinindexopMutability builtinindexopIsRange)


instance FromJSON BuiltinTy where
  parseJSON v = case v of
    String "Box" -> pure TBox
    String "Array" -> pure TArray
    String "Slice" -> pure TSlice
    String "Str" -> pure TStr
    _ -> fail "Unknown variant"


instance FromJSON ClosureInfo where
  parseJSON = withObject "ClosureInfo" $ \o -> do
    closureinfoKind <- o .: "kind"
    closureinfoFnOnceImpl <- o .: "fn_once_impl"
    closureinfoFnMutImpl <- o .: "fn_mut_impl"
    closureinfoFnImpl <- o .: "fn_impl"
    closureinfoSignature <- o .: "signature"
    pure (ClosureInfo closureinfoKind closureinfoFnOnceImpl closureinfoFnMutImpl closureinfoFnImpl closureinfoSignature)


instance FromJSON ClosureKind where
  parseJSON v = case v of
    String "Fn" -> pure Fn
    String "FnMut" -> pure FnMut
    String "FnOnce" -> pure FnOnce
    _ -> fail "Unknown variant"


instance FromJSON ConstGeneric where
  parseJSON v = case v of
    Object o | H.lookup "Global" o /= Nothing -> do
      v <- o .: "Global"
      CgGlobal <$> parseJSON v
    Object o | H.lookup "Var" o /= Nothing -> do
      v <- o .: "Var"
      CgVar <$> parseJSON v
    Object o | H.lookup "Value" o /= Nothing -> do
      v <- o .: "Value"
      CgValue <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON ConstGenericParam where
  parseJSON = withObject "ConstGenericParam" $ \o -> do
    constgenericparamIndex <- o .: "index"
    constgenericparamName <- o .: "name"
    constgenericparamTy <- o .: "ty"
    pure (ConstGenericParam constgenericparamIndex constgenericparamName constgenericparamTy)


instance FromJSON ConstGenericVarId where
  parseJSON = fmap ConstGenericVarId . parseJSON


instance FromJSON DeBruijnId where
  parseJSON = fmap DeBruijnId . parseJSON


instance (FromJSON a0) => FromJSON (DeBruijnVar a0) where
  parseJSON v = case v of
    Object o | H.lookup "Bound" o /= Nothing -> do
      withArray "Bound" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (Bound v0 v1)) =<< o .: "Bound"
    Object o | H.lookup "Free" o /= Nothing -> do
      v <- o .: "Free"
      Free <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON Disambiguator where
  parseJSON = fmap Disambiguator . parseJSON


instance FromJSON DiscriminantLayout where
  parseJSON = withObject "DiscriminantLayout" $ \o -> do
    discriminantlayoutOffset <- o .: "offset"
    discriminantlayoutTagTy <- o .: "tag_ty"
    discriminantlayoutEncoding <- o .: "encoding"
    pure (DiscriminantLayout discriminantlayoutOffset discriminantlayoutTagTy discriminantlayoutEncoding)


instance FromJSON DynPredicate where
  parseJSON = withObject "DynPredicate" $ \o -> do
    dynpredicateBinder <- o .: "binder"
    pure (DynPredicate dynpredicateBinder)


instance FromJSON Field where
  parseJSON = withObject "Field" $ \o -> do
    fieldSpan <- o .: "span"
    fieldAttrInfo <- o .: "attr_info"
    fieldFieldName <- o .: "name"
    fieldFieldTy <- o .: "ty"
    pure (Field fieldSpan fieldAttrInfo fieldFieldName fieldFieldTy)


instance FromJSON FieldId where
  parseJSON = fmap FieldId . parseJSON


instance FromJSON FloatType where
  parseJSON v = case v of
    String "F16" -> pure F16
    String "F32" -> pure F32
    String "F64" -> pure F64
    String "F128" -> pure F128
    _ -> fail "Unknown variant"


instance FromJSON FloatValue where
  parseJSON = withObject "FloatValue" $ \o -> do
    floatvalueFloatValue <- o .: "value"
    floatvalueFloatTy <- o .: "ty"
    pure (FloatValue floatvalueFloatValue floatvalueFloatTy)


instance FromJSON FnPtr where
  parseJSON = withObject "FnPtr" $ \o -> do
    fnptrKind <- o .: "kind"
    fnptrGenerics <- o .: "generics"
    pure (FnPtr fnptrKind fnptrGenerics)


instance FromJSON FnPtrKind where
  parseJSON v = case v of
    Object o | H.lookup "Fun" o /= Nothing -> do
      v <- o .: "Fun"
      FunId <$> parseJSON v
    Object o | H.lookup "Trait" o /= Nothing -> do
      withArray "TraitMethod" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (TraitMethod v0 v1 v2)) =<< o .: "Trait"
    _ -> fail "Unknown variant"


instance FromJSON FunDeclId where
  parseJSON = fmap FunDeclId . parseJSON


instance FromJSON FunDeclRef where
  parseJSON = withObject "FunDeclRef" $ \o -> do
    fundeclrefId <- o .: "id"
    fundeclrefGenerics <- o .: "generics"
    pure (FunDeclRef fundeclrefId fundeclrefGenerics)


instance FromJSON FunId where
  parseJSON v = case v of
    Object o | H.lookup "Regular" o /= Nothing -> do
      v <- o .: "Regular"
      FRegular <$> parseJSON v
    Object o | H.lookup "Builtin" o /= Nothing -> do
      v <- o .: "Builtin"
      FBuiltin <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON GenericArgs where
  parseJSON = withObject "GenericArgs" $ \o -> do
    genericargsRegions <- o .: "regions"
    genericargsTypes <- o .: "types"
    genericargsConstGenerics <- o .: "const_generics"
    genericargsTraitRefs <- o .: "trait_refs"
    pure (GenericArgs genericargsRegions genericargsTypes genericargsConstGenerics genericargsTraitRefs)


instance FromJSON GenericParams where
  parseJSON = withObject "GenericParams" $ \o -> do
    genericparamsRegions <- o .: "regions"
    genericparamsTypes <- o .: "types"
    genericparamsConstGenerics <- o .: "const_generics"
    genericparamsTraitClauses <- o .: "trait_clauses"
    genericparamsRegionsOutlive <- o .: "regions_outlive"
    genericparamsTypesOutlive <- o .: "types_outlive"
    genericparamsTraitTypeConstraints <- o .: "trait_type_constraints"
    pure (GenericParams genericparamsRegions genericparamsTypes genericparamsConstGenerics genericparamsTraitClauses genericparamsRegionsOutlive genericparamsTypesOutlive genericparamsTraitTypeConstraints)


instance FromJSON GlobalDeclId where
  parseJSON = fmap GlobalDeclId . parseJSON


instance FromJSON GlobalDeclRef where
  parseJSON = withObject "GlobalDeclRef" $ \o -> do
    globaldeclrefId <- o .: "id"
    globaldeclrefGenerics <- o .: "generics"
    pure (GlobalDeclRef globaldeclrefId globaldeclrefGenerics)


instance FromJSON ImplElem where
  parseJSON v = case v of
    Object o | H.lookup "Ty" o /= Nothing -> do
      v <- o .: "Ty"
      ImplElemTy <$> parseJSON v
    Object o | H.lookup "Trait" o /= Nothing -> do
      v <- o .: "Trait"
      ImplElemTrait <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON IntTy where
  parseJSON v = case v of
    String "Isize" -> pure Isize
    String "I8" -> pure I8
    String "I16" -> pure I16
    String "I32" -> pure I32
    String "I64" -> pure I64
    String "I128" -> pure I128
    _ -> fail "Unknown variant"


instance FromJSON IntegerType where
  parseJSON v = case v of
    Object o | H.lookup "Signed" o /= Nothing -> do
      v <- o .: "Signed"
      Signed <$> parseJSON v
    Object o | H.lookup "Unsigned" o /= Nothing -> do
      v <- o .: "Unsigned"
      Unsigned <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON ItemId where
  parseJSON v = case v of
    Object o | H.lookup "Type" o /= Nothing -> do
      v <- o .: "Type"
      IdType <$> parseJSON v
    Object o | H.lookup "Fun" o /= Nothing -> do
      v <- o .: "Fun"
      IdFun <$> parseJSON v
    Object o | H.lookup "Global" o /= Nothing -> do
      v <- o .: "Global"
      IdGlobal <$> parseJSON v
    Object o | H.lookup "TraitDecl" o /= Nothing -> do
      v <- o .: "TraitDecl"
      IdTraitDecl <$> parseJSON v
    Object o | H.lookup "TraitImpl" o /= Nothing -> do
      v <- o .: "TraitImpl"
      IdTraitImpl <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON ItemMeta where
  parseJSON = withObject "ItemMeta" $ \o -> do
    itemmetaName <- o .: "name"
    itemmetaSpan <- o .: "span"
    itemmetaSourceText <- o .: "source_text"
    itemmetaAttrInfo <- o .: "attr_info"
    itemmetaIsLocal <- o .: "is_local"
    itemmetaLangItem <- o .: "lang_item"
    pure (ItemMeta itemmetaName itemmetaSpan itemmetaSourceText itemmetaAttrInfo itemmetaIsLocal itemmetaLangItem)


instance FromJSON ItemSource where
  parseJSON v = case v of
    String "TopLevel" -> pure TopLevelItem
    Object o | H.lookup "Closure" o /= Nothing -> do
      obj <- o .: "Closure"
      info <- obj .: "info"
      pure (ClosureItem info)
    Object o | H.lookup "TraitDecl" o /= Nothing -> do
      obj <- o .: "TraitDecl"
      traitRef <- obj .: "trait_ref"
      itemName <- obj .: "item_name"
      hasDefault <- obj .: "has_default"
      pure (TraitDeclItem traitRef itemName hasDefault)
    Object o | H.lookup "TraitImpl" o /= Nothing -> do
      obj <- o .: "TraitImpl"
      implRef <- obj .: "impl_ref"
      traitRef <- obj .: "trait_ref"
      itemName <- obj .: "item_name"
      reusesDefault <- obj .: "reuses_default"
      pure (TraitImplItem implRef traitRef itemName reusesDefault)
    Object o | H.lookup "VTableTy" o /= Nothing -> do
      obj <- o .: "VTableTy"
      dynPredicate <- obj .: "dyn_predicate"
      pure (VTableTyItem dynPredicate)
    Object o | H.lookup "VTableInstance" o /= Nothing -> do
      obj <- o .: "VTableInstance"
      implRef <- obj .: "impl_ref"
      pure (VTableInstanceItem implRef)
    String "VTableMethodShim" -> pure VTableMethodShimItem
    _ -> fail "Unknown variant"


instance FromJSON Layout where
  parseJSON = withObject "Layout" $ \o -> do
    layoutSize <- o .: "size"
    layoutAlign <- o .: "align"
    layoutDiscriminantLayout <- o .: "discriminant_layout"
    layoutUninhabited <- o .: "uninhabited"
    layoutVariantLayouts <- o .: "variant_layouts"
    pure (Layout layoutSize layoutAlign layoutDiscriminantLayout layoutUninhabited layoutVariantLayouts)


instance FromJSON Literal where
  parseJSON v = case v of
    Object o | H.lookup "Scalar" o /= Nothing -> do
      v <- o .: "Scalar"
      VScalar <$> parseJSON v
    Object o | H.lookup "Float" o /= Nothing -> do
      v <- o .: "Float"
      VFloat <$> parseJSON v
    Object o | H.lookup "Bool" o /= Nothing -> do
      v <- o .: "Bool"
      VBool <$> parseJSON v
    Object o | H.lookup "Char" o /= Nothing -> do
      v <- o .: "Char"
      VChar <$> parseJSON v
    Object o | H.lookup "ByteStr" o /= Nothing -> do
      v <- o .: "ByteStr"
      VByteStr <$> parseJSON v
    Object o | H.lookup "Str" o /= Nothing -> do
      v <- o .: "Str"
      VStr <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON LiteralType where
  parseJSON v = case v of
    Object o | H.lookup "Int" o /= Nothing -> do
      v <- o .: "Int"
      TInt <$> parseJSON v
    Object o | H.lookup "UInt" o /= Nothing -> do
      v <- o .: "UInt"
      TuInt <$> parseJSON v
    Object o | H.lookup "Float" o /= Nothing -> do
      v <- o .: "Float"
      TFloat <$> parseJSON v
    String "Bool" -> pure TBool
    String "Char" -> pure TChar
    _ -> fail "Unknown variant"


instance FromJSON Name where
  parseJSON = fmap Name . parseJSON


instance (FromJSON a0, FromJSON a1) => FromJSON (OutlivesPred a0 a1) where
  parseJSON = withArray "OutlivesPred" $ \v -> do
    v0 <- parseJSON (v V.! 0)
    v1 <- parseJSON (v V.! 1)
    pure (OutlivesPred v0 v1)


instance FromJSON PathElem where
  parseJSON v = case v of
    Object o | H.lookup "Ident" o /= Nothing -> do
      withArray "PeIdent" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (PeIdent v0 v1)) =<< o .: "Ident"
    Object o | H.lookup "Impl" o /= Nothing -> do
      v <- o .: "Impl"
      PeImpl <$> parseJSON v
    Object o | H.lookup "Instantiated" o /= Nothing -> do
      v <- o .: "Instantiated"
      PeInstantiated <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON PtrMetadata where
  parseJSON v = case v of
    String "None" -> pure NoMetadata
    String "Length" -> pure Length
    Object o | H.lookup "VTable" o /= Nothing -> do
      v <- o .: "VTable"
      VTable <$> parseJSON v
    Object o | H.lookup "InheritFrom" o /= Nothing -> do
      v <- o .: "InheritFrom"
      InheritFrom <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON RefKind where
  parseJSON v = case v of
    String "Mut" -> pure RMut
    String "Shared" -> pure RShared
    _ -> fail "Unknown variant"


instance FromJSON Region where
  parseJSON v = case v of
    Object o | H.lookup "Var" o /= Nothing -> do
      v <- o .: "Var"
      RVar <$> parseJSON v
    String "Static" -> pure RStatic
    String "Erased" -> pure RErased
    _ -> fail "Unknown variant"


instance (FromJSON a0) => FromJSON (RegionBinder a0) where
  parseJSON = withObject "RegionBinder" $ \o -> do
    regionbinderBinderRegions <- o .: "regions"
    regionbinderBinderValue <- o .: "skip_binder"
    pure (RegionBinder regionbinderBinderRegions regionbinderBinderValue)


instance FromJSON RegionId where
  parseJSON = fmap RegionId . parseJSON


instance FromJSON RegionParam where
  parseJSON = withObject "RegionParam" $ \o -> do
    regionparamIndex <- o .: "index"
    regionparamName <- o .: "name"
    pure (RegionParam regionparamIndex regionparamName)


instance FromJSON ReprAlgorithm where
  parseJSON v = case v of
    String "Rust" -> pure Rust
    String "C" -> pure C
    _ -> fail "Unknown variant"


instance FromJSON ReprOptions where
  parseJSON = withObject "ReprOptions" $ \o -> do
    reproptionsReprAlgo <- o .: "repr_algo"
    reproptionsAlignModif <- o .: "align_modif"
    reproptionsTransparent <- o .: "transparent"
    reproptionsExplicitDiscrType <- o .: "explicit_discr_type"
    pure (ReprOptions reproptionsReprAlgo reproptionsAlignModif reproptionsTransparent reproptionsExplicitDiscrType)


instance FromJSON ScalarValue where
  parseJSON v = case v of
    Object o | H.lookup "Unsigned" o /= Nothing -> do
      withArray "UnsignedScalar" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseIntegerValue (v V.! 1)
        pure (UnsignedScalar v0 v1)) =<< o .: "Unsigned"
    Object o | H.lookup "Signed" o /= Nothing -> do
      withArray "SignedScalar" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseIntegerValue (v V.! 1)
        pure (SignedScalar v0 v1)) =<< o .: "Signed"
    _ -> fail "Unknown variant"


instance FromJSON TagEncoding where
  parseJSON v = case v of
    String "Direct" -> pure Direct
    Object o | H.lookup "Niche" o /= Nothing -> do
      obj <- o .: "Niche"
      untaggedVariant <- obj .: "untagged_variant"
      pure (Niche untaggedVariant)
    _ -> fail "Unknown variant"


instance FromJSON TraitAssocTyImpl where
  parseJSON = withObject "TraitAssocTyImpl" $ \o -> do
    traitassoctyimplValue <- o .: "value"
    pure (TraitAssocTyImpl traitassoctyimplValue)


instance FromJSON TraitClauseId where
  parseJSON = fmap TraitClauseId . parseJSON


instance FromJSON TraitDeclId where
  parseJSON = fmap TraitDeclId . parseJSON


instance FromJSON TraitDeclRef where
  parseJSON = withObject "TraitDeclRef" $ \o -> do
    traitdeclrefId <- o .: "id"
    traitdeclrefGenerics <- o .: "generics"
    pure (TraitDeclRef traitdeclrefId traitdeclrefGenerics)


instance FromJSON TraitImplId where
  parseJSON = fmap TraitImplId . parseJSON


instance FromJSON TraitImplRef where
  parseJSON = withObject "TraitImplRef" $ \o -> do
    traitimplrefId <- o .: "id"
    traitimplrefGenerics <- o .: "generics"
    pure (TraitImplRef traitimplrefId traitimplrefGenerics)


instance FromJSON TraitItemName where
  parseJSON = fmap TraitItemName . parseJSON


instance FromJSON TraitParam where
  parseJSON = withObject "TraitParam" $ \o -> do
    traitparamClauseId <- o .: "clause_id"
    traitparamSpan <- o .: "span"
    traitparamTrait <- o .: "trait_"
    pure (TraitParam traitparamClauseId traitparamSpan traitparamTrait)


instance FromJSON TraitRef where
  parseJSON = withObject "TraitRef" $ \o -> do
    traitrefKind <- o .: "kind"
    traitrefTraitDeclRef <- o .: "trait_decl_ref"
    pure (TraitRef traitrefKind traitrefTraitDeclRef)


instance FromJSON TraitRefKind where
  parseJSON v = case v of
    Object o | H.lookup "TraitImpl" o /= Nothing -> do
      v <- o .: "TraitImpl"
      TraitImpl <$> parseJSON v
    Object o | H.lookup "Clause" o /= Nothing -> do
      v <- o .: "Clause"
      Clause <$> parseJSON v
    Object o | H.lookup "ParentClause" o /= Nothing -> do
      withArray "ParentClause" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (ParentClause v0 v1)) =<< o .: "ParentClause"
    Object o | H.lookup "ItemClause" o /= Nothing -> do
      withArray "ItemClause" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (ItemClause v0 v1 v2)) =<< o .: "ItemClause"
    String "SelfId" -> pure Self
    Object o | H.lookup "BuiltinOrAuto" o /= Nothing -> do
      obj <- o .: "BuiltinOrAuto"
      builtinData <- obj .: "builtin_data"
      parentTraitRefs <- obj .: "parent_trait_refs"
      types <- obj .: "types"
      pure (BuiltinOrAuto builtinData parentTraitRefs types)
    String "Dyn" -> pure Dyn
    Object o | H.lookup "Unknown" o /= Nothing -> do
      v <- o .: "Unknown"
      UnknownTrait <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON TraitTypeConstraint where
  parseJSON = withObject "TraitTypeConstraint" $ \o -> do
    traittypeconstraintTraitRef <- o .: "trait_ref"
    traittypeconstraintTypeName <- o .: "type_name"
    traittypeconstraintTy <- o .: "ty"
    pure (TraitTypeConstraint traittypeconstraintTraitRef traittypeconstraintTypeName traittypeconstraintTy)


instance FromJSON Ty where
  parseJSON v = case v of
    Object o | H.lookup "Adt" o /= Nothing -> do
      v <- o .: "Adt"
      TAdt <$> parseJSON v
    Object o | H.lookup "TypeVar" o /= Nothing -> do
      v <- o .: "TypeVar"
      TVar <$> parseJSON v
    Object o | H.lookup "Literal" o /= Nothing -> do
      v <- o .: "Literal"
      TLiteral <$> parseJSON v
    String "Never" -> pure TNever
    Object o | H.lookup "Ref" o /= Nothing -> do
      withArray "TRef" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (TRef v0 v1 v2)) =<< o .: "Ref"
    Object o | H.lookup "RawPtr" o /= Nothing -> do
      withArray "TRawPtr" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (TRawPtr v0 v1)) =<< o .: "RawPtr"
    Object o | H.lookup "TraitType" o /= Nothing -> do
      withArray "TTraitType" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (TTraitType v0 v1)) =<< o .: "TraitType"
    Object o | H.lookup "DynTrait" o /= Nothing -> do
      v <- o .: "DynTrait"
      TDynTrait <$> parseJSON v
    Object o | H.lookup "FnPtr" o /= Nothing -> do
      v <- o .: "FnPtr"
      TFnPtr <$> parseJSON v
    Object o | H.lookup "FnDef" o /= Nothing -> do
      v <- o .: "FnDef"
      TFnDef <$> parseJSON v
    Object o | H.lookup "PtrMetadata" o /= Nothing -> do
      v <- o .: "PtrMetadata"
      TPtrMetadata <$> parseJSON v
    Object o | H.lookup "Error" o /= Nothing -> do
      v <- o .: "Error"
      TError <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON TypeDecl where
  parseJSON = withObject "TypeDecl" $ \o -> do
    typedeclDefId <- o .: "def_id"
    typedeclItemMeta <- o .: "item_meta"
    typedeclGenerics <- o .: "generics"
    typedeclSrc <- o .: "src"
    typedeclKind <- o .: "kind"
    typedeclLayout <- o .: "layout"
    typedeclPtrMetadata <- o .: "ptr_metadata"
    typedeclRepr <- o .: "repr"
    pure (TypeDecl typedeclDefId typedeclItemMeta typedeclGenerics typedeclSrc typedeclKind typedeclLayout typedeclPtrMetadata typedeclRepr)


instance FromJSON TypeDeclId where
  parseJSON = fmap TypeDeclId . parseJSON


instance FromJSON TypeDeclKind where
  parseJSON v = case v of
    Object o | H.lookup "Struct" o /= Nothing -> do
      v <- o .: "Struct"
      Struct <$> parseJSON v
    Object o | H.lookup "Enum" o /= Nothing -> do
      v <- o .: "Enum"
      Enum <$> parseJSON v
    Object o | H.lookup "Union" o /= Nothing -> do
      v <- o .: "Union"
      Union <$> parseJSON v
    String "Opaque" -> pure Opaque
    Object o | H.lookup "Alias" o /= Nothing -> do
      v <- o .: "Alias"
      Alias <$> parseJSON v
    Object o | H.lookup "Error" o /= Nothing -> do
      v <- o .: "Error"
      TDeclError <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON TypeDeclRef where
  parseJSON = withObject "TypeDeclRef" $ \o -> do
    typedeclrefId <- o .: "id"
    typedeclrefGenerics <- o .: "generics"
    pure (TypeDeclRef typedeclrefId typedeclrefGenerics)


instance FromJSON TypeId where
  parseJSON v = case v of
    Object o | H.lookup "Adt" o /= Nothing -> do
      v <- o .: "Adt"
      TAdtId <$> parseJSON v
    String "Tuple" -> pure TTuple
    Object o | H.lookup "Builtin" o /= Nothing -> do
      v <- o .: "Builtin"
      TBuiltin <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON TypeParam where
  parseJSON = withObject "TypeParam" $ \o -> do
    typeparamIndex <- o .: "index"
    typeparamName <- o .: "name"
    pure (TypeParam typeparamIndex typeparamName)


instance FromJSON TypeVarId where
  parseJSON = fmap TypeVarId . parseJSON


instance FromJSON UIntTy where
  parseJSON v = case v of
    String "Usize" -> pure Usize
    String "U8" -> pure U8
    String "U16" -> pure U16
    String "U32" -> pure U32
    String "U64" -> pure U64
    String "U128" -> pure U128
    _ -> fail "Unknown variant"


instance FromJSON Variant where
  parseJSON = withObject "Variant" $ \o -> do
    variantSpan <- o .: "span"
    variantAttrInfo <- o .: "attr_info"
    variantVariantName <- o .: "name"
    variantFields <- o .: "fields"
    variantDiscriminant <- o .: "discriminant"
    pure (Variant variantSpan variantAttrInfo variantVariantName variantFields variantDiscriminant)


instance FromJSON VariantId where
  parseJSON = fmap VariantId . parseJSON


instance FromJSON VariantLayout where
  parseJSON = withObject "VariantLayout" $ \o -> do
    variantlayoutFieldOffsets <- o .: "field_offsets"
    variantlayoutUninhabited <- o .: "uninhabited"
    variantlayoutTag <- o .: "tag"
    pure (VariantLayout variantlayoutFieldOffsets variantlayoutUninhabited variantlayoutTag)

