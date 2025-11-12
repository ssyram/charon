{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Types.hs`
by hand. Edit `templates/Types.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Types where

import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Generated_Meta
import Generated_Values

-- | (U)LLBC is a language with side-effects: a statement may abort in a way that isn't tracked by
-- | control-flow. The two kinds of abort are:
-- | - Panic (may unwind or not depending on compilation setting);
-- | - Undefined behavior:
data AbortKind = Panic Maybe Name
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
  { binderParams :: GenericParams
  ,   -- | Named this way to highlight accesses to the inner value that might be handling parameters
  -- | incorrectly. Prefer using helper methods.
  binderValue :: a0
  }
  deriving (Show, Eq, Ord)

data BinderKind = BkTraitType TraitDeclId TraitItemName
  | BkTraitMethod TraitDeclId TraitItemName
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
  | Index BuiltinIndexOp
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
  isArray :: Bool
  ,   -- | Whether we're indexing mutably or not. Determines the type ofreference of the input and
  -- | output.
  mutability :: RefKind
  ,   -- | Whether we're indexing a single element or a subrange. If `true`, the function takes
  -- | two indices and the output is a slice; otherwise, the function take one index and the
  -- | output is a reference to a single element.
  isRange :: Bool
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
  { kind :: ClosureKind
  ,   -- | The `FnOnce` implementation of this closure -- always exists.
  fnOnceImpl :: (RegionBinder TraitImplRef)
  ,   -- | The `FnMut` implementation of this closure, if any.
  fnMutImpl :: Maybe (RegionBinder TraitImplRef)
  ,   -- | The `Fn` implementation of this closure, if any.
  fnImpl :: Maybe (RegionBinder TraitImplRef)
  ,   -- | The signature of the function that this closure represents.
  signature :: (RegionBinder ([Ty], Ty))
  }
  deriving (Show, Eq, Ord)

data ClosureKind = Fn
  | FnMut
  | FnOnce
  deriving (Show, Eq, Ord)

-- | Const Generic Values. Either a primitive value, or a variable corresponding to a primitve value
data ConstGeneric = CgGlobal GlobalDeclId
  | CgVar (DeBruijnVar ConstGenericVarId)
  | CgValue Literal
  deriving (Show, Eq, Ord)

-- | A const generic variable in a signature or binder.
data ConstGenericParam = ConstGenericParam
  {   -- | Index identifying the variable among other variables bound at the same level.
  index :: ConstGenericVarId
  ,   -- | Const generic name
  name :: String
  ,   -- | Type of the const generic
  ty :: LiteralType
  }
  deriving (Show, Eq, Ord)

data ConstGenericVarId = ConstGenericVarId
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

-- | The index of a binder, counting from the innermost. See [`DeBruijnVar`] for details.
data DeBruijnId = DeBruijnId
  { index :: Int
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
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Layout of the discriminant.
-- | Describes the offset of the discriminant field as well as its encoding
-- | as `tag` in memory.
data DiscriminantLayout = DiscriminantLayout
  {   -- | The offset of the discriminant in bytes.
  offset :: Int
  ,   -- | The representation type of the discriminant.
  tagTy :: IntegerType
  ,   -- | How the tag is encoding in memory.
  encoding :: TagEncoding
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
  binder :: (Binder Ty)
  }
  deriving (Show, Eq, Ord)

data Field = Field
  { span :: Span
  , attrInfo :: AttrInfo
  , fieldName :: Maybe String
  , fieldTy :: Ty
  }
  deriving (Show, Eq, Ord)

data FieldId = FieldId
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

data FnPtr = FnPtr
  { kind :: FnPtrKind
  , generics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

data FnPtrKind = FunId FunId
  | TraitMethod TraitRef TraitItemName FunDeclId
  deriving (Show, Eq, Ord)

data FunDeclId = FunDeclId
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Reference to a function declaration.
data FunDeclRef = FunDeclRef
  { id :: FunDeclId
  ,   -- | Generic arguments passed to the function.
  generics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

-- | A function identifier. See [crate::ullbc_ast::Terminator]
data FunId = FRegular FunDeclId
  | FBuiltin BuiltinFunId
  deriving (Show, Eq, Ord)

-- | A set of generic arguments.
data GenericArgs = GenericArgs
  { regions :: [RegionId]
  , types :: [TypeVarId]
  , constGenerics :: [ConstGenericVarId]
  , traitRefs :: [TraitClauseId]
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
  { regions :: [RegionId]
  , types :: [TypeVarId]
  , constGenerics :: [ConstGenericVarId]
  , traitClauses :: [TraitClauseId]
  ,   -- | The first region in the pair outlives the second region
  regionsOutlive :: [(RegionBinder (OutlivesPred Region Region))]
  ,   -- | The type outlives the region
  typesOutlive :: [(RegionBinder (OutlivesPred Ty Region))]
  ,   -- | Constraints over trait associated types
  traitTypeConstraints :: [TraitTypeConstraintId]
  }
  deriving (Show, Eq, Ord)

data GlobalDeclId = GlobalDeclId
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Reference to a global declaration.
data GlobalDeclRef = GlobalDeclRef
  { id :: GlobalDeclId
  , generics :: GenericArgs
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
data ImplElem = ImplElemTy (Binder Ty)
  | ImplElemTrait TraitImplId
  deriving (Show, Eq, Ord)

-- | The id of a translated item.
data ItemId = IdType TypeDeclId
  | IdFun FunDeclId
  | IdGlobal GlobalDeclId
  | IdTraitDecl TraitDeclId
  | IdTraitImpl TraitImplId
  deriving (Show, Eq, Ord)

-- | Meta information about an item (function, trait decl, trait impl, type decl, global).
data ItemMeta = ItemMeta
  { name :: Name
  , span :: Span
  ,   -- | The source code that corresponds to this item.
  sourceText :: Maybe String
  ,   -- | Attributes and visibility.
  attrInfo :: AttrInfo
  ,   -- | `true` if the type decl is a local type decl, `false` if it comes from an external crate.
  isLocal :: Bool
  ,   -- | If the item is built-in, record its internal builtin identifier.
  langItem :: Maybe String
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
  | TraitDeclItem TraitDeclRef TraitItemName Bool
  | TraitImplItem TraitImplRef TraitDeclRef TraitItemName Bool
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
  size :: Maybe Int
  ,   -- | The alignment, in bytes.
  align :: Maybe Int
  ,   -- | The discriminant's layout, if any. Only relevant for types with multiple variants.
  discriminantLayout :: Maybe DiscriminantLayout
  ,   -- | Whether the type is uninhabited, i.e. has any valid value at all.
  -- | Note that uninhabited types can have arbitrary layouts: `(u32, !)` has space for the `u32`
  -- | and `enum E2 { A, B(!), C(i32, !) }` may have space for a discriminant.
  uninhabited :: Bool
  ,   -- | Map from `VariantId` to the corresponding field layouts. Structs are modeled as having
  -- | exactly one variant, unions as having no variant.
  variantLayouts :: [VariantId]
  }
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
  { name :: [PathElem]
  }
  deriving (Show, Eq, Ord)

-- | .0 outlives .1
data OutlivesPred a0 a1 = OutlivesPred a0 a1
  deriving (Show, Eq, Ord)

-- | See the comments for [Name]
data PathElem = PeIdent String Disambiguator
  | PeImpl ImplElem
  | PeInstantiated (Binder GenericArgs)
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

data Region = RVar (DeBruijnVar RegionId)
  | RStatic
  | RErased
  deriving (Show, Eq, Ord)

-- | A value of type `T` bound by regions. We should use `binder` instead but this causes name clash
-- | issues in the derived ocaml visitors.
-- | TODO: merge with `binder`
data RegionBinder a0 = RegionBinder
  { binderRegions :: [RegionId]
  ,   -- | Named this way to highlight accesses to the inner value that might be handling parameters
  -- | incorrectly. Prefer using helper methods.
  binderValue :: a0
  }
  deriving (Show, Eq, Ord)

data RegionId = RegionId
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

-- | A region variable in a signature or binder.
data RegionParam = RegionParam
  {   -- | Index identifying the variable among other variables bound at the same level.
  index :: RegionId
  ,   -- | Region name
  name :: Maybe String
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
  { reprAlgo :: ReprAlgorithm
  , alignModif :: Maybe AlignmentModifier
  , transparent :: Bool
  , explicitDiscrType :: Bool
  }
  deriving (Show, Eq, Ord)

-- | Describes how we represent the active enum variant in memory.
data TagEncoding = Direct
  | Niche VariantId
  deriving (Show, Eq, Ord)

-- | The value of a trait associated type.
data TraitAssocTyImpl = TraitAssocTyImpl
  { value :: Ty
  }
  deriving (Show, Eq, Ord)

data TraitClauseId = TraitClauseId
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

data TraitDeclId = TraitDeclId
  { raw :: Int
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
  { id :: TraitDeclId
  , generics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

data TraitImplId = TraitImplId
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

-- | A reference to a tait impl, using the provided arguments.
data TraitImplRef = TraitImplRef
  { id :: TraitImplId
  , generics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

data TraitItemName = TraitItemName Text
  deriving (Show, Eq, Ord)

-- | A trait predicate in a signature, of the form `Type: Trait<Args>`. This functions like a
-- | variable binder, to which variables of the form `TraitRefKind::Clause` can refer to.
data TraitParam = TraitParam
  {   -- | Index identifying the clause among other clauses bound at the same level.
  clauseId :: TraitClauseId
  , span :: Maybe Span
  ,   -- | The trait that is implemented.
  trait :: (RegionBinder TraitDeclRef)
  }
  deriving (Show, Eq, Ord)

-- | A reference to a trait
data TraitRef = TraitRef
  { kind :: TraitRefKind
  ,   -- | Not necessary, but useful
  traitDeclRef :: (RegionBinder TraitDeclRef)
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
  | Clause (DeBruijnVar TraitClauseId)
  | ParentClause TraitRef TraitClauseId
  | ItemClause TraitRef TraitItemName TraitClauseId
  | Self
  | BuiltinOrAuto BuiltinImplData [TraitClauseId] [(TraitItemName, TraitAssocTyImpl)]
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
  { traitRef :: TraitRef
  , typeName :: TraitItemName
  , ty :: Ty
  }
  deriving (Show, Eq, Ord)

data Ty = TAdt TypeDeclRef
  | TVar (DeBruijnVar TypeVarId)
  | TLiteral LiteralType
  | TNever
  | TRef Region Ty RefKind
  | TRawPtr Ty RefKind
  | TTraitType TraitRef TraitItemName
  | TDynTrait DynPredicate
  | TFnPtr (RegionBinder ([Ty], Ty))
  | TFnDef (RegionBinder FnPtr)
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
  { defId :: TypeDeclId
  ,   -- | Meta information associated with the item.
  itemMeta :: ItemMeta
  , generics :: GenericParams
  ,   -- | The context of the type: distinguishes top-level items from closure-related items.
  src :: ItemSource
  ,   -- | The type kind: enum, struct, or opaque.
  kind :: TypeDeclKind
  ,   -- | The layout of the type. Information may be partial because of generics or dynamically-
  -- | sized types. If rustc cannot compute a layout, it is `None`.
  layout :: Maybe Layout
  ,   -- | The metadata associated with a pointer to the type.
  ptrMetadata :: PtrMetadata
  ,   -- | The representation options of this type declaration as annotated by the user.
  -- | Is `None` for foreign type declarations.
  repr :: Maybe ReprOptions
  }
  deriving (Show, Eq, Ord)

data TypeDeclId = TypeDeclId
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

data TypeDeclKind = Struct [FieldId]
  | Enum [VariantId]
  | Union [FieldId]
  | Opaque
  | Alias Ty
  | TDeclError String
  deriving (Show, Eq, Ord)

-- | Reference to a type declaration or builtin type.
data TypeDeclRef = TypeDeclRef
  { id :: TypeId
  , generics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

-- | Type identifier.
-- | 
-- | Allows us to factorize the code for built-in types, adts and tuples
data TypeId = TAdtId TypeDeclId
  | TTuple
  | TBuiltin BuiltinTy
  deriving (Show, Eq, Ord)

-- | A type variable in a signature or binder.
data TypeParam = TypeParam
  {   -- | Index identifying the variable among other variables bound at the same level.
  index :: TypeVarId
  ,   -- | Variable name
  name :: String
  }
  deriving (Show, Eq, Ord)

data TypeVarId = TypeVarId
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

data Variant = Variant
  { span :: Span
  , attrInfo :: AttrInfo
  , variantName :: String
  , fields :: [FieldId]
  ,   -- | The discriminant value outputted by `std::mem::discriminant` for this variant.
  -- | This can be different than the discriminant stored in memory (called `tag`).
  -- | That one is described by [`DiscriminantLayout`] and [`TagEncoding`].
  discriminant :: Literal
  }
  deriving (Show, Eq, Ord)

data VariantId = VariantId
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Simplified layout of a single variant.
-- | 
-- | Maps fields to their offset within the layout.
data VariantLayout = VariantLayout
  {   -- | The offset of each field.
  fieldOffsets :: [FieldId]
  ,   -- | Whether the variant is uninhabited, i.e. has any valid possible value.
  -- | Note that uninhabited types can have arbitrary layouts.
  uninhabited :: Bool
  ,   -- | The memory representation of the discriminant corresponding to this
  -- | variant. It must be of the same type as the corresponding [`DiscriminantLayout::tag_ty`].
  -- | 
  -- | If it's `None`, then this variant is either:
  -- | - the untagged variant (cf. [`TagEncoding::Niche::untagged_variant`]) of a niched enum;
  -- | - the single variant of a struct;
  -- | - uninhabited.
  tag :: Maybe ScalarValue
  }
  deriving (Show, Eq, Ord)
