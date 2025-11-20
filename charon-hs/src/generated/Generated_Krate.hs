{-# LANGUAGE StrictData #-}

module Generated_Krate where

import Data.Aeson (FromJSON(..), Value(..), withObject, withArray, (.:), (.!=))
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import qualified Generated_Meta as M
import qualified Generated_Types as T
import qualified Generated_Values as Val
import qualified Generated_Expressions as E
import qualified Generated_GAst as G
import qualified Generated_LlbcAst as L
import qualified Generated_UllbcAst as U

-- | The body of a function.
data Body = Unstructured ((G.GexprBody (M.Vector U.BlockId U.Block)))
  | Structured ((G.GexprBody L.Block))
  | TraitMethodWithoutDefault
  | Opaque
  | Missing
  | Error G.Error
  deriving (Show, Eq, Ord)

-- | A function definition
data FunDecl = FunDecl
  { fundeclDefId :: T.FunDeclId
  ,   -- | The meta data associated with the declaration.
  fundeclItemMeta :: T.ItemMeta
  ,   -- | The signature contains the inputs/output types *with* non-erased regions.
  -- | It also contains the list of region and type parameters.
  fundeclSignature :: G.FunSig
  ,   -- | The function kind: "regular" function, trait method declaration, etc.
  fundeclSrc :: T.ItemSource
  ,   -- | Whether this function is in fact the body of a constant/static that we turned into an
  -- | initializer function.
  fundeclIsGlobalInitializer :: Maybe T.GlobalDeclId
  ,   -- | The function body, unless the function is opaque.
  -- | Opaque functions are: external functions, or local functions tagged
  -- | as opaque.
  fundeclBody :: Body
  }
  deriving (Show, Eq, Ord)

-- | The data of a translated crate.
data TranslatedCrate = TranslatedCrate
  {   -- | The name of the crate.
  translatedcrateCrateName :: String
  ,   -- | The options used when calling Charon. It is useful for the applications
  -- | which consumed the serialized code, to check that Charon was called with
  -- | the proper options.
  translatedcrateOptions :: G.CliOptions
  ,   -- | Information about the target platform for which rustc is called on for the crate.
  translatedcrateTargetInformation :: G.TargetInfo
  ,   -- | The names of all registered items. Available so we can know the names even of items that
  -- | failed to translate.
  -- | Invariant: after translation, any existing `ItemId` must have an associated name, even
  -- | if the corresponding item wasn't translated.
  translatedcrateItemNames :: [M.KVPair T.ItemId T.Name]
  ,   -- | Short names, for items whose last PathElem is unique.
  translatedcrateShortNames :: [M.KVPair T.ItemId T.Name]
  ,   -- | The translated files.
  translatedcrateFiles :: (M.Vector M.FileId M.File)
  ,   -- | The translated type definitions
  translatedcrateTypeDecls :: (M.Vector T.TypeDeclId T.TypeDecl)
  ,   -- | The translated function definitions
  translatedcrateFunDecls :: (M.Vector T.FunDeclId FunDecl)
  ,   -- | The translated global definitions
  translatedcrateGlobalDecls :: (M.Vector T.GlobalDeclId G.GlobalDecl)
  ,   -- | The translated trait declarations
  translatedcrateTraitDecls :: (M.Vector T.TraitDeclId G.TraitDecl)
  ,   -- | The translated trait declarations
  translatedcrateTraitImpls :: (M.Vector T.TraitImplId G.TraitImpl)
  ,   -- | A `const UNIT: () = ();` used whenever we make a thin pointer/reference to avoid creating a
  -- | local `let unit = ();` variable. It is always `Some`.
  translatedcrateUnitMetadata :: Maybe T.GlobalDeclRef
  ,   -- | The re-ordered groups of declarations, initialized as empty.
  translatedcrateOrderedDecls :: Maybe [G.DeclarationGroup]
  }
  deriving (Show, Eq, Ord)

instance FromJSON Body where
  parseJSON v = case v of
    Object o | H.lookup "Unstructured" o /= Nothing -> do
      v <- o .: "Unstructured"
      Unstructured <$> parseJSON v
    Object o | H.lookup "Structured" o /= Nothing -> do
      v <- o .: "Structured"
      Structured <$> parseJSON v
    String "TraitMethodWithoutDefault" -> pure TraitMethodWithoutDefault
    String "Opaque" -> pure Opaque
    String "Missing" -> pure Missing
    Object o | H.lookup "Error" o /= Nothing -> do
      v <- o .: "Error"
      Error <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON FunDecl where
  parseJSON = withObject "FunDecl" $ \o -> do
    fundeclDefId <- o .: "def_id"
    fundeclItemMeta <- o .: "item_meta"
    fundeclSignature <- o .: "signature"
    fundeclSrc <- o .: "src"
    fundeclIsGlobalInitializer <- o .: "is_global_initializer"
    fundeclBody <- o .: "body"
    pure (FunDecl fundeclDefId fundeclItemMeta fundeclSignature fundeclSrc fundeclIsGlobalInitializer fundeclBody)


instance FromJSON TranslatedCrate where
  parseJSON = withObject "TranslatedCrate" $ \o -> do
    translatedcrateCrateName <- o .: "crate_name"
    translatedcrateOptions <- o .: "options"
    translatedcrateTargetInformation <- o .: "target_information"
    translatedcrateItemNames <- o .: "item_names"
    translatedcrateShortNames <- o .: "short_names"
    translatedcrateFiles <- o .: "files"
    translatedcrateTypeDecls <- o .: "type_decls"
    translatedcrateFunDecls <- o .: "fun_decls"
    translatedcrateGlobalDecls <- o .: "global_decls"
    translatedcrateTraitDecls <- o .: "trait_decls"
    translatedcrateTraitImpls <- o .: "trait_impls"
    translatedcrateUnitMetadata <- o .: "unit_metadata"
    translatedcrateOrderedDecls <- o .: "ordered_decls"
    pure (TranslatedCrate translatedcrateCrateName translatedcrateOptions translatedcrateTargetInformation translatedcrateItemNames translatedcrateShortNames translatedcrateFiles translatedcrateTypeDecls translatedcrateFunDecls translatedcrateGlobalDecls translatedcrateTraitDecls translatedcrateTraitImpls translatedcrateUnitMetadata translatedcrateOrderedDecls)


-- Wrapper type for the top-level LLBC file structure
-- This is a Haskell-specific convenience type, not from Rust
data LlbcFile = LlbcFile
  { llbcfileCharonVersion :: String
  , llbcfileTranslated :: TranslatedCrate
  }
  deriving (Show, Eq, Ord)

instance FromJSON LlbcFile where
  parseJSON = withObject "LlbcFile" $ \o -> do
    charonVersion <- o .: "charon_version"
    translated <- o .: "translated"
    pure $ LlbcFile charonVersion translated
