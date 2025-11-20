{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Meta.hs`
by hand. Edit `templates/Meta.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.

`templates/Meta.hs` contains the manual definitions and some `{- __REPLACEn__ -}`
comments. These comments are replaced by auto-generated definitions by running
`make generate-hs` in the crate root. The code-generation code is in
`charon/src/bin/generate-hs`.
-}

module Generated_Meta where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (catMaybes)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V

-- Using newtype instead of type alias to avoid duplicate instance issues
newtype PathBuf = PathBuf Text
  deriving (Show, Eq, Ord)

instance FromJSON PathBuf where
  parseJSON v = PathBuf <$> parseJSON v

-- Vector is used for indexed sequences in Rust (IndexVec in charon)
-- Defined here to avoid circular dependencies
newtype Vector k v = Vector [v]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance FromJSON b => FromJSON (Vector a b) where
  parseJSON = fmap (Vector . catMaybes) . parseJSON

-- KVPair is used to deserialize HashMap serialized with HashMapToArray
-- which creates array of {key, value} objects instead of tuples
data KVPair k v = KVPair { kvpairKey :: k, kvpairValue :: v }
  deriving (Show, Eq, Ord)

instance (FromJSON k, FromJSON v) => FromJSON (KVPair k v) where
  parseJSON = withObject "KVPair" $ \o -> do
    key <- o .: "key"
    value <- o .: "value"
    pure (KVPair key value)

-- Helper function to parse Integer values that are serialized as strings
parseIntegerValue :: Value -> Parser Integer
parseIntegerValue v = case v of
  String s -> case reads (Text.unpack s) of
    [(n, "")] -> pure n
    _ -> fail $ "Failed to parse integer from string: " ++ Text.unpack s
  Number n -> pure (floor n)
  _ -> fail "Expected String or Number for integer value"

-- | Information about the attributes and visibility of an item, field or variant..
data AttrInfo = AttrInfo
  {   -- | Attributes (`#[...]`).
  attrinfoAttributes :: [Attribute]
  ,   -- | Inline hints (on functions only).
  attrinfoInline :: Maybe InlineAttr
  ,   -- | The name computed from `charon::rename` and `charon::variants_prefix` attributes, if any.
  -- | This provides a custom name that can be used by consumers of llbc. E.g. Aeneas uses this to
  -- | rename definitions in the extracted code.
  attrinfoRename :: Maybe String
  ,   -- | Whether this item is declared public. Impl blocks and closures don't have visibility
  -- | modifiers; we arbitrarily set this to `false` for them.
  -- | 
  -- | Note that this is different from being part of the crate's public API: to be part of the
  -- | public API, an item has to also be reachable from public items in the crate root. For
  -- | example:
  -- | ```rust,ignore
  -- | mod foo {
  -- |     pub struct X;
  -- | }
  -- | mod bar {
  -- |     pub fn something(_x: super::foo::X) {}
  -- | }
  -- | pub use bar::something; // exposes `X`
  -- | ```
  -- | Without the `pub use ...`, neither `X` nor `something` would be part of the crate's public
  -- | API (this is called "pub-in-priv" items). With or without the `pub use`, we set `public =
  -- | true`; computing item reachability is harder.
  attrinfoPublic :: Bool
  }
  deriving (Show, Eq, Ord)

-- | Attributes (`#[...]`).
data Attribute = AttrOpaque
  | AttrRename String
  | AttrVariantsPrefix String
  | AttrVariantsSuffix String
  | AttrDocComment String
  | AttrUnknown RawAttribute
  deriving (Show, Eq, Ord)

data File = File
  {   -- | The path to the file.
  fileName :: FileName
  ,   -- | Name of the crate this file comes from.
  fileCrateName :: String
  ,   -- | The contents of the source file, as seen by rustc at the time of translation.
  -- | Some files don't have contents.
  fileContents :: Maybe String
  }
  deriving (Show, Eq, Ord)

data FileId = FileId
  { fileidRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | A filename.
data FileName = Virtual PathBuf
  | Local PathBuf
  deriving (Show, Eq, Ord)

-- | `#[inline]` built-in attribute.
data InlineAttr = Hint
  | Never
  | Always
  deriving (Show, Eq, Ord)

data Loc = Loc
  {   -- | The (1-based) line number.
  locLine :: Int
  ,   -- | The (0-based) column offset.
  locCol :: Int
  }
  deriving (Show, Eq, Ord)

-- | A general attribute.
data RawAttribute = RawAttribute
  { rawattributePath :: String
  ,   -- | The arguments passed to the attribute, if any. We don't distinguish different delimiters or
  -- | the `path = lit` case.
  rawattributeArgs :: Maybe String
  }
  deriving (Show, Eq, Ord)

-- | Meta information about a piece of code (block, statement, etc.)
data Span = Span
  {   -- | The source code span.
  -- | 
  -- | If this meta information is for a statement/terminator coming from a macro
  -- | expansion/inlining/etc., this span is (in case of macros) for the macro
  -- | before expansion (i.e., the location the code where the user wrote the call
  -- | to the macro).
  -- | 
  -- | Ex:
  -- | ```text
  -- | // Below, we consider the spans for the statements inside `test`
  -- | 
  -- | //   the statement we consider, which gets inlined in `test`
  -- |                          VV
  -- | macro_rules! macro { ... st ... } // `generated_from_span` refers to this location
  -- | 
  -- | fn test() {
  -- |     macro!(); // <-- `span` refers to this location
  -- | }
  -- | ```
  spanData :: SpanData
  ,   -- | Where the code actually comes from, in case of macro expansion/inlining/etc.
  spanGeneratedFromSpan :: Maybe SpanData
  }
  deriving (Show, Eq, Ord)

-- | Span information
data SpanData = SpanData
  { spandataFile :: FileId
  , spandataBegLoc :: Loc
  , spandataEndLoc :: Loc
  }
  deriving (Show, Eq, Ord)

instance FromJSON AttrInfo where
  parseJSON = withObject "AttrInfo" $ \o -> do
    attrinfoAttributes <- o .: "attributes"
    attrinfoInline <- o .: "inline"
    attrinfoRename <- o .: "rename"
    attrinfoPublic <- o .: "public"
    pure (AttrInfo attrinfoAttributes attrinfoInline attrinfoRename attrinfoPublic)


instance FromJSON Attribute where
  parseJSON v = case v of
    String "Opaque" -> pure AttrOpaque
    Object o | H.lookup "Rename" o /= Nothing -> do
      v <- o .: "Rename"
      AttrRename <$> parseJSON v
    Object o | H.lookup "VariantsPrefix" o /= Nothing -> do
      v <- o .: "VariantsPrefix"
      AttrVariantsPrefix <$> parseJSON v
    Object o | H.lookup "VariantsSuffix" o /= Nothing -> do
      v <- o .: "VariantsSuffix"
      AttrVariantsSuffix <$> parseJSON v
    Object o | H.lookup "DocComment" o /= Nothing -> do
      v <- o .: "DocComment"
      AttrDocComment <$> parseJSON v
    Object o | H.lookup "Unknown" o /= Nothing -> do
      v <- o .: "Unknown"
      AttrUnknown <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON File where
  parseJSON = withObject "File" $ \o -> do
    fileName <- o .: "name"
    fileCrateName <- o .: "crate_name"
    fileContents <- o .: "contents"
    pure (File fileName fileCrateName fileContents)


instance FromJSON FileId where
  parseJSON = fmap FileId . parseJSON


instance FromJSON FileName where
  parseJSON v = case v of
    Object o | H.lookup "Virtual" o /= Nothing -> do
      v <- o .: "Virtual"
      Virtual <$> parseJSON v
    Object o | H.lookup "Local" o /= Nothing -> do
      v <- o .: "Local"
      Local <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON InlineAttr where
  parseJSON v = case v of
    String "Hint" -> pure Hint
    String "Never" -> pure Never
    String "Always" -> pure Always
    _ -> fail "Unknown variant"


instance FromJSON Loc where
  parseJSON = withObject "Loc" $ \o -> do
    locLine <- o .: "line"
    locCol <- o .: "col"
    pure (Loc locLine locCol)


instance FromJSON RawAttribute where
  parseJSON = withObject "RawAttribute" $ \o -> do
    rawattributePath <- o .: "path"
    rawattributeArgs <- o .: "args"
    pure (RawAttribute rawattributePath rawattributeArgs)


instance FromJSON Span where
  parseJSON = withObject "Span" $ \o -> do
    spanData <- o .: "data"
    spanGeneratedFromSpan <- o .: "generated_from_span"
    pure (Span spanData spanGeneratedFromSpan)


instance FromJSON SpanData where
  parseJSON = withObject "SpanData" $ \o -> do
    spandataFile <- o .: "file_id"
    spandataBegLoc <- o .: "beg"
    spandataEndLoc <- o .: "end"
    pure (SpanData spandataFile spandataBegLoc spandataEndLoc)

