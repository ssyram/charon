{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module: Charon.Printing
Description: Printing mechanism for the generated Haskell AST

This module provides a printing mechanism for the Charon AST, mirroring the
functionality in charon/src/pretty/fmt_with_ctx.rs. It uses Builder for
efficient string construction.
-}
module Charon.Printing
  ( -- * Context
    PrintingCtx(..)
  , emptyCtx
  , pushGenerics
  , setLocals
  , increaseIndent
  , indent
    -- * Typeclass
  , BuildWithCtx(..)
  , printWithCtx
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Generated_GAst
import Generated_Types
import Generated_Values
import Generated_Expressions

-- | Tab increment for indentation (4 spaces)
tabIncr :: Text
tabIncr = "    "

-- | The context (analogous to AstFormatter trait in Rust)
data PrintingCtx = PrintingCtx
  { translated :: Maybe TranslatedCrate
  , generics :: [GenericParams]  -- binding stack as pure list
  , locals :: Maybe Locals
  , indentLevel :: Int
  } deriving (Show, Eq)

-- | Create an empty printing context
emptyCtx :: PrintingCtx
emptyCtx = PrintingCtx
  { translated = Nothing
  , generics = []
  , locals = Nothing
  , indentLevel = 0
  }

-- | Push generics onto the binding stack
pushGenerics :: GenericParams -> PrintingCtx -> PrintingCtx
pushGenerics g ctx = ctx { generics = g : generics ctx }

-- | Set the locals in the context
setLocals :: Locals -> PrintingCtx -> PrintingCtx
setLocals l ctx = ctx { locals = Just l }

-- | Increase indentation level
increaseIndent :: PrintingCtx -> PrintingCtx
increaseIndent ctx = ctx { indentLevel = indentLevel ctx + 1 }

-- | Get the current indentation string
indent :: PrintingCtx -> Text
indent ctx = T.replicate (indentLevel ctx) tabIncr

-- | Single typeclass for formatting with context
class BuildWithCtx a where
  buildWithCtx :: PrintingCtx -> a -> Builder

-- | Convert to a lazy Text string using the context
printWithCtx :: BuildWithCtx a => PrintingCtx -> a -> String
printWithCtx ctx = TL.unpack . B.toLazyText . buildWithCtx ctx

-- Helper function to build from Text
fromText :: Text -> Builder
fromText = B.fromText

-- Helper function to build from String
fromString :: String -> Builder
fromString = B.fromString

--------------------
-- Basic instances
--------------------

instance BuildWithCtx Text where
  buildWithCtx _ = fromText

instance BuildWithCtx String where
  buildWithCtx _ = fromString

instance BuildWithCtx Int where
  buildWithCtx _ = B.decimal

instance BuildWithCtx Integer where
  buildWithCtx _ = B.decimal

instance BuildWithCtx Bool where
  buildWithCtx _ True = "true"
  buildWithCtx _ False = "false"

-- Maybe instance
instance BuildWithCtx a => BuildWithCtx (Maybe a) where
  buildWithCtx ctx (Just x) = buildWithCtx ctx x
  buildWithCtx _ Nothing = ""

-- List instance - renders comma-separated
instance BuildWithCtx a => BuildWithCtx [a] where
  buildWithCtx ctx xs = mconcat $ punctuate (fromText ", ") (map (buildWithCtx ctx) xs)
    where
      punctuate :: Builder -> [Builder] -> [Builder]
      punctuate _ [] = []
      punctuate _ [x] = [x]
      punctuate sep (x:xs') = (x <> sep) : punctuate sep xs'

--------------------
-- AST instances
--------------------

-- AbortKind
instance BuildWithCtx AbortKind where
  buildWithCtx ctx (Panic maybeName) = 
    "panic" <> case maybeName of
      Nothing -> ""
      Just n -> "(" <> buildWithCtx ctx n <> ")"
  buildWithCtx _ UndefinedBehavior = "undefined_behavior"
  buildWithCtx _ UnwindTerminate = "unwind_terminate"

-- Name
instance BuildWithCtx Name where
  buildWithCtx ctx (Name items) = 
    mconcat $ punctuate (fromText "::") (map (buildWithCtx ctx) items)
    where
      punctuate :: Builder -> [Builder] -> [Builder]
      punctuate _ [] = []
      punctuate _ [x] = [x]
      punctuate sep (x:xs') = (x <> sep) : punctuate sep xs'

-- PathElem
instance BuildWithCtx PathElem where
  buildWithCtx _ (PeIdent name (Disambiguator d)) = 
    fromString name <> if d == 0 then "" else "#" <> B.decimal d
  buildWithCtx _ (PeImpl _) = "{impl}"
  buildWithCtx _ (PeInstantiated _) = "{instantiated}"

-- Disambiguator
instance BuildWithCtx Disambiguator where
  buildWithCtx _ (Disambiguator d) = B.decimal d

-- OverflowMode
instance BuildWithCtx OverflowMode where
  buildWithCtx _ OPanic = "panic"
  buildWithCtx _ OWrap = "wrap"
  buildWithCtx _ Oub = "ub"

-- Binop
instance BuildWithCtx Binop where
  buildWithCtx _ BitXor = "^"
  buildWithCtx _ BitAnd = "&"
  buildWithCtx _ BitOr = "|"
  buildWithCtx _ Eq = "=="
  buildWithCtx _ Lt = "<"
  buildWithCtx _ Le = "<="
  buildWithCtx _ Ne = "!="
  buildWithCtx _ Ge = ">="
  buildWithCtx _ Gt = ">"
  buildWithCtx ctx (Add mode) = buildWithCtx ctx mode <> ".+"
  buildWithCtx ctx (Sub mode) = buildWithCtx ctx mode <> ".-"
  buildWithCtx ctx (Mul mode) = buildWithCtx ctx mode <> ".*"
  buildWithCtx ctx (Div mode) = buildWithCtx ctx mode <> "./"
  buildWithCtx ctx (Rem mode) = buildWithCtx ctx mode <> ".%"
  buildWithCtx _ AddChecked = "checked.+"
  buildWithCtx _ SubChecked = "checked.-"
  buildWithCtx _ MulChecked = "checked.*"
  buildWithCtx ctx (Shl mode) = buildWithCtx ctx mode <> ".<<"
  buildWithCtx ctx (Shr mode) = buildWithCtx ctx mode <> ".>>"
  buildWithCtx _ Cmp = "cmp"
  buildWithCtx _ Offset = "offset"

-- Unop
instance BuildWithCtx Unop where
  buildWithCtx _ Not = "~"
  buildWithCtx ctx (Neg mode) = buildWithCtx ctx mode <> ".-"
  buildWithCtx ctx (Cast ck) = buildWithCtx ctx ck

-- CastKind
instance BuildWithCtx CastKind where
  buildWithCtx ctx (CastScalar src tgt) = 
    "cast<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"
  buildWithCtx ctx (CastRawPtr src tgt) =
    "cast<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"
  buildWithCtx ctx (CastFnPtr src tgt) =
    "cast<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"
  buildWithCtx ctx (CastUnsize src tgt _meta) =
    "unsize_cast<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"
  buildWithCtx ctx (CastTransmute src tgt) =
    "transmute<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"
  buildWithCtx ctx (CastConcretize src tgt) =
    "concretize<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"

-- IntTy
instance BuildWithCtx IntTy where
  buildWithCtx _ Isize = "isize"
  buildWithCtx _ I8 = "i8"
  buildWithCtx _ I16 = "i16"
  buildWithCtx _ I32 = "i32"
  buildWithCtx _ I64 = "i64"
  buildWithCtx _ I128 = "i128"

-- UIntTy  
instance BuildWithCtx UIntTy where
  buildWithCtx _ Usize = "usize"
  buildWithCtx _ U8 = "u8"
  buildWithCtx _ U16 = "u16"
  buildWithCtx _ U32 = "u32"
  buildWithCtx _ U64 = "u64"
  buildWithCtx _ U128 = "u128"

-- FloatType
instance BuildWithCtx FloatType where
  buildWithCtx _ F16 = "f16"
  buildWithCtx _ F32 = "f32"
  buildWithCtx _ F64 = "f64"
  buildWithCtx _ F128 = "f128"

-- RefKind
instance BuildWithCtx RefKind where
  buildWithCtx _ RShared = "shared"
  buildWithCtx _ RMut = "mut"

-- BorrowKind
instance BuildWithCtx BorrowKind where
  buildWithCtx _ BShared = "Shared"
  buildWithCtx _ BMut = "Mut"
  buildWithCtx _ BTwoPhaseMut = "TwoPhaseMut"
  buildWithCtx _ BShallow = "Shallow"
  buildWithCtx _ BUniqueImmutable = "UniqueImmutable"

-- LocalId
instance BuildWithCtx LocalId where
  buildWithCtx ctx (LocalId i) =
    case locals ctx of
      Just (Locals _ _locs) -> 
        -- Try to get the variable name from the locals vector
        -- For now, just show the ID
        "@" <> B.decimal i
      Nothing -> "@" <> B.decimal i

-- FieldId
instance BuildWithCtx FieldId where
  buildWithCtx _ (FieldId i) = B.decimal i

-- VariantId
instance BuildWithCtx VariantId where
  buildWithCtx _ (VariantId i) = B.decimal i

-- TypeDeclId
instance BuildWithCtx TypeDeclId where
  buildWithCtx ctx (TypeDeclId i) =
    case translated ctx of
      Just _crate -> 
        -- Try to look up the type name in the crate
        -- For now, show as TypeDeclId
        "TypeDeclId(" <> B.decimal i <> ")"
      Nothing -> "TypeDeclId(" <> B.decimal i <> ")"

-- FunDeclId
instance BuildWithCtx FunDeclId where
  buildWithCtx _ (FunDeclId i) = "FunDeclId(" <> B.decimal i <> ")"

-- GlobalDeclId
instance BuildWithCtx GlobalDeclId where
  buildWithCtx _ (GlobalDeclId i) = "GlobalDeclId(" <> B.decimal i <> ")"

-- TraitDeclId
instance BuildWithCtx TraitDeclId where
  buildWithCtx _ (TraitDeclId i) = "TraitDeclId(" <> B.decimal i <> ")"

-- TraitImplId
instance BuildWithCtx TraitImplId where
  buildWithCtx _ (TraitImplId i) = "TraitImplId(" <> B.decimal i <> ")"

-- Assertion
instance BuildWithCtx Assertion where
  buildWithCtx ctx (Assertion cond expected _onFailure) =
    "assert(" <> buildWithCtx ctx cond <> " == " <> buildWithCtx ctx expected <> ")"

-- Call
instance BuildWithCtx Call where
  buildWithCtx ctx (Call func args dest) =
    buildWithCtx ctx dest <> " := " <> buildWithCtx ctx func <> "(" <>
    buildWithCtx ctx args <> ")"

-- BorrowKind already defined above, skipping duplicate

-- BuiltinFunId
instance BuildWithCtx BuiltinFunId where
  buildWithCtx _ BoxNew = "BoxNew"
  buildWithCtx _ ArrayToSliceShared = "ArrayToSliceShared"
  buildWithCtx _ ArrayToSliceMut = "ArrayToSliceMut"
  buildWithCtx _ ArrayRepeat = "ArrayRepeat"
  buildWithCtx _ctx (Index (BuiltinIndexOp isArray mutability isRange)) =
    let ty = if isArray then "Array" else "Slice"
        op = if isRange then "SubSlice" else "Index"
        mut = case mutability of
                RShared -> "Shared"
                RMut -> "Mut"
    in fromString (ty ++ op ++ mut)
  buildWithCtx _ (PtrFromParts RShared) = "PtrFromPartsShared"
  buildWithCtx _ (PtrFromParts RMut) = "PtrFromPartsMut"

-- Add more instances as needed...

-- Placeholder instances for complex types (to be fully implemented later)
instance BuildWithCtx LiteralType where
  buildWithCtx _ lt = fromString (show lt)

instance BuildWithCtx Ty where
  buildWithCtx _ ty = fromString (show ty)

instance BuildWithCtx Operand where
  buildWithCtx _ op = fromString (show op)

instance BuildWithCtx Place where
  buildWithCtx _ pl = fromString (show pl)

instance BuildWithCtx FnOperand where
  buildWithCtx _ fn = fromString (show fn)

