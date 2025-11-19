{-# LANGUAGE OverloadedStrings #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_UllbcAst.hs`
by hand. Edit `templates/UllbcAst.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_UllbcAst where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import Generated_Meta
import Generated_Values
import Generated_Types
import Generated_Expressions
import qualified Generated_GAst as G
-- Import everything from GAst except the data constructors that conflict with our variant constructors
import Generated_GAst hiding (Call, CopyNonOverlapping)

data Block = Block
  { blockStatements :: [Statement]
  , blockTerminator :: Terminator
  }
  deriving (Show, Eq, Ord)

data BlockId = BlockId
  { blockidRaw :: Int
  }
  deriving (Show, Eq, Ord)

type Blocks = (Vector BlockId Block)

data Statement = Statement
  { statementSpan :: Span
  , statementKind :: StatementKind
  ,   -- | Comments that precede this statement.
  statementCommentsBefore :: [String]
  }
  deriving (Show, Eq, Ord)

-- | A raw statement: a statement without meta data.
data StatementKind = Assign Place Rvalue
  | SetDiscriminant Place VariantId
  | CopyNonOverlapping G.CopyNonOverlapping
  | StorageLive LocalId
  | StorageDead LocalId
  | Deinit Place
  | Drop Place TraitRef
  | Assert Assertion
  | Nop
  deriving (Show, Eq, Ord)

data Switch = If BlockId BlockId
  | SwitchInt LiteralType ([(Literal, BlockId)]) BlockId
  deriving (Show, Eq, Ord)

data Terminator = Terminator
  { terminatorSpan :: Span
  , terminatorKind :: TerminatorKind
  ,   -- | Comments that precede this terminator.
  terminatorCommentsBefore :: [String]
  }
  deriving (Show, Eq, Ord)

-- | A raw terminator: a terminator without meta data.
data TerminatorKind = Goto BlockId
  | Switch Operand Switch
  | Call G.Call BlockId BlockId
  | Abort AbortKind
  | Return
  | UnwindResume
  deriving (Show, Eq, Ord)

instance FromJSON Block where
  parseJSON = withObject "Block" $ \o -> do
    blockStatements <- o .: "statements"
    blockTerminator <- o .: "terminator"
    pure (Block blockStatements blockTerminator)


instance FromJSON BlockId where
  parseJSON = fmap BlockId . parseJSON




instance FromJSON Statement where
  parseJSON = withObject "Statement" $ \o -> do
    statementSpan <- o .: "span"
    statementKind <- o .: "kind"
    statementCommentsBefore <- o .: "comments_before"
    pure (Statement statementSpan statementKind statementCommentsBefore)


instance FromJSON StatementKind where
  parseJSON v = case v of
    Object o | H.lookup "Assign" o /= Nothing -> do
      withArray "Assign" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (Assign v0 v1)) =<< o .: "Assign"
    Object o | H.lookup "SetDiscriminant" o /= Nothing -> do
      withArray "SetDiscriminant" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (SetDiscriminant v0 v1)) =<< o .: "SetDiscriminant"
    Object o | H.lookup "CopyNonOverlapping" o /= Nothing -> do
      v <- o .: "CopyNonOverlapping"
      CopyNonOverlapping <$> parseJSON v
    Object o | H.lookup "StorageLive" o /= Nothing -> do
      v <- o .: "StorageLive"
      StorageLive <$> parseJSON v
    Object o | H.lookup "StorageDead" o /= Nothing -> do
      v <- o .: "StorageDead"
      StorageDead <$> parseJSON v
    Object o | H.lookup "Deinit" o /= Nothing -> do
      v <- o .: "Deinit"
      Deinit <$> parseJSON v
    Object o | H.lookup "Drop" o /= Nothing -> do
      withArray "Drop" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (Drop v0 v1)) =<< o .: "Drop"
    Object o | H.lookup "Assert" o /= Nothing -> do
      v <- o .: "Assert"
      Assert <$> parseJSON v
    String "Nop" -> pure Nop
    _ -> fail "Unknown variant"


instance FromJSON Switch where
  parseJSON v = case v of
    Object o | H.lookup "If" o /= Nothing -> do
      withArray "If" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (If v0 v1)) =<< o .: "If"
    Object o | H.lookup "SwitchInt" o /= Nothing -> do
      withArray "SwitchInt" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (SwitchInt v0 v1 v2)) =<< o .: "SwitchInt"
    _ -> fail "Unknown variant"


instance FromJSON Terminator where
  parseJSON = withObject "Terminator" $ \o -> do
    terminatorSpan <- o .: "span"
    terminatorKind <- o .: "kind"
    terminatorCommentsBefore <- o .: "comments_before"
    pure (Terminator terminatorSpan terminatorKind terminatorCommentsBefore)


instance FromJSON TerminatorKind where
  parseJSON v = case v of
    Object o | H.lookup "Goto" o /= Nothing -> do
      obj <- o .: "Goto"
      target <- obj .: "target"
      pure (Goto target)
    Object o | H.lookup "Switch" o /= Nothing -> do
      obj <- o .: "Switch"
      discr <- obj .: "discr"
      targets <- obj .: "targets"
      pure (Switch discr targets)
    Object o | H.lookup "Call" o /= Nothing -> do
      obj <- o .: "Call"
      call <- obj .: "call"
      target <- obj .: "target"
      onUnwind <- obj .: "on_unwind"
      pure (Call call target onUnwind)
    Object o | H.lookup "Abort" o /= Nothing -> do
      v <- o .: "Abort"
      Abort <$> parseJSON v
    String "Return" -> pure Return
    String "UnwindResume" -> pure UnwindResume
    _ -> fail "Unknown variant"

