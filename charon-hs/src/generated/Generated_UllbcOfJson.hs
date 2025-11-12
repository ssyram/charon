{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_UllbcOfJson.hs`
by hand. Edit `templates/UllbcOfJson.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_UllbcOfJson where

import Data.Aeson
import Data.Text (Text)
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as H
import Generated_Meta
import Generated_Values
import Generated_Types
import Generated_Expressions
import Generated_GAst
import Generated_UllbcAst

instance FromJSON Block where
  parseJSON = withObject "Block" $ \o -> do
  statements <- o .: "statements"
    terminator <- o .: "terminator"
  pure Block { statements, terminator }


instance FromJSON BlockId where
  parseJSON = withObject "BlockId" $ \o -> do
  raw <- o .: "_raw"
  pure BlockId { raw }


instance FromJSON Blocks where
  parseJSON = parseJSON @[BlockId]


instance FromJSON Statement where
  parseJSON = withObject "Statement" $ \o -> do
  span <- o .: "span"
    kind <- o .: "kind"
    commentsBefore <- o .: "comments_before"
  pure Statement { span, kind, commentsBefore }


instance FromJSON StatementKind where
  parseJSON v = case v of
  Object o | H.lookup "Assign" o /= Nothing -> do
  arr <- o .: "Assign"
  withArray "Assign" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (Assign v0 v1)) arr

    Object o | H.lookup "SetDiscriminant" o /= Nothing -> do
  arr <- o .: "SetDiscriminant"
  withArray "SetDiscriminant" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (SetDiscriminant v0 v1)) arr

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
  arr <- o .: "Drop"
  withArray "Drop" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (Drop v0 v1)) arr

    Object o | H.lookup "Assert" o /= Nothing -> do
  v <- o .: "Assert"
  Assert <$> parseJSON v

    String "Nop" -> pure Nop
  _ -> fail "Unknown variant"


instance FromJSON Switch where
  parseJSON v = case v of
  Object o | H.lookup "If" o /= Nothing -> do
  arr <- o .: "If"
  withArray "If" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (If v0 v1)) arr

    Object o | H.lookup "SwitchInt" o /= Nothing -> do
  arr <- o .: "SwitchInt"
  withArray "SwitchInt" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (SwitchInt v0 v1 v2)) arr

  _ -> fail "Unknown variant"


instance FromJSON Terminator where
  parseJSON = withObject "Terminator" $ \o -> do
  span <- o .: "span"
    kind <- o .: "kind"
    commentsBefore <- o .: "comments_before"
  pure Terminator { span, kind, commentsBefore }


instance FromJSON TerminatorKind where
  parseJSON v = case v of
  Object o | H.lookup "Goto" o /= Nothing -> do
  v <- o .: "Goto"
  Goto <$> parseJSON v

    Object o | H.lookup "Switch" o /= Nothing -> do
  arr <- o .: "Switch"
  withArray "Switch" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (Switch v0 v1)) arr

    Object o | H.lookup "Call" o /= Nothing -> do
  arr <- o .: "Call"
  withArray "Call" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (Call v0 v1 v2)) arr

    Object o | H.lookup "Abort" o /= Nothing -> do
  v <- o .: "Abort"
  Abort <$> parseJSON v

    String "Return" -> pure Return
    String "UnwindResume" -> pure UnwindResume
  _ -> fail "Unknown variant"

