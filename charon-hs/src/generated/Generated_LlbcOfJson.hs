{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_LlbcOfJson.hs`
by hand. Edit `templates/LlbcOfJson.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_LlbcOfJson where

import Data.Aeson hiding (Error)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import Generated_Meta
import Generated_Values
import Generated_Types
import Generated_Expressions
import qualified Generated_GAst as G
import Generated_GAstOfJson ()  -- Import FromJSON instances
import Generated_LlbcAst

instance FromJSON Block where
  parseJSON = withObject "Block" $ \o -> do
    blockSpan <- o .: "span"
    blockStatements <- o .: "statements"
    pure (Block blockSpan blockStatements)


instance FromJSON Statement where
  parseJSON = withObject "Statement" $ \o -> do
    statementSpan <- o .: "span"
    statementStatementId <- o .: "id"
    statementKind <- o .: "kind"
    statementCommentsBefore <- o .: "comments_before"
    pure (Statement statementSpan statementStatementId statementKind statementCommentsBefore)


instance FromJSON StatementId where
  parseJSON = fmap StatementId . parseJSON


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
    Object o | H.lookup "Call" o /= Nothing -> do
      v <- o .: "Call"
      Call <$> parseJSON v
    Object o | H.lookup "Abort" o /= Nothing -> do
      v <- o .: "Abort"
      Abort <$> parseJSON v
    String "Return" -> pure Return
    Object o | H.lookup "Break" o /= Nothing -> do
      v <- o .: "Break"
      Break <$> parseJSON v
    Object o | H.lookup "Continue" o /= Nothing -> do
      v <- o .: "Continue"
      Continue <$> parseJSON v
    String "Nop" -> pure Nop
    Object o | H.lookup "Switch" o /= Nothing -> do
      v <- o .: "Switch"
      Switch <$> parseJSON v
    Object o | H.lookup "Loop" o /= Nothing -> do
      v <- o .: "Loop"
      Loop <$> parseJSON v
    Object o | H.lookup "Error" o /= Nothing -> do
      v <- o .: "Error"
      Error <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON Switch where
  parseJSON v = case v of
    Object o | H.lookup "If" o /= Nothing -> do
      withArray "If" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (If v0 v1 v2)) =<< o .: "If"
    Object o | H.lookup "SwitchInt" o /= Nothing -> do
      withArray "SwitchInt" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        v3 <- parseJSON (v V.! 3)
        pure (SwitchInt v0 v1 v2 v3)) =<< o .: "SwitchInt"
    Object o | H.lookup "Match" o /= Nothing -> do
      withArray "Match" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (Match v0 v1 v2)) =<< o .: "Match"
    _ -> fail "Unknown variant"

