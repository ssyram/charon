{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_LlbcAst.hs`
by hand. Edit `templates/LlbcAst.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_LlbcAst where

import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Generated_Meta
import Generated_Values
import Generated_Types
import Generated_Expressions
import Generated_GAst

data Block = Block
  { blockSpan :: Span
  , blockStatements :: [Statement]
  }
  deriving (Show, Eq, Ord)

data Statement = Statement
  { statementSpan :: Span
  ,   -- | Integer uniquely identifying this statement among the statmeents in the current body. To
  -- | simplify things we generate globally-fresh ids when creating a new `Statement`.
  statementStatementId :: StatementId
  , statementKind :: StatementKind
  ,   -- | Comments that precede this statement.
  statementCommentsBefore :: [String]
  }
  deriving (Show, Eq, Ord)

data StatementId = StatementId
  { statementidRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | A raw statement: a statement without meta data.
data StatementKind = Assign Place Rvalue
  | SetDiscriminant Place VariantId
  | CopyNonOverlapping CopyNonOverlapping
  | StorageLive LocalId
  | StorageDead LocalId
  | Deinit Place
  | Drop Place TraitRef
  | Assert Assertion
  | Call Call
  | Abort AbortKind
  | Return
  | Break Int
  | Continue Int
  | Nop
  | Switch Switch
  | Loop Block
  | Error String
  deriving (Show, Eq, Ord)

data Switch = If Operand Block Block
  | SwitchInt Operand LiteralType ([([Literal], Block)]) Block
  | Match Place ([([VariantId], Block)]) (Maybe Block)
  deriving (Show, Eq, Ord)
