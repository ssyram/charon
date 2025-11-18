{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_UllbcAst.hs`
by hand. Edit `templates/UllbcAst.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_UllbcAst where

import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Generated_Meta
import Generated_Values
import Generated_Types
import Generated_Expressions
import Generated_GAst

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
  | CopyNonOverlapping CopyNonOverlapping
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
  | Call Call BlockId BlockId
  | Abort AbortKind
  | Return
  | UnwindResume
  deriving (Show, Eq, Ord)
