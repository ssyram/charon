module Generated_UllbcAst where

import qualified Generated_Meta as M
import Data.Aeson (FromJSON)

newtype BlockId = BlockId { blockidRaw :: Int }
instance Show BlockId
instance Eq BlockId
instance Ord BlockId
instance FromJSON BlockId

data Block
instance Show Block
instance Eq Block
instance Ord Block
instance FromJSON Block

data Statement
instance Show Statement
instance Eq Statement
instance Ord Statement
instance FromJSON Statement

data SwitchTargets
instance Show SwitchTargets
instance Eq SwitchTargets
instance Ord SwitchTargets
instance FromJSON SwitchTargets
