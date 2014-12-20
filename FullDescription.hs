module FullDescription where
import Prelude (undefined)
import Data.Eq
import Text.Show

import MoveDescription
import qualified PartialDescription as Partial
import Square
import MoveType
import PositionReader

data MoveError = Ambiguous [FullMove] | Invalid

data Description = Description {
        destination :: Square, 
        moveType :: MoveType
    } deriving (Show)

instance MoveDescription Description where
    destination = FullDescription.destination
    moveType = FullDescription.moveType

newtype FullMove = Move Description

specify :: Partial.PartialMove -> PReader [FullMove]
specify = undefined
