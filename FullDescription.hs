module FullDescription where
import Prelude ()
import Data.Eq
import Text.Show

import MoveDescription
import Square
import MoveType

data MoveError = Ambiguous [FullMove] | Invalid

data Description = Description {
        destination :: Square, 
        moveType :: MoveType
    } deriving (Show)

instance MoveDescription Description where
    destination = FullDescription.destination
    moveType = FullDescription.moveType

newtype FullMove = Move Description
