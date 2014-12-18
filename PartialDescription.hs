module PartialDescription where
import Prelude ()
import Data.Eq
import Text.Show

import MoveDescription
import Square
import MoveType

data Description = Description {
        destination :: Square, 
        moveType :: MoveType
    } deriving (Show)

instance MoveDescription Description where
    destination = PartialDescription.destination
    moveType = PartialDescription.moveType
