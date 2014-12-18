module Partial where
import Prelude ()
import MoveDescription
import MoveType
import Square

data Description = Description {
        destination :: Square, 
        moveType :: MoveType,
    } deriving (MoveDescription, Eq, Show)

