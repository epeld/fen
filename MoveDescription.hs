module MoveDescription where
import Prelude ()
import Data.Function
import Data.Bool

import MoveType
import Square

class MoveDescription d where
    destination :: d -> Square
    moveType :: d -> MoveType
    possibleSource :: d -> Square -> Bool
    possibleSource _ _ = True

instance MoveDescription desc => MoveDescription (Move desc) where
    destination = destination. description
    moveType = moveType. description
