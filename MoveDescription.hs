module MoveDescription where
import Prelude ()
import Data.Function

import MoveType
import Square

class MoveDescription d where
    destination :: d -> Square
    moveType :: d -> MoveType

instance MoveDescription desc => MoveDescription (Move desc) where
    destination = destination. description
    moveType = moveType. description
