module MoveDescription where
import Prelude ()
import MoveType
import Square

class MoveDescription d where
    destination :: d -> Square
    moveType :: d -> MoveType
