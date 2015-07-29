module UpdateFunctions where
import Control.Monad
import Control.Monad.Reader

import ListUtils
import Square
import Piece
import MoveType
import Position
import FullMove

-- Represents the information needed to create a new position from an old
-- e.g we need to know how the position looked initially, and we need to know what is going to change
data UpdateEnvironment = UpdateEnvironment { move :: FullMove, originalPosition :: Position }

type UpdateReader = Reader UpdateEnvironment

-- We will create a new position from an old one by generating lots of small update functions,
-- each updating a single property of the position, and then composing them
type UpdateFn = Position -> Position

originalPositionR  :: UpdateReader Position
originalPositionR = asks originalPosition

moveR :: UpdateReader FullMove
moveR = asks move

whoseMoveR :: UpdateReader Color
whoseMoveR = Position._turn `liftM` originalPositionR
